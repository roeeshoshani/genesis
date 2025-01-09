use core::{
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Deref, DerefMut},
};

use arrayvec::ArrayVec;
use bitpiece::*;
use hal::mmio::gt64120::Gt64120Regs;
use paste::paste;

use crate::{interrupts::with_interrupts_disabled, println, utils::max_val_of_bit_len};

/// the maximum amount of BARs that a single function may have.
const PCI_MAX_BARS: usize = 6;

pub type PciRegNum = B6;
pub type PciFunctionNum = B3;
pub type PciDevNum = B5;
pub type PciBusNum = u8;

#[bitpiece(32)]
#[derive(Debug)]
pub struct PciConfigAddr {
    pub zero: B2,
    pub reg_num: PciRegNum,
    pub function_num: PciFunctionNum,
    pub dev_num: PciDevNum,
    pub bus_num: PciBusNum,
    pub reserved: B7,
    pub enabled: bool,
}

pub fn pci_config_read(addr: PciConfigAddr) -> u32 {
    // do this with interrupts disabled to prevent an interrupt handler from overwriting the pci config register while
    // we are operating
    with_interrupts_disabled! {
        Gt64120Regs::pci_0_config_addr().write(addr.to_bits());
        Gt64120Regs::pci_0_config_data().read()
    }
}
pub fn pci_config_write(addr: PciConfigAddr, value: u32) {
    // do this with interrupts disabled to prevent an interrupt handler from overwriting the pci config register while
    // we are operating
    with_interrupts_disabled! {
        Gt64120Regs::pci_0_config_addr().write(addr.to_bits());
        Gt64120Regs::pci_0_config_data().write(value);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciBus {
    addr: PciConfigAddr,
}
impl PciBus {
    pub fn new(bus_num: u8) -> Self {
        let mut addr = PciConfigAddr::zeroes();
        addr.set_enabled(true);
        addr.set_bus_num(bus_num);
        Self { addr }
    }
    pub fn dev(self, dev_num: u8) -> Option<PciDev> {
        let mut dev_addr = self.addr;
        dev_addr.set_dev_num(PciDevNum::new(dev_num).unwrap());
        let dev = PciDev { addr: dev_addr };

        if dev.exists() {
            Some(dev)
        } else {
            None
        }
    }
    pub fn bus_num(&self) -> u8 {
        self.addr.bus_num()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciDev {
    addr: PciConfigAddr,
}
impl PciDev {
    pub fn function(self, function_num: u8) -> Option<PciFunction> {
        let mut function_addr = self.addr;
        function_addr.set_function_num(PciFunctionNum::new(function_num).unwrap());

        let function = PciFunction {
            addr: function_addr,
        };

        if function.exists() {
            Some(function)
        } else {
            None
        }
    }

    /// checks if this pci device even exists.
    fn exists(self) -> bool {
        self.function(0).is_some()
    }

    /// returns the first function of this device. all devices must have this function.
    pub fn function0(self) -> PciFunction {
        self.function(0).unwrap()
    }

    pub fn bus_num(&self) -> u8 {
        self.addr.bus_num()
    }
    pub fn dev_num(&self) -> u8 {
        self.addr.dev_num().get()
    }
}

macro_rules! pci_function_define_bitfield_reg {
    ($num: literal) => {
        paste! {
            pub fn [<read_config_reg $num>](self) -> [<PciConfigReg $num>] {
                [<PciConfigReg $num>]::from_bits(self.config_reg($num).read())
            }
            pub fn [<write_config_reg $num>](self, value: [<PciConfigReg $num>]) {
                self.config_reg($num).write(value.to_bits())
            }
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciId {
    pub vendor_id: u16,
    pub device_id: u16,
}
impl PciId {
    pub const PIIX4_CORE: Self = Self {
        vendor_id: 0x8086,
        device_id: 0x7110,
    };
    pub const PIIX4_IDE_CONTROLLER: Self = Self {
        vendor_id: 0x8086,
        device_id: 0x7111,
    };
    pub const PIIX4_USB_CONTROLLER: Self = Self {
        vendor_id: 0x8086,
        device_id: 0x7112,
    };
    pub const PIIX4_POWER_MANAGEMENT: Self = Self {
        vendor_id: 0x8086,
        device_id: 0x7113,
    };
    pub const GT64120: Self = Self {
        vendor_id: 0x11ab,
        device_id: 0x4620,
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciFunction {
    addr: PciConfigAddr,
}
impl PciFunction {
    pub fn id(self) -> PciId {
        PciId {
            vendor_id: self.vendor_id(),
            device_id: self.device_id(),
        }
    }

    pub fn bars(self) -> ArrayVec<PciBarReg, PCI_MAX_BARS> {
        // first decide which range of registers contains the BAR registers
        let bar_regs_range = match self.header_type().kind() {
            PciHeaderKind::General => 4..10,
            PciHeaderKind::PciToPciBridge => 4..6,
            PciHeaderKind::PciToCardBusBridge | PciHeaderKind::Unknown => {
                // no BARs
                return ArrayVec::new();
            }
        };

        bar_regs_range
            .map(|reg_num| PciBarReg::new(PciConfigRegTyped::new(self.config_reg(reg_num))))
            .collect()
    }

    pub fn config_reg(self, reg_num: u8) -> PciConfigReg {
        let mut reg_addr = self.addr;
        reg_addr.set_reg_num(PciRegNum::new(reg_num).unwrap());

        PciConfigReg { addr: reg_addr }
    }

    pub fn config_reg0(self) -> PciConfigRegTyped<PciConfigReg0> {
        PciConfigRegTyped::new(self.config_reg(0))
    }
    pub fn config_reg1(self) -> PciConfigRegTyped<PciConfigReg1> {
        PciConfigRegTyped::new(self.config_reg(1))
    }
    pub fn config_reg2(self) -> PciConfigRegTyped<PciConfigReg2> {
        PciConfigRegTyped::new(self.config_reg(2))
    }
    pub fn config_reg3(self) -> PciConfigRegTyped<PciConfigReg3> {
        PciConfigRegTyped::new(self.config_reg(3))
    }

    /// checks if this pci function even exists.
    fn exists(self) -> bool {
        self.vendor_id() != 0xffff
    }

    pub fn vendor_id(self) -> u16 {
        self.config_reg0().read().vendor_id()
    }

    pub fn device_id(self) -> u16 {
        self.config_reg0().read().device_id()
    }

    pub fn class_code(self) -> u8 {
        self.config_reg2().read().class_code()
    }

    pub fn subclass(self) -> u8 {
        self.config_reg2().read().subclass()
    }

    pub fn header_type(self) -> PciHeaderType {
        self.config_reg3().read().header_type()
    }

    pub fn bus_num(&self) -> u8 {
        self.addr.bus_num()
    }
    pub fn dev_num(&self) -> u8 {
        self.addr.dev_num().get()
    }
    pub fn function_num(&self) -> u8 {
        self.addr.function_num().get()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciBarReg {
    reg: PciConfigRegTyped<PciBarRaw>,
    kind: PciBarKind,
}
impl PciBarReg {
    pub fn new(reg: PciConfigRegTyped<PciBarRaw>) -> Self {
        Self {
            reg,
            kind: reg.read().kind(),
        }
    }
    pub fn kind(self) -> PciBarKind {
        self.kind
    }
    fn mem_bar_reg(self) -> PciConfigRegTyped<PciMemBar> {
        self.reg.cast()
    }
    fn io_bar_reg(self) -> PciConfigRegTyped<PciIoBar> {
        self.reg.cast()
    }
    pub fn address(self) -> u32 {
        match self.kind {
            PciBarKind::Mem => self.mem_bar_reg().read().address().get(),
            PciBarKind::Io => self.io_bar_reg().read().address().get(),
        }
    }
    /// get the BAR's address value by only masking out the other fields of the BAR, without shifting.
    pub fn address_noshift(self) -> u32 {
        match self.kind {
            PciBarKind::Mem => self.mem_bar_reg().read().address_noshift(),
            PciBarKind::Io => self.io_bar_reg().read().address_noshift(),
        }
    }
    pub fn set_address(self, new_address: u32) {
        let bits = self.reg.read().to_bits();
        let modified_bits = match self.kind {
            PciBarKind::Mem => {
                let mut modified_value = PciMemBar::from_bits(bits);
                modified_value.set_address(BitPiece::from_bits(new_address));
                modified_value.to_bits()
            }
            PciBarKind::Io => {
                let mut modified_value = PciIoBar::from_bits(bits);
                modified_value.set_address(BitPiece::from_bits(new_address));
                modified_value.to_bits()
            }
        };
        self.reg.write(PciBarRaw::from_bits(modified_bits));
    }
    /// returns the size of the BAR, or `None` if the bar is not implemented (has a size of 0).
    pub fn size(self) -> Option<NonZeroU32> {
        // TODO: what if someone modified the BAR while we are doing this?

        // save the original value before modifying the BAR
        let orig = self.reg.read();

        // write a value of all 1 bits to the BAR. this will make the next read to the BAR's address return
        // the BAR's size.
        self.reg.write(BitPiece::ones());

        // read the BAR's value after writing the 1 bits, and decode it to get the size.
        //
        // to get the size, we must first mask out the rest of the BAR's fields, and only keep the BAR's address.
        //
        // then, what we are left with is a bitwise not of the mask of bits that are needed to represent
        // a relative offset inside the BAR's address space. we can use that to calculate the BAR's address
        // space size.
        //
        // for example, for a BAR of size 16 bytes, the mask of bits needed to represent a relative
        // offset inside the BAR is `0b1111` or `0xf`. so, the value returned from reading the BAR
        // and masking all fields except the address will be `!0xf` which is `0xfffffff0`.
        //
        // to calculate the size of the BAR, we can bitwise not the returned value to get the original mask,
        // and then add 1 to it to get the size.
        let size = !self.address_noshift() + 1;

        // write back the original value
        self.reg.write(orig);

        // return the size of the BAR
        NonZeroU32::new(size)
    }
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciBarRaw {
    pub kind: PciBarKind,
    pub data: B31,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciIoBar {
    pub kind: PciBarKind,
    pub address: B31,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciMemBar {
    pub kind: PciBarKind,
    pub locatable: PciBarLocatable,
    pub is_prefetchable: bool,
    pub address: B28,
}

#[bitpiece(1)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PciBarKind {
    Mem = 0,
    Io = 1,
}

#[bitpiece(2)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PciBarLocatable {
    Any32Bit = 0,
    Below1Mb = 1,
    Any64Bit = 2,
    Reserved = 3,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciConfigReg0 {
    pub vendor_id: u16,
    pub device_id: u16,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciConfigReg1 {
    pub command: u16,
    pub status: u16,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciConfigReg2 {
    pub revision_id: u8,
    pub prog_if: u8,
    pub subclass: u8,
    pub class_code: u8,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciConfigReg3 {
    pub cache_line_size: u8,
    pub latency_timer: u8,
    pub header_type: PciHeaderType,
    pub bist: u8,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciToPciBridgeConfigReg6 {
    pub primary_bus_num: u8,
    pub secondary_bus_num: u8,
    pub subordinate_bus_num: u8,
    pub secondary_latency_timer: u8,
}

#[bitpiece(8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciHeaderType {
    pub raw_kind: B7,
    pub is_multi_function: bool,
}
impl PciHeaderType {
    pub fn kind(self) -> PciHeaderKind {
        match self.raw_kind().get() {
            0 => PciHeaderKind::General,
            1 => PciHeaderKind::PciToPciBridge,
            2 => PciHeaderKind::PciToCardBusBridge,
            _ => PciHeaderKind::Unknown,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PciHeaderKind {
    General = 0,
    PciToPciBridge = 1,
    PciToCardBusBridge = 2,
    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciConfigReg {
    addr: PciConfigAddr,
}
impl PciConfigReg {
    pub fn read(self) -> u32 {
        pci_config_read(self.addr)
    }
    pub fn write(self, value: u32) {
        pci_config_write(self.addr, value)
    }

    pub fn bus_num(&self) -> u8 {
        self.addr.bus_num()
    }
    pub fn dev_num(&self) -> u8 {
        self.addr.dev_num().get()
    }
    pub fn function_num(&self) -> u8 {
        self.addr.function_num().get()
    }
    pub fn reg_num(&self) -> u8 {
        self.addr.reg_num().get()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciConfigRegTyped<T> {
    reg: PciConfigReg,
    phantom: PhantomData<T>,
}
impl<T: BitPiece<Bits = u32>> PciConfigRegTyped<T> {
    pub fn new(reg: PciConfigReg) -> Self {
        Self {
            reg,
            phantom: PhantomData,
        }
    }
    pub fn read(self) -> T {
        T::from_bits(self.reg.read())
    }
    pub fn write(self, value: T) {
        self.reg.write(value.to_bits());
    }
    pub fn reg(self) -> PciConfigReg {
        self.reg
    }
    pub fn cast<O: BitPiece<Bits = u32>>(self) -> PciConfigRegTyped<O> {
        PciConfigRegTyped::new(self.reg)
    }
}

pub struct PciClassCode;
impl PciClassCode {
    pub const NO_CLASS_CODE: u8 = 0;
    pub const MASS_STORAGE: u8 = 1;
    pub const NETWORK: u8 = 2;
    pub const DISPLAY: u8 = 3;
    pub const MULTIMEDIA: u8 = 4;
    pub const MEMORY_CONTROLLER: u8 = 5;
    pub const BRIDGE: u8 = 6;
}

pub struct PciBridgeSubclass;
impl PciBridgeSubclass {
    pub const HOST_PCI_BRIDGE: u8 = 0;
    pub const PCI_ISA_BRIDGE: u8 = 1;
    pub const PCI_EISA_BRIDGE: u8 = 2;
    pub const PCI_MICRO_CHANNEL_BRIDGE: u8 = 3;
    pub const PCI_PCI_BRIDGE: u8 = 4;
}

struct PciScanner<F: FnMut(PciFunction)> {
    callback: F,
}
impl<F: FnMut(PciFunction)> PciScanner<F> {
    fn new(callback: F) -> Self {
        Self { callback }
    }

    fn scan(&mut self) {
        self.scan_bus(PciBus::new(0));
    }

    fn scan_bus(&mut self, bus: PciBus) {
        for i in 0..PciDevNum::MAX.get() {
            if let Some(dev) = bus.dev(i) {
                self.scan_dev(dev);
            }
        }
    }

    fn scan_dev(&mut self, dev: PciDev) {
        let function0 = dev.function0();

        self.scan_function(function0);

        if function0.header_type().is_multi_function() {
            for i in 1..PciFunctionNum::MAX.get() {
                if let Some(function) = dev.function(i) {
                    self.scan_function(function);
                }
            }
        }
    }

    fn scan_function(&mut self, function: PciFunction) {
        (self.callback)(function);

        if function.class_code() == PciClassCode::BRIDGE
            && function.subclass() == PciBridgeSubclass::PCI_PCI_BRIDGE
        {
            // verify that the structure of the header is that of a pci to pci bridge
            assert_eq!(function.header_type().kind(), PciHeaderKind::PciToPciBridge);

            // read the register which contains information about the downstream bus of the bridge
            let reg = PciToPciBridgeConfigReg6::from_bits(function.config_reg(6).read());

            // sanity - verify that the primary bus number matches the one we expect
            assert_eq!(reg.primary_bus_num(), function.bus_num());

            // scan the secondary bus
            self.scan_bus(PciBus::new(reg.secondary_bus_num()));
        }
    }
}

pub fn pci_scan<F: FnMut(PciFunction)>(callback: F) {
    let mut scanner = PciScanner::new(callback);
    scanner.scan();
}
