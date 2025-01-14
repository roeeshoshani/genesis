use core::{marker::PhantomData, ops::Range, sync::atomic::AtomicUsize};

use arrayvec::ArrayVec;
use bitpiece::*;
use hal::{
    mem::{PhysAddr, PhysMemRegion, PCI_0_IO, PCI_0_MEM},
    mmio::gt64120::Gt64120Regs,
};
use thiserror_no_std::Error;

use crate::{interrupts::with_interrupts_disabled, utils::HexDisplay};

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

/// a physical memory bump allocator, which allocates from the given physical memory region.
pub struct PhysMemBumpAllocator {
    cur_addr: AtomicUsize,
    end_addr: PhysAddr,
}
impl PhysMemBumpAllocator {
    pub const fn new(region: PhysMemRegion) -> Self {
        Self {
            cur_addr: AtomicUsize::new(region.start.0),
            end_addr: region.end,
        }
    }
    pub fn alloc(&self, size: usize) -> Result<PhysAddr, PhysMemBumpAllocatorError> {
        let allocated_addr = self
            .cur_addr
            .fetch_add(size, core::sync::atomic::Ordering::Relaxed);
        let end_addr = allocated_addr + size;
        if end_addr <= self.end_addr.0 {
            Ok(PhysAddr(allocated_addr))
        } else {
            Err(PhysMemBumpAllocatorError {
                space_requested: HexDisplay(size),
                space_left: HexDisplay(self.end_addr.0 - allocated_addr),
            })
        }
    }
}

/// an error while trying to allocate from the physical memory bump allocator.
#[derive(Debug, Error)]
#[error("requested allocation of {space_requested} bytes, but space left is only {space_left}")]
pub struct PhysMemBumpAllocatorError {
    pub space_requested: HexDisplay<usize>,
    pub space_left: HexDisplay<usize>,
}

static PCI_IO_SPACE_ALLOCATOR: PhysMemBumpAllocator = PhysMemBumpAllocator::new(PCI_0_IO);
static PCI_MEM_SPACE_ALLOCATOR: PhysMemBumpAllocator = PhysMemBumpAllocator::new(PCI_0_MEM);

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

    /// returns the range of configuration space register indexes which are BAR registers.
    fn bar_regs_range(self) -> Range<u8> {
        match self.header_type().kind() {
            PciHeaderKind::General => 4..10,
            PciHeaderKind::PciToPciBridge => 4..6,
            PciHeaderKind::PciToCardBusBridge | PciHeaderKind::Unknown => {
                // no BARs
                0..0
            }
        }
    }

    pub fn bar(self, index: u8) -> Option<PciBarReg> {
        let bar_regs_range = self.bar_regs_range();

        let reg_num = bar_regs_range.start + index;

        if !bar_regs_range.contains(&reg_num) {
            // the index is larger than the amount of BAR registers
            return None;
        }

        PciBarReg::new(PciConfigRegTyped::new(self.config_reg(reg_num)))
    }

    pub fn bars(self) -> impl Iterator<Item = PciBarReg> {
        self.bar_regs_range().filter_map(move |reg_num| {
            PciBarReg::new(PciConfigRegTyped::new(self.config_reg(reg_num)))
        })
    }

    pub fn bars_array(self) -> ArrayVec<PciBarReg, PCI_MAX_BARS> {
        self.bars().collect()
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
    pub fn map_all_bars_to_memory(&self) {
        for bar in self.bars() {
            bar.map_to_memory();
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PciBarReg {
    reg: PciConfigRegTyped<PciBarRaw>,
    kind: PciBarKind,
}
impl PciBarReg {
    /// creates a new pci BAR register from the given raw BAR register.
    /// if the BAR is not implemented by the pci device, returns `None`.
    pub fn new(reg: PciConfigRegTyped<PciBarRaw>) -> Option<Self> {
        let result = Self {
            reg,
            kind: reg.read().kind(),
        };

        // verify the fields of the BAR
        match result.kind {
            PciBarKind::Mem => match result.mem_bar_reg().read().locatable() {
                PciBarLocatable::Any32Bit => {
                    // ok
                }
                PciBarLocatable::Below1Mb => panic!("pci memory BARs below 1mb are not supported"),
                PciBarLocatable::Any64Bit => panic!("64-bit pci memory BARs are not supported"),
                PciBarLocatable::Reserved => panic!("invalid memory pci BAR locatable field value"),
            },
            PciBarKind::Io => {
                // ok
            }
        }

        // make sure that the bar is implemented by checking that it has a nonzero size
        if result.size() == 0 {
            // the bar is not implemented
            return None;
        }

        Some(result)
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
    fn address_noshift(self) -> u32 {
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
    /// returns the size of the BAR.
    pub fn size(self) -> u32 {
        // TODO: what if someone modified the BAR while we are doing this?

        // save the original value before modifying the BAR
        let orig = self.reg.read();

        // write a value of all 1 bits to the BAR. this will make the next read to the BAR return its size information.
        self.reg.write(BitPiece::ones());

        // read the value of the bar after writing the all 1 bits value, and only take the address bits.
        let addr_bits = self.address_noshift();

        // calculate the size according to the addr bits
        let mut size = (!addr_bits).wrapping_add(1);

        // special case for IO BARs: if the upper 16-bits of the size information returned 0, then the upper 16-bits of the size
        // should be ignores
        if self.kind == PciBarKind::Io && addr_bits >> 16 == 0 {
            size = size & 0xffff;
        }

        // write back the original value
        self.reg.write(orig);

        // return the size of the BAR
        size
    }
    pub fn is_mapped_to_memory(&self) -> bool {
        self.address() != 0
    }
    pub fn map_to_memory(&self) {
        // TODO: time of check time of use here. what if someone maps the BAR to memory between the time we check
        // and then time we actually map it?
        assert!(!self.is_mapped_to_memory());

        // allocate an address for the BAR and set its base address
        let allocator = match self.kind() {
            PciBarKind::Mem => &PCI_MEM_SPACE_ALLOCATOR,
            PciBarKind::Io => &PCI_IO_SPACE_ALLOCATOR,
        };
        let base_addr = allocator
            .alloc(self.size() as usize)
            .expect("failed to allocate address space for PCI BAR");
        self.set_address(base_addr.0 as u32);
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
    pub reserved: B1,
    pub address: B30,
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
    pub command: PciConfigRegCommand,
    pub status: PciConfigRegStatus,
}

#[bitpiece(16)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciConfigRegCommand {
    pub io_enable: bool,
    pub mem_enable: bool,
    pub bus_master_enable: bool,
    pub special_cycles_enable: bool,
    pub mem_write_and_invalidate_enable: bool,
    pub vga_palette_snoop_enable: bool,
    pub parity_error_response_enable: bool,
    pub reserved7: B1,
    pub serr_enable: bool,
    pub fast_back_to_back_enable: bool,
    pub are_interrupts_disabled: bool,
    pub reserved11: B5,
}

#[bitpiece(16)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciConfigRegStatus {
    pub reserved0: B3,
    pub is_interrupt_pending: bool,
    pub supports_new_capabilites: bool,
    pub supports_66mhz: bool,
    pub reserved6: B1,
    pub supports_fast_back_to_back: bool,
    pub master_data_parity_error: bool,
    pub devsel_timing: B2,
    pub signaled_target_abort: bool,
    pub received_target_abort: bool,
    pub received_master_abort: bool,
    pub signaled_system_error: bool,
    pub detected_parity_error: bool,
}

#[bitpiece(2)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PciDevselTiming {
    Fast = 0b00,
    Medium = 0b01,
    Slow = 0b10,
    Reserved = 0b11,
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
            todo!("pci to pci bridges are currently not supported");
        }
    }
}

pub fn pci_scan<F: FnMut(PciFunction)>(callback: F) {
    let mut scanner = PciScanner::new(callback);
    scanner.scan();
}

pub struct Piix4CorePciFunction {
    function: PciFunction,
}
impl Piix4CorePciFunction {
    pub fn new(function: PciFunction) -> Self {
        Self { function }
    }
    pub fn irqs_route(self) -> PciConfigRegTyped<Piix4IrqsRoute> {
        PciConfigRegTyped::new(self.function.config_reg(24))
    }
}

#[bitpiece(32)]
pub struct Piix4IrqsRoute {
    pub irq_a: Piix4IrqRoute,
    pub irq_b: Piix4IrqRoute,
    pub irq_c: Piix4IrqRoute,
    pub irq_d: Piix4IrqRoute,
}

#[bitpiece(8)]
pub struct Piix4IrqRoute {
    pub interrupt_routing: B4,
    pub reserved4: B3,
    pub disable_routing: bool,
}
