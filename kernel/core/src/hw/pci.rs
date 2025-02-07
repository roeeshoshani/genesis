use core::{
    marker::PhantomData,
    ops::{Deref, DerefMut, Range},
};

use alloc::{sync::Arc, vec::Vec};
use arrayvec::ArrayVec;
use bitpiece::*;
use hal::{
    mem::{PhysAddr, PhysMemRegion, PCI_0_IO, PCI_0_MEM},
    mmio::gt64120::Gt64120Regs,
};
use thiserror_no_std::Error;

use crate::{
    mem::align_up,
    sync::IrqLock,
    utils::{
        callback_chain::{CallbackChain, CallbackChainFn, CallbackChainNode},
        write_once::WriteOnce,
        HexDisplay,
    },
};

use super::interrupts::I8259IrqNum;

/// the maximum amount of BARs that a single function may have.
const PCI_MAX_BARS: usize = 6;

/// PCI has 4 irq lines: INTA, INTB, INTC and INTD.
pub const PCI_IRQ_LINES_AMOUNT: usize = 4;

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

/// a physical memory bump allocator for allocating BAR addresses from a given physical memory region.
struct PhysMemBarBumpAllocator {
    region: PhysMemRegion,
    cur_addr: usize,
}
impl PhysMemBarBumpAllocator {
    const fn new(region: PhysMemRegion, start_offset: usize) -> Self {
        Self {
            region,
            cur_addr: if start_offset == 0 {
                // if we have been requested to start allocating from the very start of the region, add 1 byte to the address.
                // this is done so that we don't allocate address 0, since that's an invalid BAR address.
                region.start.0 + 1
            } else {
                region.start.0 + start_offset
            },
        }
    }
    fn alloc(&mut self, size: usize) -> Result<PhysAddr, PhysMemBarBumpAllocatorError> {
        assert!(size > 0);

        // align the allocated address.
        // the alignment of a bar is the same as its size.
        let allocated_addr = align_up(self.cur_addr, size);

        let inclusive_end_addr = allocated_addr + (size - 1);

        if inclusive_end_addr <= self.region.inclusive_end.0 {
            // update the allocation cursor
            self.cur_addr = inclusive_end_addr + 1;

            // return the allocated address
            Ok(PhysAddr(allocated_addr))
        } else {
            // not enough space to allocate, return a corresponding error.
            //
            // note that the allocation may fail even if we seem to have enough space to allocate, since allocating also
            // requires alignment, which may require extra memory space.
            Err(PhysMemBarBumpAllocatorError {
                space_requested: HexDisplay(size),
                space_left: HexDisplay(self.region.inclusive_end.0 - allocated_addr + 1),
            })
        }
    }
}

/// an error while trying to allocate from the BAR physical memory bump allocator.
#[derive(Debug, Error)]
#[error("requested allocation of {space_requested} bytes, but space left is only {space_left}")]
struct PhysMemBarBumpAllocatorError {
    pub space_requested: HexDisplay<usize>,
    pub space_left: HexDisplay<usize>,
}

/// the size of the address range of the memory mapped piix4 io registers at the start of the PCI I/O 0 address space.
const PIIX4_IO_SPACE_SIZE: usize = 0x1000;

static PCI_IO_SPACE_ALLOCATOR: IrqLock<PhysMemBarBumpAllocator> =
    IrqLock::new(PhysMemBarBumpAllocator::new(PCI_0_IO, PIIX4_IO_SPACE_SIZE));
static PCI_MEM_SPACE_ALLOCATOR: IrqLock<PhysMemBarBumpAllocator> =
    IrqLock::new(PhysMemBarBumpAllocator::new(PCI_0_MEM, 0));

/// a lock for protecting access to pci configuration space.
///
/// this is required due to the way accessing pci configuration space is implemented.
/// you must first write the address to one register, and then you can read/write the data from another register.
/// and, without using a lock, someone might overwrite the address register after you wrote to it, thus causing your write
/// to write to another address than the one you intended.
static PCI_CONFIG_SPACE_LOCK: IrqLock<PciConfigSpace> = IrqLock::new(PciConfigSpace);

struct PciConfigSpace;
impl PciConfigSpace {
    fn read(&mut self, addr: PciConfigAddr) -> u32 {
        // the mutable reference to self guarantees exclusive access here, so no one can overwrite the address while we are
        // running here.
        Gt64120Regs::pci_0_config_addr().write(addr.to_bits());
        Gt64120Regs::pci_0_config_data().read()
    }
    fn write(&mut self, addr: PciConfigAddr, value: u32) {
        // the mutable reference to self guarantees exclusive access here, so no one can overwrite the address while we are
        // running here.
        Gt64120Regs::pci_0_config_addr().write(addr.to_bits());
        Gt64120Regs::pci_0_config_data().write(value);
    }
}

pub fn pci_config_read(addr: PciConfigAddr) -> u32 {
    let mut config_space = PCI_CONFIG_SPACE_LOCK.lock();
    config_space.read(addr)
}
pub fn pci_config_write(addr: PciConfigAddr, value: u32) {
    let mut config_space = PCI_CONFIG_SPACE_LOCK.lock();
    config_space.write(addr, value)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct PciBusRaw {
    addr: PciConfigAddr,
}
impl PciBusRaw {
    fn new(bus_num: u8) -> Self {
        let mut addr = PciConfigAddr::zeroes();
        addr.set_enabled(true);
        addr.set_bus_num(bus_num);
        Self { addr }
    }
    fn dev(&self, dev_num: u8) -> Option<PciDevRaw> {
        let mut dev_addr = self.addr;
        dev_addr.set_dev_num(PciDevNum::new(dev_num).unwrap());
        let dev = PciDevRaw { addr: dev_addr };

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
struct PciDevRaw {
    addr: PciConfigAddr,
}
impl PciDevRaw {
    fn function(&self, function_num: u8) -> Option<PciFunctionRaw> {
        let mut function_addr = self.addr;
        function_addr.set_function_num(PciFunctionNum::new(function_num).unwrap());

        let mut function = PciFunctionRaw {
            addr: function_addr,
        };

        if function.exists() {
            Some(function)
        } else {
            None
        }
    }

    /// checks if this pci device even exists.
    fn exists(&self) -> bool {
        self.function(0).is_some()
    }

    /// returns the first function of this device. all devices must have this function.
    fn function0(&self) -> PciFunctionRaw {
        self.function(0).unwrap()
    }

    pub fn bus_num(&self) -> u8 {
        self.addr.bus_num()
    }
    pub fn dev_num(&self) -> u8 {
        self.addr.dev_num().get()
    }
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
    pub const AM79C970: Self = Self {
        vendor_id: 0x1022,
        device_id: 0x2000,
    };
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PciFunctionRaw {
    addr: PciConfigAddr,
}
impl PciFunctionRaw {
    pub fn id(&mut self) -> PciId {
        PciId {
            vendor_id: self.vendor_id(),
            device_id: self.device_id(),
        }
    }

    /// returns the range of configuration space register indexes which are BAR registers.
    fn bar_regs_range(&mut self) -> Range<u8> {
        match self.header_type().kind() {
            PciHeaderKind::General => 4..10,
            PciHeaderKind::PciToPciBridge => 4..6,
            PciHeaderKind::PciToCardBusBridge | PciHeaderKind::Unknown => {
                // no BARs
                0..0
            }
        }
    }

    fn bars(&mut self) -> impl Iterator<Item = PciBar> {
        let mut pci_function = Self { addr: self.addr };
        self.bar_regs_range().filter_map(move |reg_num| {
            PciBar::new(PciConfigRegTyped::new(pci_function.config_reg(reg_num)))
        })
    }

    fn bars_array(&mut self) -> ArrayVec<PciBar, PCI_MAX_BARS> {
        self.bars().collect()
    }

    pub fn config_reg(&mut self, reg_num: u8) -> PciConfigReg {
        let mut reg_addr = self.addr;
        reg_addr.set_reg_num(PciRegNum::new(reg_num).unwrap());

        PciConfigReg { addr: reg_addr }
    }

    pub fn config_reg0(&mut self) -> PciConfigRegTyped<PciConfigReg0> {
        PciConfigRegTyped::new(self.config_reg(0))
    }
    pub fn config_reg1(&mut self) -> PciConfigRegTyped<PciConfigReg1> {
        PciConfigRegTyped::new(self.config_reg(1))
    }
    pub fn config_reg2(&mut self) -> PciConfigRegTyped<PciConfigReg2> {
        PciConfigRegTyped::new(self.config_reg(2))
    }
    pub fn config_reg3(&mut self) -> PciConfigRegTyped<PciConfigReg3> {
        PciConfigRegTyped::new(self.config_reg(3))
    }
    pub fn config_reg15(&mut self) -> PciConfigRegTyped<PciConfigReg15> {
        PciConfigRegTyped::new(self.config_reg(15))
    }

    /// checks if this pci function even exists.
    fn exists(&mut self) -> bool {
        self.vendor_id() != 0xffff
    }

    pub fn vendor_id(&mut self) -> u16 {
        self.config_reg0().read().vendor_id()
    }

    pub fn device_id(&mut self) -> u16 {
        self.config_reg0().read().device_id()
    }

    pub fn class_code(&mut self) -> u8 {
        self.config_reg2().read().class_code()
    }

    pub fn subclass(&mut self) -> u8 {
        self.config_reg2().read().subclass()
    }

    pub fn header_type(&mut self) -> PciHeaderType {
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

#[derive(Debug)]
pub struct PciBar {
    reg: PciConfigRegTyped<PciBarRaw>,
    kind: PciBarKind,
}
impl PciBar {
    /// creates a new pci BAR register from the given raw BAR register.
    /// if the BAR is not implemented by the pci device, returns `None`.
    pub fn new(reg: PciConfigRegTyped<PciBarRaw>) -> Option<Self> {
        let mut result = Self {
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
    pub fn kind(&self) -> PciBarKind {
        self.kind
    }
    fn mem_bar_reg(&self) -> PciConfigRegTyped<PciMemBar> {
        self.reg.cast()
    }
    fn io_bar_reg(&self) -> PciConfigRegTyped<PciIoBar> {
        self.reg.cast()
    }
    pub fn address(&self) -> u32 {
        match self.kind {
            PciBarKind::Mem => self.mem_bar_reg().read().address().get(),
            PciBarKind::Io => self.io_bar_reg().read().address().get(),
        }
    }
    /// get the BAR's address value by only masking out the other fields of the BAR, without shifting.
    fn address_noshift(&self) -> u32 {
        match self.kind {
            PciBarKind::Mem => self.mem_bar_reg().read().address_noshift(),
            PciBarKind::Io => self.io_bar_reg().read().address_noshift(),
        }
    }
    pub fn set_address(&mut self, new_address: u32) {
        let bits = self.reg.read().to_bits();
        let modified_bits = match self.kind {
            PciBarKind::Mem => {
                let mut modified_value = PciMemBar::from_bits(bits);
                let desired_addr_value = PciMemBar::from_bits(new_address);
                modified_value.set_address(desired_addr_value.address());
                modified_value.to_bits()
            }
            PciBarKind::Io => {
                let mut modified_value = PciIoBar::from_bits(bits);
                let desired_addr_value = PciIoBar::from_bits(new_address);
                modified_value.set_address(desired_addr_value.address());
                modified_value.to_bits()
            }
        };
        self.reg.write(PciBarRaw::from_bits(modified_bits));
    }
    /// returns the size of the BAR.
    pub fn size(&mut self) -> u32 {
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

    pub fn map_to_memory(&mut self) -> MappedBar {
        // TODO: time of check time of use here. what if someone maps the BAR to memory between the time we check
        // and then time we actually map it?
        assert!(!self.is_mapped_to_memory());

        // choose the correct allocator for allocating mmio for the BAR
        let mut allocator = match self.kind() {
            PciBarKind::Mem => PCI_MEM_SPACE_ALLOCATOR.lock(),
            PciBarKind::Io => PCI_IO_SPACE_ALLOCATOR.lock(),
        };

        let size = self.size() as usize;

        let base_addr = allocator
            .alloc(size)
            .expect("failed to allocate address space for PCI BAR");

        // set the address of the bar to the offset from the start of the memory region.
        let offset = base_addr.0 - allocator.region.start.0;
        self.set_address(offset as u32);

        MappedBar {
            addr: base_addr,
            size,
        }
    }
}

pub struct MappedBar {
    pub addr: PhysAddr,
    pub size: usize,
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
pub struct PciConfigReg15 {
    /// the interrupt line that the device is connected to. this field is for software use only and is ignored by device.
    /// it is used to store information about the actual irq line that the device is connected to, which depends on the interrupt
    /// pin used by the device, and on the interrupt routing between pci irq numbers to actual irq numbers.
    pub interrupt_line: u8,

    /// the pci interrupt pin that the device uses. specifies which pci irq line this device uses - INTA, INTB, INTC or INTD.
    pub interrupt_pin: PciInterruptPin,

    /// this field's interpretation depends on the header type.
    pub unknown: u16,
}

#[bitpiece(8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciInterruptPin {
    value: u8,
}
impl PciInterruptPin {
    pub const fn new(pin: Option<PciIrqNum>) -> Self {
        match pin {
            Some(pin) => Self {
                // the +1 is because a value of zero represents no interrupt pin
                storage: (pin as u8) + 1,
            },
            None => Self { storage: 0 },
        }
    }
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
    pub fn kind(&self) -> PciHeaderKind {
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
    pub fn read(&self) -> u32 {
        pci_config_read(self.addr)
    }
    pub fn write(&self, value: u32) {
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
    fn new(reg: PciConfigReg) -> Self {
        Self {
            reg,
            phantom: PhantomData,
        }
    }
    pub fn read(&self) -> T {
        T::from_bits(self.reg.read())
    }
    pub fn write(&mut self, value: T) {
        self.reg.write(value.to_bits());
    }
    pub fn modify<F>(&mut self, modify: F)
    where
        F: FnOnce(&mut T),
    {
        let mut value = self.read();
        modify(&mut value);
        self.write(value);
    }
    pub fn reg(&self) -> PciConfigReg {
        self.reg
    }
    pub fn cast<O: BitPiece<Bits = u32>>(&self) -> PciConfigRegTyped<O> {
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

static PCI_INTERRUPT_CALLBACK_CHAINS: [CallbackChain; PCI_IRQ_LINES_AMOUNT] =
    [const { CallbackChain::new() }; PCI_IRQ_LINES_AMOUNT];

pub fn pci_interrupt_handler_register<'a, F: CallbackChainFn + 'a>(
    irq: PciIrqNum,
    callback: F,
) -> CallbackChainNode<'a> {
    let chain = &PCI_INTERRUPT_CALLBACK_CHAINS[irq as usize];
    chain.register(callback)
}

/// the pci interrupt handler. the irq argument specifies which pci interrupt line was raised - INTA, INTB, INTC or INTD.
pub fn pci_interrupt_handler(irq: PciIrqNum) {
    let chain = &PCI_INTERRUPT_CALLBACK_CHAINS[irq as usize];
    chain.trigger();
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum PciIrqNum {
    IntA = 0,
    IntB = 1,
    IntC = 2,
    IntD = 3,
}
impl PciIrqNum {
    pub fn i8259_irq_num(&self) -> I8259IrqNum {
        match self {
            PciIrqNum::IntA => I8259IrqNum::INTA,
            PciIrqNum::IntB => I8259IrqNum::INTB,
            PciIrqNum::IntC => I8259IrqNum::INTC,
            PciIrqNum::IntD => I8259IrqNum::INTD,
        }
    }
}

pub struct Piix4CorePciFunction<'a> {
    function: &'a mut PciFunctionInner,
}
impl<'a> Piix4CorePciFunction<'a> {
    pub fn new(function: &'a mut PciFunctionInner) -> Self {
        assert_eq!(function.id(), PciId::PIIX4_CORE);
        Self { function }
    }
    pub fn pci_irq_routing(&mut self) -> PciConfigRegTyped<Piix4PciIrqRouting> {
        PciConfigRegTyped::new(self.function.raw.config_reg(24))
    }
}

#[bitpiece(32)]
pub struct Piix4PciIrqRouting {
    pub inta: Piix4PciIrqRoute,
    pub intb: Piix4PciIrqRoute,
    pub intc: Piix4PciIrqRoute,
    pub intd: Piix4PciIrqRoute,
}

#[bitpiece(8)]
pub struct Piix4PciIrqRoute {
    pub routing: B4,
    pub reserved4: B3,
    pub disable_routing: bool,
}

pub struct PciBus {
    _raw: PciBusRaw,
    pub devs: Vec<PciDev>,
}
impl PciBus {
    fn scan(raw: PciBusRaw) -> Self {
        let mut bus = PciBus {
            _raw: raw,
            devs: Vec::new(),
        };
        for i in 0..PciDevNum::MAX.get() {
            let Some(raw_dev) = raw.dev(i) else {
                continue;
            };
            bus.devs.push(PciDev::scan(raw_dev));
        }
        bus
    }
}

pub struct PciDev {
    _raw: PciDevRaw,
    pub functions: Vec<PciFunction>,
}
impl PciDev {
    fn scan(raw: PciDevRaw) -> Self {
        let mut dev = PciDev {
            _raw: raw,
            functions: Vec::new(),
        };

        let mut function0 = raw.function0();

        if function0.header_type().is_multi_function() {
            for i in 1..PciFunctionNum::MAX.get() {
                if let Some(function) = raw.function(i) {
                    dev.functions.push(PciFunction::scan(function));
                }
            }
        }

        dev.functions.push(PciFunction::scan(function0));

        dev
    }
}

#[derive(Debug, Clone)]
pub struct PciFunction(pub Arc<IrqLock<PciFunctionInner>>);
impl PciFunction {
    fn scan(mut raw: PciFunctionRaw) -> PciFunction {
        let bars = raw.bars_array();
        PciFunction(Arc::new(IrqLock::new(PciFunctionInner { raw, bars })))
    }
    fn id(&self) -> PciId {
        let mut inner = self.0.lock();
        inner.raw.id()
    }
}

#[derive(Debug)]
pub struct PciFunctionInner {
    raw: PciFunctionRaw,
    bars: ArrayVec<PciBar, PCI_MAX_BARS>,
}
impl PciFunctionInner {
    pub fn raw(&mut self) -> &mut PciFunctionRaw {
        &mut self.raw
    }
    pub fn bars(&mut self) -> &mut [PciBar] {
        &mut self.bars
    }
}
impl Deref for PciFunctionInner {
    type Target = PciFunctionRaw;

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}
impl DerefMut for PciFunctionInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.raw
    }
}

pub struct PciHierarchy {
    pub buses: Vec<PciBus>,
}

pub static PCI_HIERARCHY: WriteOnce<PciHierarchy> = WriteOnce::new();

pub fn pci_find(id: PciId) -> Option<&'static PciFunction> {
    let hierarchy = PCI_HIERARCHY.get();
    for bus in &hierarchy.buses {
        for dev in &bus.devs {
            for function in &dev.functions {
                if function.id() == id {
                    return Some(function);
                }
            }
        }
    }
    None
}

pub fn pci_init() {
    let mut buses = Vec::new();

    // we currently don't support pci to pci buses, so we only scan the root bus.
    buses.push(PciBus::scan(PciBusRaw::new(0)));

    PCI_HIERARCHY.write(PciHierarchy { buses });
}
