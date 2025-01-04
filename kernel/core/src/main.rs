#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::{panic::PanicInfo, ptr::NonNull};
use hal::{mem::PhysAddr, mmio::gt64120::Gt64120Regs};
use interrupts::{
    interrupts_disable, interrupts_enable, interrupts_init, with_interrupts_disabled,
};
use paste::paste;
use uart::{uart_init, uart_read_byte};
use utils::max_val_of_bit_len;

pub mod interrupts;
pub mod uart;
pub mod utils;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    interrupts_disable();
    println!("{}", info);
    loop {}
}

#[bitpiece(32)]
#[derive(Debug)]
pub struct PciConfigAddr {
    pub zero: B2,
    pub reg_num: B6,
    pub function_num: B3,
    pub dev_num: B5,
    pub bus_num: u8,
    pub reserved: B7,
    pub enabled: bool,
}

const PCI_MAX_DEV: u8 = max_val_of_bit_len!(5);
const PCI_MAX_FUNCTION: u8 = max_val_of_bit_len!(3);
const PCI_MAX_REGISTER: u8 = max_val_of_bit_len!(6);

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
        assert!(dev_num <= PCI_MAX_DEV);

        let mut dev_addr = self.addr;
        dev_addr.set_dev_num(B5(dev_num));
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
        assert!(function_num <= PCI_MAX_FUNCTION);

        let mut function_addr = self.addr;
        function_addr.set_function_num(B3(function_num));

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
        self.addr.dev_num().0
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
pub struct PciFunction {
    addr: PciConfigAddr,
}
impl PciFunction {
    pub fn config_reg(self, reg_num: u8) -> PciConfigReg {
        assert!(reg_num <= PCI_MAX_REGISTER);

        let mut reg_addr = self.addr;
        reg_addr.set_reg_num(B6(reg_num));

        PciConfigReg { addr: reg_addr }
    }

    // define helper functions for all registers that have special bitfields in them
    pci_function_define_bitfield_reg!(0);
    pci_function_define_bitfield_reg!(1);
    pci_function_define_bitfield_reg!(2);
    pci_function_define_bitfield_reg!(3);

    /// checks if this pci function even exists.
    fn exists(self) -> bool {
        self.vendor_id() != 0xffff
    }

    pub fn vendor_id(self) -> u16 {
        self.read_config_reg0().vendor_id()
    }

    pub fn device_id(self) -> u16 {
        self.read_config_reg0().device_id()
    }

    pub fn class_code(self) -> u8 {
        self.read_config_reg2().class_code()
    }

    pub fn subclass(self) -> u8 {
        self.read_config_reg2().subclass()
    }

    pub fn header_type(self) -> PciHeaderType {
        self.read_config_reg3().header_type()
    }

    pub fn bus_num(&self) -> u8 {
        self.addr.bus_num()
    }
    pub fn dev_num(&self) -> u8 {
        self.addr.dev_num().0
    }
    pub fn function_num(&self) -> u8 {
        self.addr.function_num().0
    }
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

#[bitpiece(8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciHeaderType {
    pub raw_kind: B7,
    pub is_multi_function: bool,
}
impl PciHeaderType {
    pub fn kind(self) -> PciHeaderKind {
        match self.raw_kind().0 {
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
        self.addr.dev_num().0
    }
    pub fn function_num(&self) -> u8 {
        self.addr.function_num().0
    }
    pub fn reg_num(&self) -> u8 {
        self.addr.reg_num().0
    }
}

pub struct PciScanner {}
impl PciScanner {
    pub fn new() -> Self {
        Self {}
    }

    pub fn scan(&mut self) {
        self.scan_bus(PciBus::new(0));
    }

    pub fn scan_bus(&mut self, bus: PciBus) {
        for i in 0..PCI_MAX_DEV {
            if let Some(dev) = bus.dev(i) {
                self.scan_dev(dev);
            }
        }
    }

    pub fn scan_dev(&mut self, dev: PciDev) {
        let function0 = dev.function0();

        self.scan_function(function0);

        if function0.header_type().is_multi_function() {
            for i in 1..PCI_MAX_FUNCTION {
                if let Some(function) = dev.function(i) {
                    self.scan_function(function);
                }
            }
        }
    }

    pub fn scan_function(&mut self, function: PciFunction) {
        println!("found function: {:?}", function.addr.to_fields());
    }
}

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    interrupts_init();

    let mut scanner = PciScanner::new();
    scanner.scan();

    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
        if byte == b'Z' {
            interrupts_enable();
        }
    }
}
