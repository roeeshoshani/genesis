#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::{panic::PanicInfo, sync::atomic::AtomicUsize};
use hal::{
    mem::{PhysAddr, PhysMemRegion, PCI_0_IO, PCI_0_MEM},
    mmio::gt64120::Gt64120Regs,
};
use interrupts::{
    interrupts_disable, interrupts_enable, interrupts_init, PIIX4_I8259_CHAIN, PIIX4_I8259_MASTER,
};
use pci::{pci_scan, PciBarKind, PciBarReg, PciConfigRegTyped, PciFunction, PciId, PciMemBar};
use thiserror_no_std::Error;
use uart::{uart_init, uart_read_byte};
use utils::HexDisplay;

pub mod interrupts;
pub mod pci;
pub mod uart;
pub mod utils;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    interrupts_disable();
    println!("{}", info);
    loop {}
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

fn probe_piix4_core(function: PciFunction) {
    println!("probing piix4");
}

fn probe_pci_function(function: PciFunction) {
    match function.id() {
        PciId::PIIX4_CORE => {
            probe_piix4_core(function);
        }
        _ => {
            println!("unknown pci function id {:x?}", function.id());
        }
    }
}

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    interrupts_init();

    pci_scan(probe_pci_function);

    interrupts_enable();
    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
    }
}
