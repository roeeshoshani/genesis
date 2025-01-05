#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::panic::PanicInfo;
use interrupts::{interrupts_disable, interrupts_enable, interrupts_init};
use pci::{pci_scan, PciBarRegByKind, PciConfigRegTyped, PciFunction, PciId};
use uart::{uart_init, uart_read_byte};

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

// pub struct

fn probe_piix4_core(function: PciFunction) {
    // function.config_reg()
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

fn pci_function_map_bars(function: PciFunction) {
    for bar in function.bars() {
        match bar.by_kind() {
            PciBarRegByKind::Mem(mem_bar) => {
                println!("mem bar addr: {:x?}", mem_bar.read().to_fields());
            }
            PciBarRegByKind::Io(io_bar) => {
                println!("io bar addr: {:x?}", io_bar.read().to_fields());
            }
        }
    }
}

fn pci_map_bars() {
    pci_scan(pci_function_map_bars);
}

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    interrupts_init();

    pci_map_bars();
    // pci_scan(probe_pci_function);

    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
    }
}
