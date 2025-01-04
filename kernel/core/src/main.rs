#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use core::{panic::PanicInfo, ptr::NonNull};
use hal::mem::{PhysAddr, PCI_0_MEM};
use interrupts::{interrupts_disable, interrupts_enable, interrupts_init};
use pci::PciScanner;
use uart::{uart_init, uart_read_byte};
use volatile::VolatilePtr;

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

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    interrupts_init();

    let mut scanner = PciScanner::new(|f| println!("{:?}", f));
    scanner.scan();

    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
        if byte == b'Z' {
            interrupts_enable();
        }
    }
}
