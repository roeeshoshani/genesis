#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(allocator_api)]

use core::panic::PanicInfo;
use hw::{
    interrupts::{interrupts_disable, interrupts_enable, interrupts_init, wait_for_interrupt},
    uart::{uart_init, uart_init_interrupts},
};
use mem::page_alloc::page_allocator_init;

pub mod hw;
pub mod mem;
pub mod sync;
pub mod utils;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    interrupts_disable();
    println!("{}", info);
    loop {}
}

#[no_mangle]
extern "C" fn _start() -> ! {
    // initialize the uart. this will allow us to print to the serial console very early on.
    uart_init();

    // initialize the interrupt management logic
    interrupts_init();

    // initialize the uart interrupt management logic
    uart_init_interrupts();

    // initialize the page allocator
    page_allocator_init();

    // done initializing, enable interrupts
    interrupts_enable();

    loop {
        wait_for_interrupt();
    }
}
