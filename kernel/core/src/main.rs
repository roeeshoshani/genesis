#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use core::{arch::asm, panic::PanicInfo};
use hw::{
    interrupts::{interrupts_disable, interrupts_enable, interrupts_init, wait_for_interrupt},
    uart::{uart_init, uart_read_byte},
};
use mem::{alloc_pages, page_allocator_init};

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
    uart_init();
    interrupts_init();
    page_allocator_init();

    // done initializing, enable interrupts
    interrupts_enable();

    loop {
        wait_for_interrupt();
    }
}
