#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use core::panic::PanicInfo;
use hw::{
    interrupts::{interrupts_disable, interrupts_enable, interrupts_init},
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

/// a main loop for testing the uart functionality.
fn test_uart_main_loop() -> ! {
    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
    }
}

#[no_mangle]
extern "C" fn _start() -> ! {
    uart_init();
    interrupts_init();
    page_allocator_init();

    // done initializing, enable interrupts
    interrupts_enable();

    test_uart_main_loop()
}
