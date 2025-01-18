#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use core::panic::PanicInfo;
use hal::mem::{PhysMemRegion, RAM_0};
use interrupts::{interrupts_disable, interrupts_enable, interrupts_init};
use mm::{alloc_pages, kernel_end_phys, page_allocator_init, PageAllocator};
use uart::{uart_init, uart_read_byte};

pub mod interrupts;
pub mod mm;
pub mod pci;
pub mod sync;
pub mod uart;
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

    let pages = alloc_pages(4).unwrap();
    println!("{:x}", pages.addr().0);

    test_uart_main_loop()
}
