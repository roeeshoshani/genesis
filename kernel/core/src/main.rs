#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(allocator_api)]

extern crate alloc;

use core::panic::PanicInfo;
use executor::EXECUTOR;
use hw::{
    interrupts::{interrupts_disable, interrupts_enable, interrupts_init, wait_for_interrupt},
    uart::{uart_init, uart_read_byte, uart_task},
};
use mem::page_alloc::page_allocator_init;

pub mod executor;
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

fn spawn_initial_tasks() {
    let mut executor = EXECUTOR.lock();
    executor.spawn(uart_task());
}

/// returns whether we finished executing all tasks
fn poll_executor() -> bool {
    let mut executor = EXECUTOR.lock();
    executor.poll();
    executor.is_empty()
}

fn main_loop() -> ! {
    spawn_initial_tasks();
    loop {
        if poll_executor() {
            break;
        }
        wait_for_interrupt();
    }
    todo!("shutdown the device");
}

#[no_mangle]
extern "C" fn _start() -> ! {
    // initialize the uart. this will allow us to print to the serial console very early on.
    uart_init();

    // initialize the page allocator
    page_allocator_init();

    // initialize the interrupt management logic
    interrupts_init();

    // done initializing, enable interrupts
    interrupts_enable();

    main_loop();
}
