#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(allocator_api)]

extern crate alloc;

use core::panic::PanicInfo;
use executor::EXECUTOR;
use hw::{
    interrupts::{
        are_interrupts_enabled, interrupts_disable, interrupts_enable, interrupts_init,
        wait_for_interrupt,
    },
    nic::nic_task,
    pci::pci_init,
    uart::{uart_init, uart_task},
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
    loop {
        wait_for_interrupt();
    }
}

fn spawn_initial_tasks() {
    let mut executor = EXECUTOR.lock();
    executor.spawn(uart_task());
    executor.spawn(nic_task());
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
        assert!(are_interrupts_enabled());
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

    // initialize pci. pci initialization requires memory allocation, so the memory allocation must be initialized first.
    pci_init();

    // initialize the interrupt management logic. interrupt initialization requires access to pci devices, so pci must be
    // initialized first.
    interrupts_init();

    // done initializing, enable interrupts
    interrupts_enable();

    main_loop();
}
