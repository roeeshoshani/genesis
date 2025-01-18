#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use core::panic::PanicInfo;
use hal::mem::{PhysMemRegion, PCI_0_IO, RAM_0};
use interrupts::{interrupts_disable, interrupts_enable, interrupts_init};
use mm::{kernel_end_phys, kernel_size, PageAllocator};
use uart::{uart_init, uart_read_byte};
use utils::HexDisplay;

pub mod interrupts;
pub mod mm;
pub mod pci;
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
    println!(
        "setting up allocator from {:x} to {:x}",
        kernel_end_phys().0,
        RAM_0.inclusive_end.0
    );
    let mut allocator = PageAllocator::new();
    unsafe {
        // SAFETY: this memory region is unused
        allocator.add_region(PhysMemRegion {
            start: kernel_end_phys(),
            inclusive_end: RAM_0.inclusive_end,
        });
    }

    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
    }
}

#[no_mangle]
extern "C" fn _start() -> ! {
    uart_init();
    interrupts_init();

    // done initializing, enable interrupts
    interrupts_enable();

    test_uart_main_loop()
}
