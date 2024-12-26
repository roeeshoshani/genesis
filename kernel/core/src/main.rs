#![no_std]
#![no_main]

use core::panic::PanicInfo;
use uart::{uart_init, uart_read_byte, uart_write_byte};

pub mod uart;

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    loop {
        let byte = uart_read_byte();
        uart_write_byte(byte);
    }
}
