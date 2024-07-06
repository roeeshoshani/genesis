#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use core::{arch::global_asm, panic::PanicInfo};

global_asm!(include_str!("main.S"));

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}
