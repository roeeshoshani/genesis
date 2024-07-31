#![no_std]
#![no_main]

use core::panic::PanicInfo;
use hal::*;

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
extern "C" fn _start() {
    loop {}
}
