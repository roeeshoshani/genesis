#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

fn foo() {}

fn bar() {}

#[inline(never)]
fn shit() {
    unsafe { BAR = foo };
}

static mut SHIT: fn() = foo;
static mut BAR: fn() = bar;

#[no_mangle]
extern "C" fn _start() {
    loop {
        shit();
    }
}
