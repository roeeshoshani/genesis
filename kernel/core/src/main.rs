#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::panic::PanicInfo;
use hal::{
    insn::MipsRelJumper,
    mem::{VirtAddr, EXCEPTION_VECTOR_BASE},
    sys::{
        Cp0Reg, Cp0RegCause, Cp0RegStatus, Cp0RegStatusFields, CpuErrorLevel, CpuExceptionLevel,
        InterruptBitmap, InterruptBitmapFields, OperatingMode,
    },
};
use uart::{uart_init, uart_read_byte};

pub mod uart;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    println!("panicked: {:?}", info);
    // TODO: disable interrupts before entering the infinite loop
    loop {}
}

fn general_exception_handler() {
    panic!("got interrupt");
}

fn write_general_exception_vector() {
    // use kseg1 to avoid the cache, since we are writing instructions, and we want it to go directly to ram.
    let interrupt_vector_addr = EXCEPTION_VECTOR_BASE.kseg1_addr().unwrap() + 0x180;
    let jumper = MipsRelJumper::new(
        VirtAddr(general_exception_handler as usize),
        interrupt_vector_addr,
    );
    unsafe {
        interrupt_vector_addr
            .as_mut_ptr::<MipsRelJumper>()
            .write_volatile(jumper)
    };
}

fn exceptions_init_cp0_status() {
    let mut status = Cp0RegStatus::read();

    // allow access to all coprocessors
    status.set_allow_access_to_coprocessor_0(true);
    status.set_allow_access_to_coprocessor_1(true);
    status.set_allow_access_to_coprocessor_2(true);
    status.set_allow_access_to_coprocessor_3(true);

    // don't use reverse endianness
    status.set_reverse_endianness(false);

    // don't use the bootstrap exception vectors, use the normal ones
    status.set_use_bootstrap_exception_vectors(false);

    // enable all interrupts in the interrupt mask. this will allow us to receive all types of interrupts once we eventually
    // enable interrupts.
    status.set_interrupt_mask(InterruptBitmap::ones());

    // initialize the operating mode properly
    status.set_operating_mode(OperatingMode::KernelMode);

    // clear the cpu error level that was set by the processor during reset
    status.set_error_level(CpuErrorLevel::NormalLevel);

    // clear the cpu exception level field to allow the processor to receive interrupts
    status.set_exception_level(CpuExceptionLevel::NormalLevel);

    // keep interrupts disabled for now. we will enable them once we are finished configuring the system.
    status.set_are_interrupts_enabled(false);

    // write the modified status value
    Cp0RegStatus::write(status);
}

fn exceptions_init_cp0_cause() {
    let mut cause = Cp0RegCause::read();

    // don't use the special interrupt vector, use the normal one
    cause.set_use_special_interrupt_vector(false);

    Cp0RegCause::write(cause);
}

fn exceptions_init() {
    write_general_exception_vector();
    exceptions_init_cp0_status();
    exceptions_init_cp0_cause();
}

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    exceptions_init();
    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
    }
}
