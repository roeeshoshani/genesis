#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::panic::PanicInfo;
use hal::{
    insn::{MipsAbsJumper, MipsInsnReg, MipsPushReg, MipsRelJumper},
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
    println!("{}", info);
    // TODO: disable interrupts before entering the infinite loop
    loop {}
}

fn general_exception_handler() {
    panic!("got interrupt");
}

#[repr(C)]
pub struct ExceptionVectorStub {
    pub push: MipsPushReg,
    pub jumper: MipsAbsJumper,
}
impl ExceptionVectorStub {
    pub fn new(target_addr: VirtAddr, tmp_reg: MipsInsnReg) -> Self {
        Self {
            push: MipsPushReg::new(tmp_reg),
            jumper: MipsAbsJumper::new(tmp_reg, target_addr),
        }
    }
}

fn write_general_exception_vector() {
    // build the stub
    let stub = ExceptionVectorStub::new(
        VirtAddr(general_exception_handler as usize),
        MipsInsnReg::RA,
    );

    // when writing the stub, use kseg1, to avoid the cache.
    // we are writing instructions, and we want them to go directly to ram, and not be stuck in the data cache.
    let interrupt_vector_write_addr = EXCEPTION_VECTOR_BASE.kseg1_addr().unwrap() + 0x180;
    unsafe {
        interrupt_vector_write_addr
            .as_mut_ptr::<ExceptionVectorStub>()
            .write_volatile(stub)
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
