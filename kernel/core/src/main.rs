#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::{arch::global_asm, panic::PanicInfo};
use hal::{
    insn::{MipsAbsJump, MipsInsnReg, MipsPushReg},
    mem::{VirtAddr, GENERAL_EXCEPTION_VECTOR_ADDR},
    sys::{
        Cp0Reg, Cp0RegCause, Cp0RegStatus, CpuErrorLevel, CpuExceptionLevel, InterruptBitmap,
        OperatingMode,
    },
};
use uart::{uart_init, uart_read_byte};

pub mod uart;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    disable_interrupts();
    println!("{}", info);
    loop {}
}

pub fn set_interrupts_enabled(enabled: bool) {
    let mut status = Cp0RegStatus::read();
    status.set_are_interrupts_enabled(enabled);
    Cp0RegStatus::write(status);
}

pub fn disable_interrupts() {
    set_interrupts_enabled(false);
}

pub fn enable_interrupts() {
    set_interrupts_enabled(true);
}

global_asm!(
    // define a symbol for the raw exception handler
    ".globl raw_general_exception_handler",
    "raw_general_exception_handler:",

    // this part requires precise control, so don't reorder
    ".set noreorder",

    // we are about to use $at directly, disable the warnings about this
    ".set noat",

    // first, perform mips PIC function prologue, which will properly initialize GP.
    //
    // this will allow us to later use the `la` assembler pseudo-instruction to load the address of the handler from
    // the GOT using gp-relative access, as should be performed in PIC mips code.
    ".cpload $t9",

    // restore the register pushed to the stack by the stub
    "lw $t9, 0($sp)",
    "addiu $sp, $sp, 4",

    // now save all registers on the stack, except for the following registers:
    // - the $zero register, which does not need to be saved for obvious reasons
    // - $sp, which is preserved across function calls due to calling convention
    // - $s0-$s7, which are preserved due to calling convention
    // NOTE: an alert reader might notice that we also don't need to save $s8, as it is also preserved across function calls.
    // but, we later use it as part of this handler (to store the pre-alignment stack pointer), so we must save it as well.
    "addiu $sp, $sp, -88",
    "sw $at, 0($sp)",
    "sw $v0, 4($sp)",
    "sw $v1, 8($sp)",
    "sw $a0, 12($sp)",
    "sw $a1, 16($sp)",
    "sw $a2, 20($sp)",
    "sw $a3, 24($sp)",
    "sw $t0, 28($sp)",
    "sw $t1, 32($sp)",
    "sw $t2, 36($sp)",
    "sw $t3, 40($sp)",
    "sw $t4, 44($sp)",
    "sw $t5, 48($sp)",
    "sw $t6, 52($sp)",
    "sw $t7, 56($sp)",
    "sw $t8, 60($sp)",
    "sw $t9, 64($sp)",
    "sw $k0, 68($sp)",
    "sw $k1, 72($sp)",
    "sw $gp, 76($sp)",
    "sw $s8, 80($sp)",
    "sw $ra, 84($sp)",

    // we now want to align the stack to 8 bytes, which is required according to the mips abi.
    // but first, we must save the original stack pointer before aligning, so that we can restore it later.
    // we save is in $s8 as it is saved across function calls, so the handler will not overwrite it.
    "move $s8, $sp",

    // calculate the mask required to align the stack pointer to 8 bytes
    "li $t0, 0xfffffff8",

    // now align the stack pointer
    "and $sp, $sp, $t0",

    // call the handler using $t9 to comply to the mips PIC calling convention
    "la $t9, {handler}",
    "jalr $t9",

    // restore the pre-alignment stack pointer that we saved in $s8
    // NOTE: we can't use macros here, and `move` is a macro, so we use `or`.
    "or $sp, $s8, $0",

    // restore all registers
    "lw $at, 0($sp)",
    "lw $v0, 4($sp)",
    "lw $v1, 8($sp)",
    "lw $a0, 12($sp)",
    "lw $a1, 16($sp)",
    "lw $a2, 20($sp)",
    "lw $a3, 24($sp)",
    "lw $t0, 28($sp)",
    "lw $t1, 32($sp)",
    "lw $t2, 36($sp)",
    "lw $t3, 40($sp)",
    "lw $t4, 44($sp)",
    "lw $t5, 48($sp)",
    "lw $t6, 52($sp)",
    "lw $t7, 56($sp)",
    "lw $t8, 60($sp)",
    "lw $t9, 64($sp)",
    "lw $k0, 68($sp)",
    "lw $k1, 72($sp)",
    "lw $gp, 76($sp)",
    "lw $s8, 80($sp)",
    "lw $ra, 84($sp)",
    "addiu $sp, $sp, 88",

    // TODO: iret?

    // restore assembler state
    ".set at",
    ".set reorder",

    handler = sym general_exception_handler,
);
unsafe extern "C" {
    unsafe fn raw_general_exception_handler();
}

fn general_exception_handler() {
    panic!("got interrupt");
}

#[repr(C)]
pub struct ExceptionVectorStub {
    pub push: MipsPushReg,
    pub jump: MipsAbsJump,
}
impl ExceptionVectorStub {
    pub fn new(target_addr: VirtAddr) -> Self {
        // use $t9 as the jump register to comply to mips PIC calling convention.
        // this is required for the exception handler to be able to properly calculate its $gp value.
        let jump_reg = MipsInsnReg::T9;
        Self {
            push: MipsPushReg::new(jump_reg),
            jump: MipsAbsJump::new(target_addr, jump_reg),
        }
    }
}

fn write_general_exception_vector() {
    // build the stub
    let stub = ExceptionVectorStub::new(VirtAddr(raw_general_exception_handler as usize));

    // when writing the stub, use kseg1, to avoid the cache.
    // we are writing instructions, and we want them to go directly to ram, and not be stuck in the data cache.
    let general_exception_vector_addr = GENERAL_EXCEPTION_VECTOR_ADDR.kseg1_addr().unwrap();
    unsafe {
        general_exception_vector_addr
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
    println!("stub size: {}", size_of::<ExceptionVectorStub>());
    uart_init();
    exceptions_init();
    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
        if byte == b'Z' {
            enable_interrupts();
        }
    }
}
