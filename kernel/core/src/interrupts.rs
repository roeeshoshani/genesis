use core::arch::global_asm;

use bitpiece::BitPiece;
use hal::{
    insn::{MipsAbsJump, MipsInsnReg, MipsMoveInsn},
    mem::{VirtAddr, GENERAL_EXCEPTION_VECTOR_ADDR},
    sys::{
        Cp0Reg, Cp0RegCause, Cp0RegStatus, CpuErrorLevel, CpuExceptionLevel, InterruptBitmap,
        OperatingMode,
    },
};

use crate::println;

pub fn interrupts_set_enabled(enabled: bool) {
    let mut status = Cp0RegStatus::read();
    status.set_are_interrupts_enabled(enabled);
    Cp0RegStatus::write(status);
}

#[derive(Clone)]
pub struct InterruptsPrevState {
    pub were_enabled: bool,
}

/// disables interrupts and returns the previous state of the interrupts.
pub fn interrupts_save() -> InterruptsPrevState {
    let mut status = Cp0RegStatus::read();

    let prev_state = InterruptsPrevState {
        were_enabled: status.are_interrupts_enabled(),
    };

    status.set_are_interrupts_enabled(false);
    Cp0RegStatus::write(status);

    prev_state
}

/// restores interrupts to their previous state
pub fn interrupts_restore(prev_state: InterruptsPrevState) {
    let mut status = Cp0RegStatus::read();
    status.set_are_interrupts_enabled(prev_state.were_enabled);
    Cp0RegStatus::write(status);
}

pub struct InterruptsDisabledGuard {
    prev_state: InterruptsPrevState,
}
impl InterruptsDisabledGuard {
    pub fn new() -> Self {
        Self {
            prev_state: interrupts_save(),
        }
    }
}
impl Drop for InterruptsDisabledGuard {
    fn drop(&mut self) {
        interrupts_restore(self.prev_state.clone());
    }
}

pub fn interrupts_disable() {
    interrupts_set_enabled(false);
}

pub fn interrupts_enable() {
    interrupts_set_enabled(true);
}

macro_rules! with_interrupts_disabled {
    {$($t:tt)*} => {{
        let _ = $crate::interrupts::InterruptsDisabledGuard::new();
        $($t)*
    }};
}
pub(crate) use with_interrupts_disabled;

#[repr(C)]
pub struct ExceptionVectorStub {
    pub save_tmp_reg: MipsMoveInsn,
    pub jump: MipsAbsJump,
}
impl ExceptionVectorStub {
    /// the register used to perform the absolute jump to the exception handler.
    /// use $t9 as the jump register to comply to mips PIC calling convention.
    /// this is required for the exception handler to be able to properly calculate its $gp value.
    pub const JUMP_REG: MipsInsnReg = MipsInsnReg::T9;

    pub fn new(target_addr: VirtAddr) -> Self {
        Self {
            // save the jump register in k0
            save_tmp_reg: MipsMoveInsn::new(MipsInsnReg::K0, Self::JUMP_REG),
            // perform the jump
            jump: MipsAbsJump::new(target_addr, Self::JUMP_REG),
        }
    }
}

global_asm!(
    // define a symbol for the raw exception handler
    ".globl raw_general_exception_handler",
    "raw_general_exception_handler:",

    // save assembler state
    ".set push",

    // this part requires precise control, so don't reorder
    ".set noreorder",

    // we are about to use $at directly, disable the warnings about this
    ".set noat",

    // first, perform mips PIC function prologue, which will properly initialize GP.
    //
    // this will allow us to later use the `la` assembler pseudo-instruction to load the address of the handler from
    // the GOT using gp-relative access, as should be performed in PIC mips code.
    ".cpload $t9",

    // restore the $t9 register which was saved by the stub in $k0
    "move $t9, $k0",

    // now save all registers on the stack, except for the following registers:
    // - the $zero register, which does not need to be saved for obvious reasons
    // - $sp, which is preserved across function calls due to calling convention
    // - $s0-$s7, which are preserved due to calling convention
    // NOTE: an alert reader might notice that we also don't need to save $s8, as it is also preserved across function calls,
    // but we still save it.
    // this is because we later use it as part of this handler (to store the pre-alignment stack pointer), so we must save it as well.
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
    "nop",

    // restore the pre-alignment stack pointer that we saved in $s8
    "move $sp, $s8",

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

    // return from the interrupt
    "eret",

    // restore assembler state
    ".set pop",

    handler = sym general_exception_handler,
);
unsafe extern "C" {
    unsafe fn raw_general_exception_handler();
}

extern "C" fn general_exception_handler() {
    let cause = Cp0RegCause::read();
    let pending = cause.pending_interrupts();

    // we do not support software interrupts
    let pending_software_interrupts = pending.software().0;
    if pending_software_interrupts != 0 {
        panic!(
            "software interrupts are not supported, but got pending software interrupts: 0b{:b}",
            pending_software_interrupts
        );
    }

    println!("pending hw: {:b}", pending.hardware().0);
}

fn write_general_exception_vector_sub() {
    // build the stub
    let stub = ExceptionVectorStub::new(VirtAddr(raw_general_exception_handler as usize));

    // calculate the virtual address to use when writing the stub to memory.
    //
    // use an uncachable address since we are writing instructions, and we want them to go directly to ram, and not be stuck
    // in the data cache.
    let general_exception_vector_addr = GENERAL_EXCEPTION_VECTOR_ADDR
        .kseg_uncachable_addr()
        .unwrap();

    // write it
    unsafe {
        general_exception_vector_addr
            .as_mut_ptr::<ExceptionVectorStub>()
            .write_volatile(stub)
    };

    // we do not need to flush the icache as we have never executed the exception vector yet, since we haven't enabled interrupts yet,
    // so we know that it can't be present in the icache at this point.
}

fn init_cp0_status() {
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

fn init_cp0_cause() {
    let mut cause = Cp0RegCause::read();

    // don't use the special interrupt vector, use the normal one
    cause.set_use_special_interrupt_vector(false);

    Cp0RegCause::write(cause);
}

pub fn interrupts_init() {
    write_general_exception_vector_sub();
    init_cp0_status();
    init_cp0_cause();
}
