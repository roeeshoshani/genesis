use core::arch::global_asm;

use bitpiece::*;
use hal::{
    insn::{MipsAbsJump, MipsInsnReg, MipsMoveInsn},
    mem::{VirtAddr, GENERAL_EXCEPTION_VECTOR_ADDR},
    mmio::{gt64120::Gt64120Regs, piix4::*},
    sys::{
        Cp0Reg, Cp0RegCause, Cp0RegStatus, CpuErrorLevel, CpuExceptionLevel, InterruptBitmap,
        OperatingMode,
    },
};
use volatile::VolatilePtr;

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
    if pending.software0() || pending.software1() {
        panic!("software interrupts are not supported, but got pending software interrupts");
    }

    println!("interrupt: {:?}", pending.to_fields());

    if pending.piix4_intr() {
        // acknowledge the interrupt
        let _ = Gt64120Regs::pci_0_interrupt_ack().read();

        // read the interrupt service register of the interrupt controller. this will tell us which interrupt we are currently servicing.
        let isr = PIIX4_I8259_CHAIN.read_isr();

        // the isr should only have 1 bit set, since we only service one interrupt at a time.
        assert_eq!(isr.count_ones(), 1);
        let irq = I8259Irq::from_bits(isr.trailing_zeros() as u8);

        match irq {
            I8259Irq::Timer => {
                // TODO
            }
            _ => {
                panic!("received unsupported i8259 irq: {:?}", irq)
            }
        }

        PIIX4_I8259_CHAIN.eoi();
    }
}

#[bitpiece(4)]
#[derive(Debug, Clone, Copy)]
pub enum I8259Irq {
    Timer = 0,
    Keyboard = 1,
    Reserved2 = 2,
    Tty1 = 3,
    Tty2 = 4,
    Unused5 = 5,
    FloppyDisk = 6,
    ParallelPort = 7,
    RealTimeClock = 8,
    I2C = 9,
    PciAB = 10,
    PciCD = 11,
    Mouse = 12,
    Reserved13 = 13,
    PrimaryIde = 14,
    SecondaryIde = 15,
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

    // enable all interrupts, except for the tty2 UART interrupts.
    //
    // qemu's implementation of the the tty2 UART device generates interrupts requests to the cpu even when we configure
    // it to not use interrupts.
    //
    // we need to be able to write to the UART without generating interrupts, because we want to write to it inside our interrupt handler,
    // and this can generate an infinite interrupt loop.
    //
    // so, we mask this interrupt out.
    let mut interrupt_enable_mask = InterruptBitmap::ones();
    interrupt_enable_mask.set_tty2(false);
    // NOTE: i currently disable the timer interrupt just so that it is easier for me to work with the PIIX4 interrupts to understand
    // how they work.
    // TODO: remove this to re-enable the timer interrupt
    interrupt_enable_mask.set_timer(false);
    status.set_interrupt_mask(interrupt_enable_mask);

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

fn piix4_timer_init() {
    Piix4IoRegs::timer_control().write(
        Piix4TimerControlRegularCmd::from_fields(Piix4TimerControlRegularCmdFields {
            countdown_kind: Piix4CountdownKind::BinaryCountdown,
            counter_mode: Piix4CounterMode::HardwareTriggeredStrobe,
            rw_select: Piix4TimerRwSelect::RwLsbThenMsb,
            counter_select: Piix4CounterSelect::Counter0,
        })
        .to_bits(),
    );
    Piix4IoRegs::counter_0().write(u8::MAX);
    Piix4IoRegs::counter_0().write(u8::MAX);

    Piix4IoRegs::timer_control().write(
        Piix4TimerControlRegularCmd::from_fields(Piix4TimerControlRegularCmdFields {
            countdown_kind: Piix4CountdownKind::BinaryCountdown,
            counter_mode: Piix4CounterMode::HardwareTriggeredStrobe,
            rw_select: Piix4TimerRwSelect::RwLsbThenMsb,
            counter_select: Piix4CounterSelect::Counter1,
        })
        .to_bits(),
    );
    Piix4IoRegs::counter_1().write(u8::MAX);
    Piix4IoRegs::counter_1().write(u8::MAX);

    Piix4IoRegs::timer_control().write(
        Piix4TimerControlRegularCmd::from_fields(Piix4TimerControlRegularCmdFields {
            countdown_kind: Piix4CountdownKind::BinaryCountdown,
            counter_mode: Piix4CounterMode::HardwareTriggeredStrobe,
            rw_select: Piix4TimerRwSelect::RwLsbThenMsb,
            counter_select: Piix4CounterSelect::Counter2,
        })
        .to_bits(),
    );
    Piix4IoRegs::counter_2().write(u8::MAX);
    Piix4IoRegs::counter_2().write(u8::MAX);
}

pub struct I8259Dev {
    pub cmd_reg: VolatilePtr<'static, u8>,
    pub data_reg: VolatilePtr<'static, u8>,
    pub is_master: bool,
}
impl I8259Dev {
    pub fn init(&self) {
        self.cmd_reg.write(
            I8259InitCmd1::from_fields(I8259InitCmd1Fields {
                icw4_write_required: true,
                reserved3: BitPiece::zeroes(),
                must_be_true: true,
                reserved5: BitPiece::zeroes(),
            })
            .to_bits(),
        );
        self.data_reg.write(
            I8259InitCmd2::from_fields(I8259InitCmd2Fields {
                reserved0: BitPiece::zeroes(),
                interrupt_vector_base: BitPiece::from_bits(if self.is_master {
                    // the master's base irq is 0
                    0
                } else {
                    // the slave's base irq is 8, since the master has 8 irqs
                    8
                }),
            })
            .to_bits(),
        );
        if self.is_master {
            self.data_reg.write(
                I8259InitCmd3Master::from_fields(I8259InitCmd3MasterFields {
                    reserved0: BitPiece::zeroes(),
                    enable_cascaded_mode: true,
                    reserved3: BitPiece::zeroes(),
                })
                .to_bits(),
            );
        } else {
            self.data_reg.write(
                I8259InitCmd3Slave::from_fields(I8259InitCmd3SlaveFields {
                    slave_identification_code: BitPiece::from_bits(0b010),
                    reserved3: BitPiece::zeroes(),
                })
                .to_bits(),
            );
        }
        self.data_reg.write(
            I8259InitCmd4::from_fields(I8259InitCmd4Fields {
                microprocessor_mode: true,
                automatic_end_of_interrupt: false,
                reserved2: BitPiece::zeroes(),
                special_fully_nested_mode: false,
                reserved5: BitPiece::zeroes(),
            })
            .to_bits(),
        );
    }

    fn read_op3_reg(&self, reg_read_cmd: I8259RegReadCmd) -> u8 {
        self.cmd_reg.write(
            I8259OpCmd3::from_fields(I8259OpCmd3Fields {
                reg_read_cmd,
                poll_mode_cmd: false,
                must_be_true: true,
                reserved4: BitPiece::zeroes(),
                enable_special_mask_mode: false,
                special_mask_mode: false,
                reserved7: BitPiece::zeroes(),
            })
            .to_bits(),
        );
        self.cmd_reg.read()
    }

    pub fn read_irr(&self) -> u8 {
        self.read_op3_reg(I8259RegReadCmd::ReadIrrReg)
    }
    pub fn read_isr(&self) -> u8 {
        self.read_op3_reg(I8259RegReadCmd::ReadIsrReg)
    }
    pub fn eoi(&self) {
        self.cmd_reg.write(
            I8259OpCmd2::from_fields(I8259OpCmd2Fields {
                irq_line_select: BitPiece::zeroes(),
                reserved: BitPiece::zeroes(),
                cmd_kind: I8259OpCmd2Kind::NonSpecificEoiCmd,
            })
            .to_bits(),
        );
    }
}

/// a chain of 2 i8259 devices.
pub struct I8259Chain {
    pub master: I8259Dev,
    pub slave: I8259Dev,
}
impl I8259Chain {
    pub fn init(&self) {
        self.master.init();
        self.slave.init();
    }
    pub fn read_irr(&self) -> u16 {
        self.master.read_irr() as u16 | ((self.slave.read_irr() as u16) << 8)
    }
    pub fn read_isr(&self) -> u16 {
        self.master.read_isr() as u16 | ((self.slave.read_isr() as u16) << 8)
    }
    pub fn eoi(&self) {
        // TODO: do we always want to do an EOI on the slave here? no we don't... but for that we need to know which irq line
        // the interrupt was received on, which i currently have no idea how to do.
        self.slave.eoi();
        self.master.eoi();
    }
}

pub const PIIX4_I8259_MASTER: I8259Dev = I8259Dev {
    cmd_reg: Piix4IoRegs::master_8259_cmd(),
    data_reg: Piix4IoRegs::master_8259_data(),
    is_master: true,
};
pub const PIIX4_I8259_SLAVE: I8259Dev = I8259Dev {
    cmd_reg: Piix4IoRegs::slave_8259_cmd(),
    data_reg: Piix4IoRegs::slave_8259_data(),
    is_master: false,
};
pub const PIIX4_I8259_CHAIN: I8259Chain = I8259Chain {
    master: PIIX4_I8259_MASTER,
    slave: PIIX4_I8259_SLAVE,
};

fn i8259_init() {
    PIIX4_I8259_CHAIN.init();
}

pub fn interrupts_init() {
    write_general_exception_vector_sub();
    init_cp0_status();
    init_cp0_cause();
    piix4_timer_init();
    i8259_init();
}
