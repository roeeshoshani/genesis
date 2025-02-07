use core::arch::{asm, global_asm};

use bit_field::BitField;
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

use crate::hw::{pci::*, uart::uart_interrupt_handler};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct I8259IrqNum(pub u8);
impl I8259IrqNum {
    pub const TIMER: Self = Self(0);
    pub const INTA: Self = Self(3);
    pub const INTB: Self = Self(4);
    pub const INTC: Self = Self(5);
    pub const INTD: Self = Self(6);
}

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
        let _interrupts_disabled_guard = $crate::hw::interrupts::InterruptsDisabledGuard::new();
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

    if pending.tty2() {
        uart_interrupt_handler();
    }

    if pending.piix4_intr() {
        // acknowledge the interrupt
        let _ = Gt64120Regs::pci_0_interrupt_ack().read();

        // read the interrupt service register of the interrupt controller. this will tell us which interrupt we are currently servicing.
        // also make sure to apply the mask to ignore interrupts that are masked out.
        let isr = PIIX4_I8259_CHAIN.read_isr();

        // the isr should only have 1 bit set, since we only service one interrupt at a time.
        assert_eq!(isr.count_ones(), 1);

        // calculate the irq number by calculating the index of the bit that is set in the ISR.
        let irq_num = I8259IrqNum(isr.trailing_zeros() as u8);

        match irq_num {
            I8259IrqNum::TIMER => {
                // TODO
            }
            I8259IrqNum::INTA => {
                pci_interrupt_handler(PciIrqNum::IntA);
            }
            I8259IrqNum::INTB => {
                pci_interrupt_handler(PciIrqNum::IntB);
            }
            I8259IrqNum::INTC => {
                pci_interrupt_handler(PciIrqNum::IntC);
            }
            I8259IrqNum::INTD => {
                pci_interrupt_handler(PciIrqNum::IntD);
            }
            _ => {
                panic!("received unsupported i8259 irq number: {:?}", irq_num)
            }
        }

        PIIX4_I8259_CHAIN.eoi(irq_num);
    }
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

    // disable all interrupts. specific interrupts should be enabled on demand.
    status.set_interrupt_mask(InterruptBitmap::zeroes());

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

pub struct I8259Dev {
    pub cmd_reg: VolatilePtr<'static, u8>,
    pub data_reg: VolatilePtr<'static, u8>,
    pub is_master: bool,
}
impl I8259Dev {
    /// the total amount of irq lines in a single i8259 interrupt controller device.
    pub const IRQ_LINES_AMOUNT: usize = 8;

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

        // mask all interrupts
        self.set_mask(0xff);
    }

    pub fn set_mask(&self, mut mask: u8) {
        // when masking interrupts on the master, we don't want to mask irq2, since this is the irq on which the slave is connected.
        // masking slave interrupts should be done by writing the slave's interrupt mask, not by masking the slave from the parent.
        if self.is_master {
            mask &= !(1 << 2)
        }

        // reads/writes to the data register by default apply to the IMR (interrupt mask register).
        self.data_reg.write(mask);
    }

    pub fn get_mask(&self) -> u8 {
        self.data_reg.read()
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
    /// the total amount of irq lines in a chain of 2 i8259 interrupt controllers.
    pub const IRQ_LINES_AMOUNT: usize = I8259Dev::IRQ_LINES_AMOUNT * 2;

    pub fn init(&self) {
        self.master.init();
        self.slave.init();
    }

    fn merge_slave_master_reg(master_reg_val: u8, slave_reg_val: u8) -> u16 {
        master_reg_val as u16 | ((slave_reg_val as u16) << 8)
    }

    /// returns a tuple of `(master, slave)` register values
    fn split_slave_master_reg(reg_val: u16) -> (u8, u8) {
        ((reg_val & 0xff) as u8, ((reg_val >> 8) & 0xff) as u8)
    }

    pub fn read_irr(&self) -> u16 {
        Self::merge_slave_master_reg(self.master.read_irr(), self.slave.read_irr())
    }
    pub fn read_isr(&self) -> u16 {
        Self::merge_slave_master_reg(self.master.read_isr(), self.slave.read_isr())
    }

    pub fn set_mask(&self, mask: u16) {
        let (master_mask, slave_mask) = Self::split_slave_master_reg(mask);
        self.master.set_mask(master_mask);
        self.slave.set_mask(slave_mask);
    }
    pub fn get_mask(&self) -> u16 {
        Self::merge_slave_master_reg(self.master.get_mask(), self.slave.get_mask())
    }
    pub fn get_irq_mask(&self, irq: I8259IrqNum) -> bool {
        assert!(irq.0 < Self::IRQ_LINES_AMOUNT as u8);
        self.get_mask().get_bit(irq.0 as usize)
    }
    pub fn set_irq_mask(&self, irq: I8259IrqNum, is_masked: bool) {
        assert!(irq.0 < Self::IRQ_LINES_AMOUNT as u8);
        let mut mask = self.get_mask();
        mask.set_bit(irq.0 as usize, is_masked);
        self.set_mask(mask);
    }

    /// sends an end of interrupt to the interrupt controller chain, according to the given irq number of the interrupt.
    pub fn eoi(&self, irq_num: I8259IrqNum) {
        // if the irq number is from the slave, then send eoi to the slave as well
        if irq_num.0 >= 8 {
            self.slave.eoi();
        }

        // always send eoi to the master, even if the irq is from the slave, since all irqs pass through the master.
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

    // enable receiving interrupts from the piix4 intr line, which is connected to the output pin of the master i8259 device,
    // to allow receiving interrupts from the i8259.
    let mut status = Cp0RegStatus::read();
    status.interrupt_mask_mut().set_piix4_intr(true);
    Cp0RegStatus::write(status);
}

fn piix4_init() {
    // find the piix4 pci device
    let piix4 = Piix4CorePciFunction::new(pci_find(PciId::PIIX4_CORE).unwrap());

    // map pci irq lines to their desired i8259 irq lines.
    piix4
        .pci_irq_routing()
        .write(Piix4PciIrqRouting::from_fields(Piix4PciIrqRoutingFields {
            inta: Piix4PciIrqRouteFields {
                routing: BitPiece::from_bits(I8259IrqNum::INTA.0),
                reserved4: BitPiece::zeroes(),
                disable_routing: false,
            },
            intb: Piix4PciIrqRouteFields {
                routing: BitPiece::from_bits(I8259IrqNum::INTB.0),
                reserved4: BitPiece::zeroes(),
                disable_routing: false,
            },
            intc: Piix4PciIrqRouteFields {
                routing: BitPiece::from_bits(I8259IrqNum::INTC.0),
                reserved4: BitPiece::zeroes(),
                disable_routing: false,
            },
            intd: Piix4PciIrqRouteFields {
                routing: BitPiece::from_bits(I8259IrqNum::INTD.0),
                reserved4: BitPiece::zeroes(),
                disable_routing: false,
            },
        }));

    // make all pci irq lines level triggered since pci irq lines are always level triggered.
    for irq in [
        I8259IrqNum::INTA,
        I8259IrqNum::INTB,
        I8259IrqNum::INTC,
        I8259IrqNum::INTD,
    ] {
        piix4_set_irq_trigger_mode(irq, Piix4IrqTriggerMode::LevelTriggered);
    }
}

fn piix4_set_irq_trigger_mode(irq: I8259IrqNum, mode: Piix4IrqTriggerMode) {
    // for controlling the irq trigger mode, there are two 8-bit registers, where each bit represents the mode of a single irq line.
    //
    // so, we need to decide which register we want to use and the bit offset inside that register according to the irq line number
    // that was provided.
    let reg = if irq.0 < 8 {
        Piix4IoRegs::irq_trigger_mode_reg_1()
    } else {
        Piix4IoRegs::irq_trigger_mode_reg_1()
    };
    let bit_offset = irq.0 % 8;

    let mut value = reg.read();
    value.set_bit(bit_offset as usize, (mode as u8) != 0);
    reg.write(value);
}

/// puts the cpu to sleep until an external event is received (interrupt, nmi, or reset).
pub fn wait_for_interrupt() {
    unsafe { asm!("wait") };
}

pub fn interrupts_init() {
    write_general_exception_vector_sub();
    init_cp0_status();
    init_cp0_cause();
    i8259_init();
    piix4_init();
}
