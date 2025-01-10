use crate::{
    mem::PCI_0_IO,
    mmio_regs::{mmio_base, mmio_reg},
};

use bitpiece::*;

/// a struct representing the PIIX4 hardware I/O registers, providing access to them.
pub struct Piix4IoRegs;
impl Piix4IoRegs {
    mmio_base!(PCI_0_IO.start);

    mmio_reg! {
        master_8259_cmd, u8, ReadWrite, 0x20
    }
    mmio_reg! {
        master_8259_data, u8, ReadWrite, 0x21
    }
    mmio_reg! {
        slave_8259_cmd, u8, ReadWrite, 0xa0
    }
    mmio_reg! {
        slave_8259_data, u8, ReadWrite, 0xa1
    }
    mmio_reg! {
        timer_control, u8, ReadWrite, 0x43
    }
    mmio_reg! {
        counter_0, u8, ReadWrite, 0x40
    }
    mmio_reg! {
        counter_1, u8, ReadWrite, 0x41
    }
    mmio_reg! {
        counter_2, u8, ReadWrite, 0x42
    }
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piix4CounterStatus {
    pub countdown_kind: Piix4CountdownKind,
    pub counter_mode: Piix4CounterMode,
    pub rw_select: Piix4TimerRwSelect,
    pub is_count_not_yet_transferred: bool,
    pub pin_state: bool,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piix4TimerControlReadBackCmd {
    pub reserved0: B1,
    pub counter0_select: bool,
    pub counter1_select: bool,
    pub counter2_select: bool,
    pub latch_status: bool,
    pub latch_count: bool,
    pub must_be_true6: bool,
    pub must_be_true7: bool,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piix4TimerControlCounterLatchCmd {
    pub reserved0: B6,
    pub counter_select: Piix4CounterSelect,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piix4TimerControlRegularCmd {
    pub countdown_kind: Piix4CountdownKind,
    pub counter_mode: Piix4CounterMode,
    pub rw_select: Piix4TimerRwSelect,
    pub counter_select: Piix4CounterSelect,
}

#[bitpiece(2)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Piix4TimerRwSelect {
    Reserved = 0b00,
    RwLsb = 0b01,
    RwMsb = 0b10,
    RwLsbThenMsb = 0b11,
}

#[bitpiece(2)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Piix4CounterSelect {
    Counter0 = 0b00,
    Counter1 = 0b01,
    Counter2 = 0b10,
    Reserved = 0b11,
}

#[bitpiece(1)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Piix4CountdownKind {
    BinaryCountdown = 0,
    BinaryCodedDecimalCountdown = 1,
}

#[bitpiece(3)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Piix4CounterMode {
    OutOnEndOfCount = 0b000,
    HardwareTriggerableOneShot = 0b001,
    RateGenerator = 0b010,
    SquareWave = 0b011,
    SoftwareTriggeredStrobe = 0b100,
    HardwareTriggeredStrobe = 0b101,
    RateGenerator2 = 0b110,
    SquareWave2 = 0b111,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct I8259OpCmd3 {
    pub reg_read_cmd: I8259RegReadCmd,
    pub poll_mode_cmd: bool,
    pub must_be_true: bool,
    pub reserved4: B1,
    pub enable_special_mask_mode: bool,
    pub special_mask_mode: bool,
    pub reserved7: B1,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct I8259OpCmd2 {
    pub irq_line_select: B3,
    pub reserved: B2,
    pub cmd_kind: I8259OpCmd2Kind,
}

#[bitpiece(3)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum I8259OpCmd2Kind {
    RotateInAutoEoiModeClear = 0b000,
    NonSpecificEoiCmd = 0b001,
    Nop = 0b010,
    SpecificEoiCmd = 0b011,
    RotateInAutoEoiModeSet = 0b100,
    RotateOnNonSpecificEoiCmd = 0b101,
    SetPriorityCmd = 0b110,
    RotateOnSpecificEoiCmd = 0b111,
}

#[bitpiece(2)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum I8259RegReadCmd {
    NoAction0 = 0b00,
    NoAction1 = 0b01,
    ReadIrrReg = 0b10,
    ReadIsrReg = 0b11,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct I8259InitCmd1 {
    pub icw4_write_required: bool,
    pub reserved3: B3,
    pub must_be_true: bool,
    pub reserved5: B3,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct I8259InitCmd2 {
    pub reserved0: B3,
    pub interrupt_vector_base: B5,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct I8259InitCmd3Master {
    pub reserved0: B2,
    pub enable_cascaded_mode: bool,
    pub reserved3: B5,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct I8259InitCmd3Slave {
    pub slave_identification_code: B3,
    pub reserved3: B5,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct I8259InitCmd4 {
    pub microprocessor_mode: bool,
    pub automatic_end_of_interrupt: bool,
    pub reserved2: B2,
    pub special_fully_nested_mode: bool,
    pub reserved5: B3,
}
