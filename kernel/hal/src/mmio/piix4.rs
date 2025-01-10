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
