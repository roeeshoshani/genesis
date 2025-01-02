use core::ptr::NonNull;

use crate::{
    mem::PhysAddr,
    mmio_regs::{mmio_base, mmio_reg},
};
use bitpiece::*;

/// a struct representing the UART hardware registers, providing access to them.
pub struct UartRegs;
impl UartRegs {
    mmio_base!(PhysAddr(0x1F00_0900));

    mmio_reg! {
        /// the RX register.
        /// this is only accessible if the divisor latch access bit is not set.
        rx, u8, ReadOnly, 0x0
    }
    mmio_reg! {
        /// the TX register.
        /// this is only accessible if the divisor latch access bit is not set.
        tx, u8, WriteOnly, 0x0
    }
    mmio_reg! {
        /// the INTEN (interrupt enable) register.
        interrupt_enable, UartInterruptEnableReg, ReadWrite, 0x8
    }
    mmio_reg! {
        /// the II (interrup identification) register.
        interrupt_id, UartInterruptIdReg, ReadOnly, 0x10
    }
    mmio_reg! {
        /// the FIFO (fifo control) register.
        fifo_control, UartFifoControlReg, WriteOnly, 0x10
    }
    mmio_reg! {
        /// the LCTRL (line control) register.
        line_control, UartLineControlReg, ReadWrite, 0x18
    }
    mmio_reg! {
        /// the MCTRL (modem control) register.
        modem_control, UartModemControlReg, ReadWrite, 0x20
    }
    mmio_reg! {
        /// the LSTAT (line status) register.
        line_status, UartLineStatusReg, ReadWrite, 0x28
    }
    mmio_reg! {
        /// the MSTAT (modem status) register.
        modem_status, UartModemStatusReg, ReadWrite, 0x30
    }
    mmio_reg! {
        /// the SCRATCH register.
        scratch, u8, ReadWrite, 0x38
    }
    mmio_reg! {
        /// the DLL (divisor latch lsb) register.
        /// this is only accessible if the divisor latch access bit is set.
        divisor_latch_lsb, u8, ReadWrite, 0x0
    }
    mmio_reg! {
        /// the DLM (divisor latch msb) register.
        /// this is only accessible if the divisor latch access bit is set.
        divisor_latch_msb, u8, ReadWrite, 0x8
    }
}

/// the UART interrupt enable register. this is a read-write register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct UartInterruptEnableReg {
    pub is_received_data_available_interrupt_enabled: bool,
    pub is_transmitter_holding_register_empty_interrupt_enabled: bool,
    pub is_receiver_line_status_interrupt_enabled: bool,
    pub is_modem_status_interrupt_enabled: bool,
    pub zero: B4,
}

/// the UART interrupt identification register. this is a read-only register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct UartInterruptIdReg {
    pub interrupt_status: UartInterruptStatus,
    pub interrupt_id: B3,
    pub zero: B2,
    pub are_fifos_enabled: B2,
}

/// the UART FIFO-control register. this is a write-only register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct UartFifoControlReg {
    pub is_fifo_enabled: bool,
    pub receiver_fifo_reset: bool,
    pub transmitter_fifo_reset: bool,
    pub dma_mode_select: B1,
    pub reserved: B2,
    pub receiver_trigger_level: UartFifoReceiverTriggerLevel,
}

/// the UART line-control register. this is a read-write register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct UartLineControlReg {
    pub word_length: UartWordLength,
    pub use_extra_stop_bits: bool,
    pub is_parity_enabled: bool,
    pub parity_mode: UartParityMode,
    pub use_sticky_parity: bool,
    pub enable_break_condition: bool,
    pub enable_divisor_latch_access: bool,
}

/// the UART modem-control register. this is a read-write register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct UartModemControlReg {
    pub data_terminal_ready: B1,
    pub request_to_send: B1,
    pub out: B2,
    pub is_loop_mode_enabled: bool,
    pub autoflow_control_enable: B1,
    pub zero: B2,
}

/// the UART line status register. this is a read-write register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct UartLineStatusReg {
    pub is_data_ready: bool,
    pub receive_errors: UartReceiveErrors,
    pub has_break_interrupt_occured: bool,
    pub is_transmitter_holding_register_empty: bool,
    pub is_transmitter_empty: bool,
    pub has_error_in_receiver_fifo: bool,
}

/// the UART modem status register. this is a read-write register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct UartModemStatusReg {
    pub has_clear_to_send_changed: bool,
    pub has_data_set_ready_changed: bool,
    pub trailing_edge_ring_indicator: B1,
    pub has_data_carrier_detect_changed: bool,
    pub clear_to_send_complement: bool,
    pub data_set_ready_complement: bool,
    pub ring_indicator_complement: bool,
    pub data_carrier_detect_complement: bool,
}

#[bitpiece(3)]
#[derive(Debug, Clone, Copy)]
pub struct UartReceiveErrors {
    pub has_overrun_error_occured: bool,
    pub has_parity_error_occured: bool,
    pub has_framing_error_occured: bool,
}

#[bitpiece(2)]
#[derive(Debug, Clone, Copy)]
pub enum UartWordLength {
    /// word length of 5 bits
    L5 = 0,

    /// word length of 6 bits
    L6 = 1,

    /// word length of 7 bits
    L7 = 2,

    /// word length of 8 bits
    L8 = 3,
}

/// the trigger level of a FIFO receiver.
///
/// this value determines how many bytes must be present in the RX FIFO before the UART generates an interrupt or sets a flag
/// indicating data is ready to be read.
#[bitpiece(2)]
#[derive(Debug, Clone, Copy)]
pub enum UartFifoReceiverTriggerLevel {
    /// trigger level is 1 byte
    B1 = 0b00,

    /// trigger level is 4 bytes
    B4 = 0b01,

    /// trigger level is 8 bytes
    B8 = 0b10,

    /// trigger level is 14 bytes
    B14 = 0b11,
}

#[bitpiece(1)]
#[derive(Debug, Clone, Copy)]
pub enum UartParityMode {
    Odd = 0,
    Even = 1,
}

#[bitpiece(1)]
#[derive(Debug, Clone, Copy)]
pub enum UartInterruptStatus {
    Pending = 0,
    Clear = 1,
}
