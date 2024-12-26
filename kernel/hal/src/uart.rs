use core::ptr::NonNull;

use bitpiece::*;
use volatile::{
    access::{ReadOnly, ReadWrite, WriteOnly},
    VolatilePtr,
};

use crate::{PhysAddr, VirtAddr};

/// a struct representing the UART hardware registers, providing access to them.
pub struct UartRegs;
impl UartRegs {
    /// the base physical address of the UART registers.
    pub const BASE_PHYS_ADDR: PhysAddr = PhysAddr(0x1F00_0900);

    /// the base virtual address of the UART registers.
    /// that virtual address in in kseg1 so that it is not cachable, which is important for mmio addresses.
    pub const BASE_VIRT_ADDR: VirtAddr = UartRegs::BASE_PHYS_ADDR.kseg1_addr().unwrap();

    /// the RX register.
    pub fn rx() -> VolatilePtr<'static, u8, ReadOnly> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadOnly,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x0).as_mut()),
            )
        }
    }

    /// the TX register.
    pub fn tx() -> VolatilePtr<'static, u8, WriteOnly> {
        unsafe {
            VolatilePtr::new_restricted(
                WriteOnly,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x0).as_mut()),
            )
        }
    }

    /// the INTEN (interrupt enable) register.
    pub fn interrupt_enable() -> VolatilePtr<'static, UartInterruptEnableReg, ReadWrite> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadWrite,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x8).as_mut()),
            )
        }
    }

    /// the II (interrup identification) register.
    pub fn interrupt_id() -> VolatilePtr<'static, UartInterruptIdReg, ReadOnly> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadOnly,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x10).as_mut()),
            )
        }
    }

    /// the FIFO (fifo control) register.
    pub fn fifo_control() -> VolatilePtr<'static, UartFifoControlReg, WriteOnly> {
        unsafe {
            VolatilePtr::new_restricted(
                WriteOnly,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x10).as_mut()),
            )
        }
    }

    /// the LCTRL (line control) register.
    pub fn line_control() -> VolatilePtr<'static, UartLineControlReg, ReadWrite> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadWrite,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x18).as_mut()),
            )
        }
    }

    /// the MCTRL (modem control) register.
    pub fn modem_control() -> VolatilePtr<'static, UartModemControlReg, ReadWrite> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadWrite,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x20).as_mut()),
            )
        }
    }

    /// the LSTAT (line status) register.
    pub fn line_status() -> VolatilePtr<'static, UartLineStatusReg, ReadWrite> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadWrite,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x28).as_mut()),
            )
        }
    }

    /// the MSTAT (modem status) register.
    pub fn modem_status() -> VolatilePtr<'static, UartModemStatusReg, ReadWrite> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadWrite,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x30).as_mut()),
            )
        }
    }

    /// the SCRATCH register.
    pub fn scratch() -> VolatilePtr<'static, u8, ReadWrite> {
        unsafe {
            VolatilePtr::new_restricted(
                ReadWrite,
                NonNull::new_unchecked((Self::BASE_VIRT_ADDR + 0x38).as_mut()),
            )
        }
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
