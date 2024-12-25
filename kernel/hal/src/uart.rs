use bitpiece::*;

use crate::{PhysAddr, VirtAddr};

/// a struct representing the UART hardware registers, providing access to them.
pub struct UartRegs;
impl UartRegs {
    /// the base physical address of the UART registers.
    pub const BASE_PHYS_ADDR: PhysAddr = PhysAddr(0x1F00_0900);

    /// the base virtual address of the UART registers.
    /// that virtual address in in kseg1 so that it is not cachable, which is important for mmio addresses.
    pub const BASE_VIRT_ADDR: VirtAddr = UartRegs::BASE_PHYS_ADDR.kseg1_addr().unwrap();

    /// the RXTX register.
    pub fn rxtx() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x0).as_mut() }
    }

    /// the INTEN register.
    pub fn inten() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x8).as_mut() }
    }

    /// the IIFOFO register.
    pub fn iififo() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x10).as_mut() }
    }

    /// the LCTRL register.
    pub fn lctrl() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x18).as_mut() }
    }

    /// the MCTRL register.
    pub fn mctrl() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x20).as_mut() }
    }

    /// the LSTAT register.
    pub fn lstat() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x28).as_mut() }
    }

    /// the MSTAT register.
    pub fn mstat() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x30).as_mut() }
    }

    /// the SCRATCH register.
    pub fn scratch() -> &'static mut u8 {
        unsafe { (Self::BASE_VIRT_ADDR + 0x38).as_mut() }
    }
}

/// the UART interrupt enable register. this is a read-write register.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
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
    pub receiver_fifo_reset: B1,
    pub transmitter_fifo_reset: B1,
    pub dma_mode_select: B1,
    pub reserved: B2,
    pub receiver_trigger_lsb: B1,
    pub receiver_trigger_msb: B1,
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
    pub has_any_error_in_fifo_mode: bool,
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
