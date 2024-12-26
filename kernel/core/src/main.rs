#![no_std]
#![no_main]

use bitpiece::*;
use core::panic::PanicInfo;
use hal::uart::*;

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

fn uart_set_divisor_latch_access(value: bool) {
    let mut line_control_value = UartRegs::line_control().read();
    line_control_value.set_enable_divisor_latch_access(value);
    UartRegs::line_control().write(line_control_value);
}

fn uart_set_baud_rate_divisor(divisor: u16) {
    let [divisor_lsb, divisor_msb] = divisor.to_le_bytes();

    uart_set_divisor_latch_access(true);
    UartRegs::divisor_latch_lsb().write(divisor_lsb);
    UartRegs::divisor_latch_msb().write(divisor_msb);
    uart_set_divisor_latch_access(false);
}

fn uart_init() {
    // disable interrupts to use polled mode
    UartRegs::interrupt_enable().write(UartInterruptEnableReg::from_fields(
        UartInterruptEnableRegFields {
            is_received_data_available_interrupt_enabled: false,
            is_transmitter_holding_register_empty_interrupt_enabled: false,
            is_receiver_line_status_interrupt_enabled: false,
            is_modem_status_interrupt_enabled: false,
            zero: BitPiece::zeroed(),
        },
    ));

    // enable the fifo, reset the rx and tx buffers, set the receiver trigger level to 1 byte
    UartRegs::fifo_control().write(UartFifoControlReg::from_fields(UartFifoControlRegFields {
        is_fifo_enabled: true,
        receiver_fifo_reset: true,
        transmitter_fifo_reset: true,
        receiver_trigger_level: UartFifoReceiverTriggerLevel::B1,
        dma_mode_select: BitPiece::zeroed(),
        reserved: BitPiece::zeroed(),
    }));

    // set the divisor to 1 to achieve a baud rate of 115200
    uart_set_baud_rate_divisor(1);

    // configure the line parameters
    UartRegs::line_control().write(UartLineControlReg::from_fields(UartLineControlRegFields {
        word_length: UartWordLength::L8,
        use_extra_stop_bits: false,
        is_parity_enabled: false,
        parity_mode: UartParityMode::Odd,
        use_sticky_parity: false,
        enable_break_condition: false,
        enable_divisor_latch_access: false,
    }));
}

fn uart_read_byte() -> u8 {
    while !UartRegs::line_status().read().is_data_ready() {}
    UartRegs::rx().read()
}

fn uart_write_byte(byte: u8) {
    while !UartRegs::line_status()
        .read()
        .is_transmitter_holding_register_empty()
    {}
    UartRegs::tx().write(byte)
}
#[no_mangle]
extern "C" fn _start() {
    uart_init();
    loop {
        let byte = uart_read_byte();
        uart_write_byte(byte);
    }
}
