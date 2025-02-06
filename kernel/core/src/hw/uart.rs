use core::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

use bitpiece::*;
use hal::{
    mmio::uart::*,
    sys::{Cp0Reg, Cp0RegStatus},
};

use crate::executor::async_event::AsyncEvent;

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::hw::uart::_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[macro_export]
macro_rules! dbg {
    () => {
        $crate::println!("[{}:{}:{}]", file!(), line!(), column!())
    };
    ($val:expr $(,)?) => {
        match $val {
            tmp => {
                $crate::println!("[{}:{}:{}] {} = {:#?}",
                    file!(), line!(), column!(), stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg!($val)),+,)
    };
}

/// sets the state of the divisor latch access
fn uart_set_divisor_latch_access(value: bool) {
    let mut line_control_value = UartRegs::line_control().read();
    line_control_value.set_enable_divisor_latch_access(value);
    UartRegs::line_control().write(line_control_value);
}

/// sets the uart baud rate divisor.
///
/// NOTE: the divisor latch access must be enabled before calling this function.
fn uart_set_baud_rate_divisor(divisor: u16) {
    let [divisor_lsb, divisor_msb] = divisor.to_le_bytes();

    UartRegs::divisor_latch_lsb().write(divisor_lsb);
    UartRegs::divisor_latch_msb().write(divisor_msb);
}

/// initializes the uart. this function has no pre-requisites and can be called at any point.
pub fn uart_init() {
    // disable divisor latch access so that we can access the regular registers of the uart.
    uart_set_divisor_latch_access(false);

    // for tx, disable interrupts to use polled mode, so that we can write from interrupt context
    // for rx, use interrupts so that we don't have to busy poll for input
    UartRegs::interrupt_enable().write(UartInterruptEnableReg::from_fields(
        UartInterruptEnableRegFields {
            is_received_data_available_interrupt_enabled: true,
            is_transmitter_holding_register_empty_interrupt_enabled: false,
            is_receiver_line_status_interrupt_enabled: false,
            is_modem_status_interrupt_enabled: false,
            zero: BitPiece::zeroes(),
        },
    ));

    // enable the fifo, reset the rx and tx buffers, set the receiver trigger level to 1 byte
    UartRegs::fifo_control().write(UartFifoControlReg::from_fields(UartFifoControlRegFields {
        is_fifo_enabled: true,
        receiver_fifo_reset: true,
        transmitter_fifo_reset: true,
        receiver_trigger_level: UartFifoReceiverTriggerLevel::B1,
        dma_mode_select: BitPiece::zeroes(),
        reserved: BitPiece::zeroes(),
    }));

    // set the divisor to 1 to use a baud rate equal to the clock rate
    uart_set_divisor_latch_access(true);
    uart_set_baud_rate_divisor(1);
    uart_set_divisor_latch_access(false);

    // configure the line parameters to use 8 bit words, one stop bit, and no parity
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

fn uart_set_interrupts_enabled(enabled: bool) {
    // enable the uart interrupts
    let mut status = Cp0RegStatus::read();
    status.interrupt_mask_mut().set_tty2(enabled);
    Cp0RegStatus::write(status);
}

fn uart_enable_interrupts() {
    uart_set_interrupts_enabled(true);
}

fn uart_disable_interrupts() {
    uart_set_interrupts_enabled(false);
}

/// tries to read a single byte of input from the uart. return `None` if there is not input byte currently available.
pub fn uart_try_read_byte() -> Option<u8> {
    if UartRegs::line_status().read().is_data_ready() {
        Some(UartRegs::rx().read())
    } else {
        None
    }
}

pub fn uart_read_byte() -> UartReadByte {
    UartReadByte
}

pub struct UartReadByte;
impl Future for UartReadByte {
    type Output = u8;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // listen to the uart byte available event, so that if there is no byte currently available,
        // we will get a notification once a byte is available.
        let mut listener = UART_BYTE_AVAILABLE_EVENT.listen(cx.waker().clone());

        // try sampling the hardware to see if we have a byte available
        match uart_try_read_byte() {
            Some(byte) => {
                // we already got the byte, so no need to listen for the event.
                listener.stop_listening();

                Poll::Ready(byte)
            }
            None => {
                // no byte currently available, enable uart interrupts so that we will wake up once the
                // byte is available.
                uart_enable_interrupts();

                Poll::Pending
            }
        }
    }
}

static UART_BYTE_AVAILABLE_EVENT: AsyncEvent = AsyncEvent::new();

pub fn uart_interrupt_handler() {
    UART_BYTE_AVAILABLE_EVENT.trigger();

    // disable interrupts until we read the byte.
    //
    // we do this because the UART will keep holding its interrupt line until we read the content, but we don't want to read it
    // immediately.
    uart_disable_interrupts();
}

/// writes a single byte to the uart.
fn uart_write_byte_raw(byte: u8) {
    while !UartRegs::line_status()
        .read()
        .is_transmitter_holding_register_empty()
    {}
    UartRegs::tx().write(byte)
}

/// writes a single byte to the uart.
fn uart_write_byte(byte: u8) {
    if byte == b'\n' || byte == b'\r' {
        uart_write_byte_raw(b'\r');
        uart_write_byte_raw(b'\n');
    } else {
        uart_write_byte_raw(byte);
    }
}

pub async fn uart_task() {
    loop {
        let byte = uart_read_byte().await;
        uart_write_byte(byte);
    }
}

struct UartWriter;
impl core::fmt::Write for UartWriter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for byte in s.bytes() {
            uart_write_byte(byte);
        }
        Ok(())
    }
}

#[doc(hidden)]
pub fn _print(args: core::fmt::Arguments) {
    use core::fmt::Write;
    UartWriter.write_fmt(args).unwrap();
}
