use bitpiece::*;
use hal::mmio::uart::*;

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

pub fn uart_init() {
    // disable divisor latch access so that we can access the regular registers of the uart.
    uart_set_divisor_latch_access(false);

    // disable interrupts to use polled mode
    UartRegs::interrupt_enable().write(UartInterruptEnableReg::from_fields(
        UartInterruptEnableRegFields {
            is_received_data_available_interrupt_enabled: false,
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

    // configure the line parameters to use 8 bit words, one stip bit, and not parity
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

pub fn uart_read_byte() -> u8 {
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

struct UartWriter;
impl core::fmt::Write for UartWriter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for byte in s.bytes() {
            if byte == b'\n' {
                uart_write_byte(b'\r');
                uart_write_byte(b'\n');
            } else {
                uart_write_byte(byte);
            }
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::uart::_print(format_args!($($arg)*)));
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

#[doc(hidden)]
pub fn _print(args: core::fmt::Arguments) {
    use core::fmt::Write;
    UartWriter.write_fmt(args).unwrap();
}
