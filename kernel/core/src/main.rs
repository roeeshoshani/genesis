#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::panic::PanicInfo;
use hal::mmio::gt64120::Gt64120Regs;
use interrupts::{
    interrupts_disable, interrupts_enable, interrupts_init, with_interrupts_disabled,
};
use uart::{uart_init, uart_read_byte};

pub mod interrupts;
pub mod uart;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    interrupts_disable();
    println!("{}", info);
    loop {}
}

#[bitpiece(32)]
struct PciConfigRegAddrEncoded {
    pub zero: B2,
    pub register_number: B6,
    pub function: B3,
    pub device: B5,
    pub bus: B8,
    pub reserved: B7,
    pub enable: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PciConfigRegAddr {
    pub bus: u8,
    pub device: u8,
    pub function: u8,
    pub register_number: u8,
}
impl PciConfigRegAddr {
    fn encode(self) -> PciConfigRegAddrEncoded {
        // verify the field values
        assert_eq!(self.device >> 5, 0);
        assert_eq!(self.function >> 3, 0);
        assert_eq!(self.register_number >> 6, 0);

        PciConfigRegAddrEncoded::from_fields(PciConfigRegAddrEncodedFields {
            zero: BitPiece::zeroes(),
            register_number: B6(self.register_number),
            function: B3(self.function),
            device: B5(self.device),
            bus: B8(self.bus),
            reserved: BitPiece::zeroes(),
            enable: true,
        })
    }
}

pub struct PciConfigReg {
    encoded_addr: PciConfigRegAddrEncoded,
}
impl PciConfigReg {
    pub fn new(addr: PciConfigRegAddr) -> Self {
        Self {
            encoded_addr: addr.encode(),
        }
    }
    pub fn read(&self) -> u32 {
        // do this with interrupts disabled to prevent an interrupt handler from overwriting the pci config register while
        // we are operating
        with_interrupts_disabled!({
            Gt64120Regs::pci_0_config_addr().write(self.encoded_addr.to_bits());
            Gt64120Regs::pci_0_config_data().read()
        })
    }
    pub fn write(&self, value: u32) {
        // do this with interrupts disabled to prevent an interrupt handler from overwriting the pci config register while
        // we are operating
        with_interrupts_disabled!({
            Gt64120Regs::pci_0_config_addr().write(self.encoded_addr.to_bits());
            Gt64120Regs::pci_0_config_data().write(value);
        })
    }
}

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    interrupts_init();

    let reg = PciConfigReg::new(PciConfigRegAddr {
        bus: 0,
        device: 0,
        function: 0,
        register_number: 0,
    });
    println!("{:x}", reg.read());

    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
        if byte == b'Z' {
            interrupts_enable();
        }
    }
}
