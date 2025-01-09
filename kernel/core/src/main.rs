#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

use bitpiece::*;
use core::{panic::PanicInfo, sync::atomic::AtomicUsize};
use hal::mem::{PhysAddr, PhysMemRegion, PCI_0_IO, PCI_0_MEM};
use interrupts::{interrupts_disable, interrupts_enable, interrupts_init};
use pci::{pci_scan, PciBarKind, PciConfigRegTyped, PciFunction, PciId, PciMemBar};
use thiserror_no_std::Error;
use uart::{uart_init, uart_read_byte};
use utils::HexDisplay;

pub mod interrupts;
pub mod pci;
pub mod uart;
pub mod utils;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    interrupts_disable();
    println!("{}", info);
    loop {}
}

pub struct Piix4CorePciFunction {
    function: PciFunction,
}
impl Piix4CorePciFunction {
    pub fn new(function: PciFunction) -> Self {
        Self { function }
    }
    pub fn irqs_route(self) -> PciConfigRegTyped<Piix4IrqsRoute> {
        PciConfigRegTyped::new(self.function.config_reg(24))
    }
}

#[bitpiece(32)]
pub struct Piix4IrqsRoute {
    pub irq_a: Piix4IrqRoute,
    pub irq_b: Piix4IrqRoute,
    pub irq_c: Piix4IrqRoute,
    pub irq_d: Piix4IrqRoute,
}

#[bitpiece(8)]
pub struct Piix4IrqRoute {
    pub interrupt_routing: B4,
    pub reserved4: B3,
    pub disable_routing: bool,
}

fn probe_piix4_core(function: PciFunction) {
    // function.config_reg()
}

fn probe_pci_function(function: PciFunction) {
    match function.id() {
        PciId::PIIX4_CORE => {
            probe_piix4_core(function);
        }
        _ => {
            println!("unknown pci function id {:x?}", function.id());
        }
    }
}

/// a physical memory bump allocator, which allocates from the given physical memory region.
pub struct PhysMemBumpAllocator {
    cur_addr: AtomicUsize,
    end_addr: PhysAddr,
}
impl PhysMemBumpAllocator {
    pub const fn new(region: PhysMemRegion) -> Self {
        Self {
            cur_addr: AtomicUsize::new(region.start.0),
            end_addr: region.end,
        }
    }
    pub fn alloc(&self, size: usize) -> Result<PhysAddr, PhysMemBumpAllocatorError> {
        let allocated_addr = self
            .cur_addr
            .fetch_add(size, core::sync::atomic::Ordering::Relaxed);
        let end_addr = allocated_addr + size;
        if end_addr <= self.end_addr.0 {
            Ok(PhysAddr(allocated_addr))
        } else {
            Err(PhysMemBumpAllocatorError {
                space_requested: HexDisplay(size),
                space_left: HexDisplay(self.end_addr.0 - allocated_addr),
            })
        }
    }
}

/// an error while trying to allocate from the physical memory bump allocator.
#[derive(Debug, Error)]
#[error("requested allocation of {space_requested} bytes, but space left is only {space_left}")]
pub struct PhysMemBumpAllocatorError {
    pub space_requested: HexDisplay<usize>,
    pub space_left: HexDisplay<usize>,
}

static PCI_IO_SPACE_ALLOCATOR: PhysMemBumpAllocator = PhysMemBumpAllocator::new(PCI_0_IO);
static PCI_MEM_SPACE_ALLOCATOR: PhysMemBumpAllocator = PhysMemBumpAllocator::new(PCI_0_MEM);

fn pci_function_map_bars(function: PciFunction) {
    for bar in function.bars() {
        match bar.size() {
            Some(size) => {
                // allocate an address for the BAR and set its base address
                let allocator = match bar.kind() {
                    PciBarKind::Mem => &PCI_MEM_SPACE_ALLOCATOR,
                    PciBarKind::Io => &PCI_IO_SPACE_ALLOCATOR,
                };
                let base_addr = allocator
                    .alloc(size.get() as usize)
                    .expect("failed to allocate address space for PCI BAR");
                bar.set_address(base_addr.0 as u32);
            }
            None => {
                // BAR is not implemented, make sure that its address is 0, so that future users
                // will know that it is not implemented just by looking at its address.
                bar.set_address(0);
            }
        }
    }
}

fn pci_map_bars() {
    pci_scan(pci_function_map_bars);
}

#[no_mangle]
extern "C" fn _start() {
    uart_init();
    interrupts_init();

    pci_map_bars();
    // pci_scan(probe_pci_function);

    loop {
        let byte = uart_read_byte();
        println!("received byte: {}", byte);
    }
}
