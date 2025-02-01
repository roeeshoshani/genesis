use core::ptr::NonNull;

use hal::mem::VirtAddr;
use volatile::VolatilePtr;

use crate::{hw::pci::PciBarKind, println};

use super::pci::{pci_find, PciId};

const NIC_BAR_SIZE: usize = 0x20;

pub fn nic_init() {
    let Some(dev) = pci_find(PciId::AM79C970) else {
        println!("nic not found");
        return;
    };

    let bar = dev.bar(0).unwrap();
    let mapped_bar = bar.map_to_memory();

    // sanity
    assert_eq!(bar.kind(), PciBarKind::Io);
    assert_eq!(mapped_bar.size, NIC_BAR_SIZE);

    // enable io for this device so that we can access its io bar.
    let mut reg1 = dev.config_reg1().read();
    reg1.command_mut().set_io_enable(true);
    dev.config_reg1().write(reg1);

    let mut regs = NicRegs {
        addr: mapped_bar.addr.kseg_uncachable_addr().unwrap(),
        size: mapped_bar.size,
    };

    // perform a 32-bit write to the RDP to configure the NIC to use dword io instead of word io.
    regs.rdp().write(0);
}

pub struct NicRegs {
    addr: VirtAddr,
    size: usize,
}
impl NicRegs {
    pub fn reg(&mut self, index: usize) -> VolatilePtr<u32> {
        let offset = index * 4;

        let end_offset = offset + 4;
        assert!(end_offset <= self.size);

        let reg_addr = self.addr + offset;
        unsafe { VolatilePtr::new(NonNull::new_unchecked(reg_addr.as_mut_ptr::<u32>())) }
    }
    pub fn aprom0(&mut self) -> VolatilePtr<u32> {
        self.reg(0)
    }
    pub fn aprom1(&mut self) -> VolatilePtr<u32> {
        self.reg(1)
    }
    pub fn aprom2(&mut self) -> VolatilePtr<u32> {
        self.reg(2)
    }
    pub fn aprom3(&mut self) -> VolatilePtr<u32> {
        self.reg(3)
    }
    pub fn rdp(&mut self) -> VolatilePtr<u32> {
        self.reg(4)
    }
    pub fn rap(&mut self) -> VolatilePtr<u32> {
        self.reg(5)
    }
    pub fn reset(&mut self) -> VolatilePtr<u32> {
        self.reg(6)
    }
    pub fn bdp(&mut self) -> VolatilePtr<u32> {
        self.reg(7)
    }
}
