use core::{marker::PhantomData, ptr::NonNull};

use bitpiece::*;
use hal::mem::VirtAddr;
use volatile::{
    access::{Access, ReadOnly, ReadWrite},
    VolatilePtr,
};

use crate::{hw::pci::PciBarKind, println};

use super::{
    interrupts::with_interrupts_disabled,
    pci::{pci_find, PciId},
};

const NIC_BAR_SIZE: usize = 0x20;
const NIC_APROM_SIZE: usize = 16;

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

    // make sure that the device uses 32-bit software structures. we currently don't support the 16-bit mode.
    assert!(regs.bcr20().read().software_size_32());
}

pub struct NicRegs {
    addr: VirtAddr,
    size: usize,
}
impl NicRegs {
    /// returns a pointer to the register at the given offset.
    ///
    /// # safety
    /// this function takes an immutable reference to `self`, but if the returned pointer can be written to, the caller should
    /// guarantee exclusivity.
    pub unsafe fn reg<A: Access>(&self, offset: usize, access: A) -> VolatilePtr<u32, A> {
        assert!(offset % 4 == 0);

        let end_offset = offset + 4;
        assert!(end_offset <= self.size);

        let reg_addr = self.addr + offset;
        unsafe {
            VolatilePtr::new_restricted(
                access,
                NonNull::new_unchecked(reg_addr.as_mut_ptr::<u32>()),
            )
        }
    }
    pub fn reg_rw(&mut self, offset: usize) -> VolatilePtr<u32, ReadWrite> {
        unsafe {
            // SAFETY: we take a mutable reference to self which guarantees exclusivity.
            self.reg(offset, ReadWrite)
        }
    }
    pub fn reg_ro(&self, offset: usize) -> VolatilePtr<u32, ReadOnly> {
        unsafe {
            // SAFETY: the pointer is not writable
            self.reg(offset, ReadOnly)
        }
    }
    pub fn aprom0(&mut self) -> VolatilePtr<u32, ReadOnly> {
        self.reg_ro(0x00)
    }
    pub fn aprom1(&mut self) -> VolatilePtr<u32, ReadOnly> {
        self.reg_ro(0x04)
    }
    pub fn aprom2(&mut self) -> VolatilePtr<u32, ReadOnly> {
        self.reg_ro(0x08)
    }
    pub fn aprom3(&mut self) -> VolatilePtr<u32, ReadOnly> {
        self.reg_ro(0x0c)
    }
    pub fn read_aprom(&mut self) -> [u8; NIC_APROM_SIZE] {
        let mut result = [0u8; NIC_APROM_SIZE];
        for offset in (0..NIC_APROM_SIZE).step_by(4) {
            let reg_value = self.reg_ro(offset).read();
            result[offset..offset + 4].copy_from_slice(reg_value.to_ne_bytes().as_slice());
        }
        result
    }
    pub fn rdp(&mut self) -> VolatilePtr<u32, ReadWrite> {
        self.reg_rw(0x10)
    }
    pub fn rap(&mut self) -> VolatilePtr<u32, ReadWrite> {
        self.reg_rw(0x14)
    }
    pub fn reset(&mut self) -> VolatilePtr<u32, ReadWrite> {
        self.reg_rw(0x18)
    }
    pub fn bdp(&mut self) -> VolatilePtr<u32, ReadWrite> {
        self.reg_rw(0x1c)
    }
    pub fn csr<T: BitPiece<Bits = u32>>(&mut self, index: u8) -> NicCsr<T> {
        NicCsr {
            csr_index: index,
            nic_regs: self,
            phantom: PhantomData,
        }
    }
    pub fn read_csr(&mut self, index: u8) -> u32 {
        // do this with interrupts disabled to prevent an interrupt handler from overwriting the rap register while
        // we are operating
        with_interrupts_disabled! {
            self.rap().write(index as u32);
            self.rdp().read()
        }
    }
    pub fn write_csr(&mut self, index: u8, value: u32) {
        // do this with interrupts disabled to prevent an interrupt handler from overwriting the rap register while
        // we are operating
        with_interrupts_disabled! {
            self.rap().write(index as u32);
            self.rdp().write(value)
        }
    }
    pub fn bcr<T: BitPiece<Bits = u32>>(&mut self, index: u8) -> NicBcr<T> {
        NicBcr {
            bcr_index: index,
            nic_regs: self,
            phantom: PhantomData,
        }
    }
    pub fn read_bcr(&mut self, index: u8) -> u32 {
        // do this with interrupts disabled to prevent an interrupt handler from overwriting the rap register while
        // we are operating
        with_interrupts_disabled! {
            self.rap().write(index as u32);
            self.bdp().read()
        }
    }
    pub fn write_bcr(&mut self, index: u8, value: u32) {
        // do this with interrupts disabled to prevent an interrupt handler from overwriting the rap register while
        // we are operating
        with_interrupts_disabled! {
            self.rap().write(index as u32);
            self.bdp().write(value)
        }
    }
    pub fn csr0(&mut self) -> NicCsr<NicCsr0> {
        self.csr(0)
    }
    pub fn csr1(&mut self) -> NicCsr<NicCsr1> {
        self.csr(1)
    }
    pub fn csr2(&mut self) -> NicCsr<NicCsr2> {
        self.csr(2)
    }
    pub fn csr3(&mut self) -> NicCsr<NicCsr3> {
        self.csr(3)
    }
    pub fn csr4(&mut self) -> NicCsr<NicCsr4> {
        self.csr(4)
    }
    pub fn csr6(&mut self) -> NicCsr<NicCsr6> {
        self.csr(6)
    }
    pub fn csr8(&mut self) -> NicCsr<NicCsr8> {
        self.csr(8)
    }
    pub fn bcr20(&mut self) -> NicBcr<NicBcr20> {
        self.bcr(20)
    }
}

#[bitpiece(32)]
pub struct NicCsr0 {
    pub init: bool,
    pub start: bool,
    pub stop: bool,
    pub transmit_demand: bool,
    pub tx_on: bool,
    pub rx_on: bool,
    pub interrupts_enabled: bool,
    pub interrupt_pending: bool,
    pub initialization_done: bool,
    pub tx_interrupt: bool,
    pub rx_interrupt: bool,
    pub mem_err: bool,
    pub missed_frame: bool,
    pub collision_err: bool,
    pub tx_timeout_err: bool,
    pub any_err: bool,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr1 {
    pub init_block_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr2 {
    pub init_block_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr3 {
    pub reserved0: B2,
    pub endianness: NicEndianness,
    pub enable_modified_backoff: bool,
    pub disable_tx_2_part_deferral: bool,
    pub lookahead_processing_enable: bool,
    pub reserved6: B2,
    pub init_done_interrupt_mask: bool,
    pub tx_interrupt_mask: bool,
    pub rx_interrupt_mask: bool,
    pub mem_err_interrupt_mask: bool,
    pub missed_frame_interrupt_mask: bool,
    pub reserved13: B1,
    pub tx_timeout_err_interrupt_mask: bool,
    pub reserved15: B1,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr4 {
    pub jabber_err_interrupt_mask: bool,
    pub jabber_err: bool,
    pub tx_start_interrupt_mask: bool,
    pub tx_start: bool,
    pub rx_collision_overflow_interrupt_mask: bool,
    pub rx_collision_overflow: bool,
    pub reserved6: B2,
    pub missed_frame_overflow_interrupt_mask: bool,
    pub missed_frame_overflow: bool,
    pub auto_strip_rx: bool,
    pub auto_pad_tx: bool,
    pub disable_tx_polling: bool,
    pub timer_enable: bool,
    pub dma_plus: bool,
    pub enable_test_mode: bool,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr6 {
    pub reserved0: u8,
    pub rx_ring_len: B4,
    pub tx_ring_len: B4,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr8 {
    pub addr_filter_0_16: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr9 {
    pub addr_filter_16_32: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr10 {
    pub addr_filter_32_48: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr11 {
    pub addr_filter_48_64: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr12 {
    pub phys_addr_0_16: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr13 {
    pub phys_addr_16_32: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr14 {
    pub phys_addr_32_48: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr15 {
    pub disable_rx: bool,
    pub disable_tx: bool,
    pub enable_loopback: bool,
    pub disable_tx_fcs: bool,
    pub force_collision: bool,
    pub disable_retry: bool,
    pub internal_loopback: bool,
    pub port_select: NicPortSelect,
    pub tsel_or_lrt: bool,
    pub mendec_loopback: bool,
    pub disable_polarity_correction: bool,
    pub disable_link_status: bool,
    pub disable_phys_addr_detection: bool,
    pub disable_rx_broadcast: bool,
    pub promisc: bool,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr18 {
    pub rx_buf_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr19 {
    pub rx_buf_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr20 {
    pub tx_buf_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr21 {
    pub tx_buf_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr22 {
    pub next_rx_buf_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr23 {
    pub next_rx_buf_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr24 {
    pub rx_ring_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr25 {
    pub rx_ring_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr26 {
    pub next_rx_desc_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr27 {
    pub next_rx_desc_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr28 {
    pub rx_desc_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr29 {
    pub rx_desc_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr30 {
    pub tx_ring_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr31 {
    pub tx_ring_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr32 {
    pub next_tx_desc_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr33 {
    pub next_tx_desc_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr34 {
    pub tx_desc_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr35 {
    pub tx_desc_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr36 {
    pub next_next_rx_desc_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr37 {
    pub next_next_rx_desc_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr38 {
    pub next_next_tx_desc_addr_low: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr39 {
    pub next_next_tx_desc_addr_high: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr40 {
    pub rx_byte_count: B12,
    pub reserved12: B4,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr41 {
    pub reserved0: u8,
    pub rx_status: u8,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr42 {
    pub tx_byte_count: B12,
    pub reserved12: B4,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr43 {
    pub reserved0: u8,
    pub tx_status: u8,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr44 {
    pub next_rx_byte_count: B12,
    pub reserved12: B4,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr45 {
    pub reserved0: u8,
    pub next_rx_status: u8,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr46 {
    pub poll_time_counter: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr47 {
    pub poll_interval: u16,
    pub reserved16: u16,
}

#[bitpiece(32)]
pub struct NicCsr58 {
    pub software_style: NicSoftwareStyle,
    pub reserved2: B6,
    pub software_size_32_bits: bool,
    pub csr_pcnet_isa: bool,
    pub reserved10: B6,
    pub reserved16: u16,
}

#[bitpiece(32)]
#[derive(Debug)]
pub struct NicBcr20 {
    pub software_style: NicSoftwareStyle,
    pub reserved2: B6,
    pub software_size_32: bool,
    pub csr_pcnet_isa: bool,
    pub reserved10: B6,
    pub reserved16: u16,
}

#[bitpiece(2)]
#[derive(Debug, Clone, Copy)]
pub enum NicSoftwareStyle {
    Lance = 0,
    Ilacc = 1,
    PcnetPci = 2,
    Reserved3 = 3,
}

#[bitpiece(2)]
#[derive(Debug, Clone, Copy)]
pub enum NicPortSelect {
    Aui = 0,
    S10BaseT = 1,
    Reserved2 = 2,
    Reserved3 = 3,
}

#[bitpiece(1)]
#[derive(Debug, Clone, Copy)]
pub enum NicEndianness {
    Little = 0,
    Big = 1,
}

/// a control status register of the NIC.
pub struct NicCsr<'a, T: BitPiece<Bits = u32>> {
    csr_index: u8,
    nic_regs: &'a mut NicRegs,
    phantom: PhantomData<T>,
}
impl<'a, T: BitPiece<Bits = u32>> NicCsr<'a, T> {
    pub fn read(&mut self) -> T {
        T::from_bits(self.nic_regs.read_csr(self.csr_index))
    }
    pub fn write(&mut self, value: T) {
        self.nic_regs.write_csr(self.csr_index, value.to_bits());
    }
}

/// a bus control register of the NIC.
pub struct NicBcr<'a, T: BitPiece<Bits = u32>> {
    bcr_index: u8,
    nic_regs: &'a mut NicRegs,
    phantom: PhantomData<T>,
}
impl<'a, T: BitPiece<Bits = u32>> NicBcr<'a, T> {
    pub fn read(&mut self) -> T {
        T::from_bits(self.nic_regs.read_bcr(self.bcr_index))
    }
    pub fn write(&mut self, value: T) {
        self.nic_regs.write_bcr(self.bcr_index, value.to_bits());
    }
}
