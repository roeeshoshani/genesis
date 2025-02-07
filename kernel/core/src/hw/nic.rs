use core::{marker::PhantomData, pin::Pin, ptr::NonNull};

use alloc::boxed::Box;
use bitpiece::*;
use hal::mem::VirtAddr;
use static_assertions::const_assert_eq;
use volatile::{
    access::{Access, ReadOnly, ReadWrite},
    VolatilePtr,
};

use crate::{
    hw::pci::{PciBarKind, PciConfigRegCommand, PciConfigRegCommandFields, PciInterruptPin},
    println,
};

use super::{
    interrupts::{with_interrupts_disabled, PCI_INTA_IRQ, PIIX4_I8259_CHAIN},
    pci::{pci_find, PciId},
};

const NIC_BAR_SIZE: usize = 0x20;
const NIC_APROM_SIZE: usize = 16;

pub const ETH_ADDR_LEN: usize = 6;

const NIC_RX_RING_ENTRIES_AMOUNT: RingEntriesAmountVal = RingEntriesAmountVal::N512;
const NIC_TX_RING_ENTRIES_AMOUNT: RingEntriesAmountVal = RingEntriesAmountVal::N512;
const NIC_RX_RING_SIZE: usize = NIC_RX_RING_ENTRIES_AMOUNT.value();
const NIC_TX_RING_SIZE: usize = NIC_TX_RING_ENTRIES_AMOUNT.value();

const NIC_LOGICAL_ADDR_FILTER_SIZE: usize = 8;

pub const ETH_MAX_MTU: usize = 1500;
pub const ETH_HDR_LEN: usize = ETH_ADDR_LEN * 2 + 2;

const NIC_BUF_SIZE: usize = ETH_MAX_MTU + ETH_HDR_LEN;

pub const NIC_ETH_ADDR: EthAddr = EthAddr([0x44; ETH_ADDR_LEN]);

pub fn nic_init() -> Option<Nic> {
    let Some(dev) = pci_find(PciId::AM79C970) else {
        println!("nic not found");
        return None;
    };
    let bar = dev.bar(0).unwrap();
    let mapped_bar = bar.map_to_memory();

    // sanity
    assert_eq!(bar.kind(), PciBarKind::Io);
    assert_eq!(mapped_bar.size, NIC_BAR_SIZE);

    let mut regs = NicRegs {
        addr: mapped_bar.addr.kseg_uncachable_addr().unwrap(),
        size: mapped_bar.size,
    };

    // configure the pci command register of the device
    dev.config_reg1().modify(|reg| {
        reg.set_command(PciConfigRegCommand::from_fields(
            PciConfigRegCommandFields {
                // enable io for this device so that we can access its io bar.
                // NOTE: this should only be done after we are finished mapping all bars.
                io_enable: true,
                // enable mmio. we don't really use it, but enable it anyway, just in case.
                mem_enable: true,
                // enable bus master support so that the device can generate interrupts and dma cycles.
                bus_master_enable: true,
                // the nic should not generate any pci special cycles
                special_cycles_enable: false,
                // allow efficient dma memory writes
                mem_write_and_invalidate_enable: true,
                // irrelevant for this device
                vga_palette_snoop_enable: false,
                // detect parity errors
                parity_error_response_enable: true,
                reserved7: BitPiece::zeroes(),
                // allow device to assert SERR. we don't really use it, but whatever.
                serr_enable: true,
                // allow fast back to back transactions for faster communication.
                fast_back_to_back_enable: true,
                // enable interrupts
                are_interrupts_disabled: false,
                reserved11: BitPiece::zeroes(),
            },
        ));
    });

    // configure the nic to use the pci INTA irq line
    dev.config_reg15().modify(|reg| {
        reg.set_interrupt_pin(PciInterruptPin::INTA);
    });

    // unmask the pci INTA irq so that we can receive interrupts from the nic
    PIIX4_I8259_CHAIN.set_irq_mask(PCI_INTA_IRQ, false);

    // perform a 32-bit write to the RDP to configure the NIC to use dword io instead of word io.
    regs.rdp().write(0);

    // set the nic's software style to the preferred style.
    // the pcnet-pci style supports 32-bit software structures and provides all features, so it is the best choice for us.
    regs.bcr20().modify(|reg| {
        reg.set_software_style(NicSoftwareStyle::PcnetPci);
    });

    // make sure that the device uses 32-bit software structures. we currently don't support the 16-bit mode.
    // we chose the pcnet-pci software style, which should support 32-bit software structures.
    assert!(regs.bcr20().read().software_size_32());

    // allocate buffers for rx descriptors
    let mut rx_bufs = RxRingBufs(core::array::from_fn(|_| {
        Box::pin(NicBuf([0; NIC_BUF_SIZE]))
    }));

    // build a ring of rx descriptors
    let mut rx_ring = Box::pin(RxRing {
        descs: core::array::from_fn(|i| {
            let buf = &mut *rx_bufs.0[i];
            let buf_virt_addr = VirtAddr(buf as *mut NicBuf as usize);
            let buf_phys_addr = buf_virt_addr.kseg_cachable_phys_addr().unwrap();
            RxDesc {
                buf_addr: buf_phys_addr.0 as u32,
                status1: RxDescStatus1::from_fields(RxDescStatus1Fields {
                    buf_len_2s_complement: BitPiece::from_bits(
                        (NIC_BUF_SIZE as u16).wrapping_neg(),
                    ),
                    ones: BitPiece::ones(),
                    reserved16: BitPiece::zeroes(),
                    end_of_packet: false,
                    start_of_packet: false,
                    buf_err: false,
                    crc_err: false,
                    overflow_err: false,
                    framing_err: false,
                    any_err: false,
                    is_owned_by_nic: true,
                }),
                status2: RxDescStatus2::zeroes(),
                reserved: BitPiece::zeroes(),
            }
        }),
    });
    let rx_ring_virt_addr = VirtAddr(&mut *rx_ring as *mut RxRing as usize);
    let rx_ring_phys_addr = rx_ring_virt_addr.kseg_cachable_phys_addr().unwrap();

    // allocate buffers for tx descriptors
    let mut tx_bufs = TxRingBufs(core::array::from_fn(|_| {
        Box::pin(NicBuf([0; NIC_BUF_SIZE]))
    }));

    // build a ring of tx descriptors
    let mut tx_ring = Box::pin(TxRing {
        descs: core::array::from_fn(|i| {
            let buf = &mut *tx_bufs.0[i];
            let buf_virt_addr = VirtAddr(buf as *mut NicBuf as usize);
            let buf_phys_addr = buf_virt_addr.kseg_cachable_phys_addr().unwrap();
            TxDesc {
                buf_addr: buf_phys_addr.0 as u32,
                status1: TxDescStatus1::from_fields(TxDescStatus1Fields {
                    buf_len_2s_complement: BitPiece::from_bits(
                        (NIC_BUF_SIZE as u16).wrapping_neg(),
                    ),
                    ones: BitPiece::ones(),
                    reserved16: BitPiece::zeroes(),
                    end_of_packet: false,
                    start_of_packet: false,
                    deferred: false,
                    only_one_retry_needed: false,
                    more_than_one_retry_needed: false,
                    // don't override the global FCS settings for this frame, use the global settings
                    no_fcs_or_add_fcs: false,
                    any_err: false,
                    // the descriptor should not currently be owned by the nic. only when sending and filling it with data we will
                    // move ownership to the nic.
                    is_owned_by_nic: false,
                }),
                status2: TxDescStatus2::zeroes(),
                reserved: BitPiece::zeroes(),
            }
        }),
    });
    let tx_ring_virt_addr = VirtAddr(&mut *tx_ring as *mut TxRing as usize);
    let tx_ring_phys_addr = tx_ring_virt_addr.kseg_cachable_phys_addr().unwrap();

    // build the initialization block
    let mut init_block = Box::pin(InitBlock {
        mode: NicMode::from_fields(NicModeFields {
            disable_rx: false,
            disable_tx: false,
            enable_loopback: false,
            disable_tx_fcs: false,
            force_collision: false,
            disable_retry: false,
            internal_loopback: false,
            port_select: NicPortSelect::S10BaseT,
            tsel_or_lrt: false,
            mendec_loopback: false,
            disable_polarity_correction: false,
            disable_link_status: false,
            disable_phys_addr_detection: false,
            disable_rx_broadcast: false,
            promisc: false,
        }),
        rx_ring_entries_amount: RingEntriesAmount::from_fields(RingEntriesAmountFields {
            reserved0: BitPiece::zeroes(),
            val: NIC_RX_RING_ENTRIES_AMOUNT,
        }),
        tx_ring_entries_amount: RingEntriesAmount::from_fields(RingEntriesAmountFields {
            reserved0: BitPiece::zeroes(),
            val: NIC_TX_RING_ENTRIES_AMOUNT,
        }),
        phys_addr: NIC_ETH_ADDR,
        reserved: BitPiece::zeroes(),
        logical_addr_filter: [0u8; NIC_LOGICAL_ADDR_FILTER_SIZE],
        rx_ring_addr: rx_ring_phys_addr.0 as u32,
        tx_ring_addr: tx_ring_phys_addr.0 as u32,
    });
    let init_block_virt_addr = VirtAddr(&mut *init_block as *mut InitBlock as usize);
    let init_block_phys_addr = init_block_virt_addr.kseg_cachable_phys_addr().unwrap();

    regs.csr1().write(NicCsr1::from_fields(NicCsr1Fields {
        init_block_addr_low: (init_block_phys_addr.0 & 0xffff) as u16,
    }));
    regs.csr2().write(NicCsr2::from_fields(NicCsr2Fields {
        init_block_addr_high: ((init_block_phys_addr.0 >> 16) & 0xffff) as u16,
    }));
    regs.csr0().write(NicCsr0::from_fields(NicCsr0Fields {
        init: true,
        start: false,
        stop: false,
        transmit_demand: false,
        tx_on: true,
        rx_on: true,
        interrupts_enabled: true,
        interrupt_pending: false,
        initialization_done: false,
        tx_interrupt: true,
        rx_interrupt: true,
        mem_err: false,
        missed_frame: false,
        collision_err: false,
        tx_timeout_err: false,
        any_err: false,
    }));

    Some(Nic {
        init_block_storage: Some(init_block),
        rx_bufs,
        tx_bufs,
        rx_ring_storage: rx_ring,
        tx_ring_storage: tx_ring,
    })
}

pub struct Nic {
    init_block_storage: Option<Pin<Box<InitBlock>>>,
    rx_bufs: RxRingBufs,
    tx_bufs: TxRingBufs,
    rx_ring_storage: Pin<Box<RxRing>>,
    tx_ring_storage: Pin<Box<TxRing>>,
}

#[repr(transparent)]
struct NicBuf([u8; NIC_BUF_SIZE]);

#[repr(transparent)]
struct RxRing {
    descs: [RxDesc; NIC_RX_RING_SIZE],
}
#[repr(transparent)]
struct TxRing {
    descs: [TxDesc; NIC_RX_RING_SIZE],
}

struct RxRingBufs([Pin<Box<NicBuf>>; NIC_RX_RING_SIZE]);
struct TxRingBufs([Pin<Box<NicBuf>>; NIC_TX_RING_SIZE]);

#[repr(transparent)]
pub struct EthAddr(pub [u8; ETH_ADDR_LEN]);

#[repr(C)]
struct InitBlock {
    pub mode: NicMode,
    pub rx_ring_entries_amount: RingEntriesAmount,
    pub tx_ring_entries_amount: RingEntriesAmount,
    pub phys_addr: EthAddr,
    pub reserved: u16,
    pub logical_addr_filter: [u8; NIC_LOGICAL_ADDR_FILTER_SIZE],
    pub rx_ring_addr: u32,
    pub tx_ring_addr: u32,
}

// the initialization block should be 7 dwords in size.
const_assert_eq!(size_of::<InitBlock>(), 7 * 4);

#[repr(C)]
struct RxDesc {
    pub buf_addr: u32,
    pub status1: RxDescStatus1,
    pub status2: RxDescStatus2,
    pub reserved: u32,
}

// the rx desc should be 4 dwords in size.
const_assert_eq!(size_of::<RxDesc>(), 4 * 4);

#[repr(C)]
struct TxDesc {
    pub buf_addr: u32,
    pub status1: TxDescStatus1,
    pub status2: TxDescStatus2,
    pub reserved: u32,
}

#[bitpiece(32)]
struct TxDescStatus1 {
    pub buf_len_2s_complement: B12,
    pub ones: B4,
    pub reserved16: u8,
    pub end_of_packet: bool,
    pub start_of_packet: bool,
    pub deferred: bool,
    pub only_one_retry_needed: bool,
    pub more_than_one_retry_needed: bool,
    pub no_fcs_or_add_fcs: bool,
    pub any_err: bool,
    pub is_owned_by_nic: bool,
}

#[bitpiece(32)]
struct TxDescStatus2 {
    pub tx_retry_count: B4,
    pub reserved4: B12,
    pub time_domain_reflectometry: B10,
    pub retry_err: bool,
    pub loss_of_carrier: bool,
    pub late_collision: bool,
    pub excessive_deferral: bool,
    pub underflow_err: bool,
    pub buf_err: bool,
}

#[bitpiece(32)]
struct RxDescStatus1 {
    pub buf_len_2s_complement: B12,
    pub ones: B4,
    pub reserved16: u8,
    pub end_of_packet: bool,
    pub start_of_packet: bool,
    pub buf_err: bool,
    pub crc_err: bool,
    pub overflow_err: bool,
    pub framing_err: bool,
    pub any_err: bool,
    pub is_owned_by_nic: bool,
}

#[bitpiece(32)]
struct RxDescStatus2 {
    pub msg_len: B12,
    pub reserved12: B4,
    pub runt_packet_count: u8,
    pub rx_collision_count: u8,
}

#[bitpiece(8)]
pub struct RingEntriesAmount {
    pub reserved0: B4,
    pub val: RingEntriesAmountVal,
}

#[bitpiece(4)]
#[derive(Debug, Clone, Copy)]
pub enum RingEntriesAmountVal {
    N1 = 0,
    N2 = 1,
    N4 = 2,
    N8 = 3,
    N16 = 4,
    N32 = 5,
    N64 = 6,
    N128 = 7,
    N256 = 8,
    N512 = 9,
    Reserved10 = 10,
    Reserved11 = 11,
    Reserved12 = 12,
    Reserved13 = 13,
    Reserved14 = 14,
    Reserved15 = 15,
}
impl RingEntriesAmountVal {
    pub const fn value(&self) -> usize {
        match self {
            RingEntriesAmountVal::N1 => 1,
            RingEntriesAmountVal::N2 => 2,
            RingEntriesAmountVal::N4 => 4,
            RingEntriesAmountVal::N8 => 8,
            RingEntriesAmountVal::N16 => 16,
            RingEntriesAmountVal::N32 => 32,
            RingEntriesAmountVal::N64 => 64,
            RingEntriesAmountVal::N128 => 128,
            RingEntriesAmountVal::N256 => 256,
            RingEntriesAmountVal::N512 => 512,
            _ => unreachable!(),
        }
    }
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
    pub fn csr<T: BitPiece<Bits = u16>>(&mut self, index: u8) -> NicCsr<T> {
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
    pub fn bcr<T: BitPiece<Bits = u16>>(&mut self, index: u8) -> NicBcr<T> {
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

#[bitpiece(16)]
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
}

#[bitpiece(16)]
pub struct NicCsr1 {
    pub init_block_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr2 {
    pub init_block_addr_high: u16,
}

#[bitpiece(16)]
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
}

#[bitpiece(16)]
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
}

#[bitpiece(16)]
pub struct NicCsr6 {
    pub reserved0: u8,
    pub rx_ring_len: B4,
    pub tx_ring_len: B4,
}

#[bitpiece(16)]
pub struct NicCsr8 {
    pub addr_filter_0_16: u16,
}

#[bitpiece(16)]
pub struct NicCsr9 {
    pub addr_filter_16_32: u16,
}

#[bitpiece(16)]
pub struct NicCsr10 {
    pub addr_filter_32_48: u16,
}

#[bitpiece(16)]
pub struct NicCsr11 {
    pub addr_filter_48_64: u16,
}

#[bitpiece(16)]
pub struct NicCsr12 {
    pub phys_addr_0_16: u16,
}

#[bitpiece(16)]
pub struct NicCsr13 {
    pub phys_addr_16_32: u16,
}

#[bitpiece(16)]
pub struct NicCsr14 {
    pub phys_addr_32_48: u16,
}

#[bitpiece(16)]
pub struct NicCsr15 {
    pub mode: NicMode,
}

#[bitpiece(16)]
pub struct NicMode {
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
}

#[bitpiece(16)]
pub struct NicCsr18 {
    pub rx_buf_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr19 {
    pub rx_buf_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr20 {
    pub tx_buf_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr21 {
    pub tx_buf_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr22 {
    pub next_rx_buf_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr23 {
    pub next_rx_buf_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr24 {
    pub rx_ring_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr25 {
    pub rx_ring_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr26 {
    pub next_rx_desc_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr27 {
    pub next_rx_desc_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr28 {
    pub rx_desc_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr29 {
    pub rx_desc_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr30 {
    pub tx_ring_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr31 {
    pub tx_ring_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr32 {
    pub next_tx_desc_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr33 {
    pub next_tx_desc_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr34 {
    pub tx_desc_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr35 {
    pub tx_desc_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr36 {
    pub next_next_rx_desc_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr37 {
    pub next_next_rx_desc_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr38 {
    pub next_next_tx_desc_addr_low: u16,
}

#[bitpiece(16)]
pub struct NicCsr39 {
    pub next_next_tx_desc_addr_high: u16,
}

#[bitpiece(16)]
pub struct NicCsr40 {
    pub rx_byte_count: B12,
    pub reserved12: B4,
}

#[bitpiece(16)]
pub struct NicCsr41 {
    pub reserved0: u8,
    pub rx_status: u8,
}

#[bitpiece(16)]
pub struct NicCsr42 {
    pub tx_byte_count: B12,
    pub reserved12: B4,
}

#[bitpiece(16)]
pub struct NicCsr43 {
    pub reserved0: u8,
    pub tx_status: u8,
}

#[bitpiece(16)]
pub struct NicCsr44 {
    pub next_rx_byte_count: B12,
    pub reserved12: B4,
}

#[bitpiece(16)]
pub struct NicCsr45 {
    pub reserved0: u8,
    pub next_rx_status: u8,
}

#[bitpiece(16)]
pub struct NicCsr46 {
    pub poll_time_counter: u16,
}

#[bitpiece(16)]
pub struct NicCsr47 {
    pub poll_interval: u16,
}

#[bitpiece(16)]
pub struct NicCsr58 {
    pub software_style: NicSoftwareStyle,
    pub reserved2: B6,
    pub software_size_32_bits: bool,
    pub csr_pcnet_isa: bool,
    pub reserved10: B6,
}

#[bitpiece(16)]
#[derive(Debug)]
pub struct NicBcr20 {
    pub software_style: NicSoftwareStyle,
    pub reserved2: B6,
    pub software_size_32: bool,
    pub csr_pcnet_isa: bool,
    pub reserved10: B6,
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

/// a bitfield for the contents of a csr.
#[bitpiece(32)]
pub struct NicCsrValue {
    /// the actual contents of the csr.
    pub content: u16,

    /// reserved bits of the csr.
    pub reserved: u16,
}

/// a bitfield for the contents of a bcr.
#[bitpiece(32)]
pub struct NicBcrValue {
    /// the actual contents of the bcr.
    pub content: u16,

    /// reserved bits of the bcr.
    pub reserved: u16,
}

/// a control status register of the NIC.
pub struct NicCsr<'a, T: BitPiece<Bits = u16>> {
    csr_index: u8,
    nic_regs: &'a mut NicRegs,
    phantom: PhantomData<T>,
}
impl<'a, T: BitPiece<Bits = u16>> NicCsr<'a, T> {
    pub fn read(&mut self) -> T {
        let csr_value = NicCsrValue::from_bits(self.nic_regs.read_csr(self.csr_index));
        T::from_bits(csr_value.content())
    }
    pub fn write(&mut self, value: T) {
        let csr_value = NicCsrValue::from_fields(NicCsrValueFields {
            content: value.to_bits(),
            reserved: BitPiece::zeroes(),
        });
        self.nic_regs.write_csr(self.csr_index, csr_value.to_bits());
    }
    pub fn modify<F>(&mut self, modify: F)
    where
        F: FnOnce(&mut T),
    {
        let mut value = self.read();
        modify(&mut value);
        self.write(value);
    }
}

/// a bus control register of the NIC.
pub struct NicBcr<'a, T: BitPiece<Bits = u16>> {
    bcr_index: u8,
    nic_regs: &'a mut NicRegs,
    phantom: PhantomData<T>,
}
impl<'a, T: BitPiece<Bits = u16>> NicBcr<'a, T> {
    pub fn read(&mut self) -> T {
        let bcr_value = NicBcrValue::from_bits(self.nic_regs.read_bcr(self.bcr_index));
        T::from_bits(bcr_value.content())
    }
    pub fn write(&mut self, value: T) {
        let bcr_value = NicBcrValue::from_fields(NicBcrValueFields {
            content: value.to_bits(),
            reserved: BitPiece::zeroes(),
        });
        self.nic_regs.write_bcr(self.bcr_index, bcr_value.to_bits());
    }
    pub fn modify<F>(&mut self, modify: F)
    where
        F: FnOnce(&mut T),
    {
        let mut value = self.read();
        modify(&mut value);
        self.write(value);
    }
}
