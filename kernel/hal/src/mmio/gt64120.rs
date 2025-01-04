use crate::{
    mem::PhysAddr,
    mmio_regs::{mmio_base, mmio_reg},
};

use bitpiece::*;

/// a struct representing the GT64120 hardware registers, providing access to them.
pub struct Gt64120Regs;
impl Gt64120Regs {
    mmio_base!(PhysAddr(0x1400_0000));

    mmio_reg! {
        cpu_interface_config, u32, ReadWrite, 0x0
    }
    mmio_reg! {
        scs_1_0_low_decode, u32, ReadWrite, 0x8
    }
    mmio_reg! {
        scs_1_0_high_decode, u32, ReadWrite, 0x10
    }
    mmio_reg! {
        scs_3_2_low_decode, u32, ReadWrite, 0x18
    }
    mmio_reg! {
        scs_3_2_high_decode, u32, ReadWrite, 0x20
    }
    mmio_reg! {
        cs_2_0_low_decode, u32, ReadWrite, 0x28
    }
    mmio_reg! {
        cs_2_0_high_decode, u32, ReadWrite, 0x30
    }
    mmio_reg! {
        cs_3_and_boot_low_decode, u32, ReadWrite, 0x38
    }
    mmio_reg! {
        cs_3_and_boot_high_decode, u32, ReadWrite, 0x40
    }
    mmio_reg! {
        pci_0_io_low_decode, u32, ReadWrite, 0x48
    }
    mmio_reg! {
        pci_0_io_high_decode, u32, ReadWrite, 0x50
    }
    mmio_reg! {
        pci_0_mem_0_low_decode, u32, ReadWrite, 0x58
    }
    mmio_reg! {
        pci_0_mem_0_high_decode, u32, ReadWrite, 0x60
    }
    mmio_reg! {
        pci_0_mem_1_low_decode, u32, ReadWrite, 0x80
    }
    mmio_reg! {
        pci_0_mem_1_high_decode, u32, ReadWrite, 0x88
    }
    mmio_reg! {
        pci_1_io_low_decode, u32, ReadWrite, 0x90
    }
    mmio_reg! {
        pci_1_io_high_decode, u32, ReadWrite, 0x98
    }
    mmio_reg! {
        pci_1_mem_0_low_decode, u32, ReadWrite, 0xa0
    }
    mmio_reg! {
        pci_1_mem_0_high_decode, u32, ReadWrite, 0xa8
    }
    mmio_reg! {
        pci_1_mem_1_low_decode, u32, ReadWrite, 0xb0
    }
    mmio_reg! {
        pci_1_mem_1_high_decode, u32, ReadWrite, 0xb8
    }
    mmio_reg! {
        internal_space_decode, u32, ReadWrite, 0x68
    }
    mmio_reg! {
        scs_1_0_addr_remap, u32, ReadWrite, 0xd0
    }
    mmio_reg! {
        scs_3_2_addr_remap, u32, ReadWrite, 0xd8
    }
    mmio_reg! {
        cs_2_0_addr_remap, u32, ReadWrite, 0xe0
    }
    mmio_reg! {
        cs_3_and_boot_addr_remap, u32, ReadWrite, 0xe8
    }
    mmio_reg! {
        pci_0_io_addr_remap, u32, ReadWrite, 0xf0
    }
    mmio_reg! {
        pci_0_mem_0_addr_remap, u32, ReadWrite, 0xf8
    }
    mmio_reg! {
        pci_0_mem_1_addr_remap, u32, ReadWrite, 0x100
    }
    mmio_reg! {
        pci_1_io_addr_remap, u32, ReadWrite, 0x108
    }
    mmio_reg! {
        pci_1_mem_0_addr_remap, u32, ReadWrite, 0x110
    }
    mmio_reg! {
        pci_1_mem_1_addr_remap, u32, ReadWrite, 0x118
    }
    mmio_reg! {
        cpu_err_addr_low, u32, ReadWrite, 0x70
    }
    mmio_reg! {
        cpu_err_addr_high, u32, ReadWrite, 0x78
    }
    mmio_reg! {
        cpu_err_data_low, u32, ReadWrite, 0x128
    }
    mmio_reg! {
        cpu_err_data_high, u32, ReadWrite, 0x130
    }
    mmio_reg! {
        cpu_err_parity, u32, ReadWrite, 0x138
    }
    mmio_reg! {
        pci_0_sync, u32, ReadWrite, 0x0c0
    }
    mmio_reg! {
        pci_1_sync, u32, ReadWrite, 0x0c8
    }
    mmio_reg! {
        scs_0_low_decode, u32, ReadWrite, 0x400
    }
    mmio_reg! {
        scs_0_high_decode, u32, ReadWrite, 0x404
    }
    mmio_reg! {
        scs_1_low_decode, u32, ReadWrite, 0x408
    }
    mmio_reg! {
        scs_1_high_decode, u32, ReadWrite, 0x40c
    }
    mmio_reg! {
        scs_2_low_decode, u32, ReadWrite, 0x410
    }
    mmio_reg! {
        scs_2_high_decode, u32, ReadWrite, 0x414
    }
    mmio_reg! {
        scs_3_low_decode, u32, ReadWrite, 0x418
    }
    mmio_reg! {
        scs_3_high_decode, u32, ReadWrite, 0x41c
    }
    mmio_reg! {
        cs_0_low_decode, u32, ReadWrite, 0x420
    }
    mmio_reg! {
        cs_0_high_decode, u32, ReadWrite, 0x424
    }
    mmio_reg! {
        cs_1_low_decode, u32, ReadWrite, 0x428
    }
    mmio_reg! {
        cs_1_high_decode, u32, ReadWrite, 0x42c
    }
    mmio_reg! {
        cs_2_low_decode, u32, ReadWrite, 0x430
    }
    mmio_reg! {
        cs_2_high_decode, u32, ReadWrite, 0x434
    }
    mmio_reg! {
        cs_3_low_decode, u32, ReadWrite, 0x438
    }
    mmio_reg! {
        cs_3_high_decode, u32, ReadWrite, 0x43c
    }
    mmio_reg! {
        boot_low_decode, u32, ReadWrite, 0x440
    }
    mmio_reg! {
        boot_high_decode, u32, ReadWrite, 0x444
    }
    mmio_reg! {
        addr_decode_err, u32, ReadWrite, 0x470
    }
    mmio_reg! {
        sdram_config, u32, ReadWrite, 0x448
    }
    mmio_reg! {
        sdram_operation_mode, u32, ReadWrite, 0x474
    }
    mmio_reg! {
        sdram_burst_mode, u32, ReadWrite, 0x478
    }
    mmio_reg! {
        sdram_addr_decode, u32, ReadWrite, 0x47c
    }
    mmio_reg! {
        sdram_bank0_params, SdramBankParams, ReadWrite, 0x44c
    }
    mmio_reg! {
        sdram_bank1_params, SdramBankParams, ReadWrite, 0x450
    }
    mmio_reg! {
        sdram_bank2_params, SdramBankParams, ReadWrite, 0x454
    }
    mmio_reg! {
        sdram_bank3_params, SdramBankParams, ReadWrite, 0x458
    }
    mmio_reg! {
        device_bank0_params, u32, ReadWrite, 0x45c
    }
    mmio_reg! {
        device_bank1_params, u32, ReadWrite, 0x460
    }
    mmio_reg! {
        device_bank2_params, u32, ReadWrite, 0x464
    }
    mmio_reg! {
        device_bank3_params, u32, ReadWrite, 0x468
    }
    mmio_reg! {
        device_boot_bank_params, u32, ReadWrite, 0x46c
    }
    mmio_reg! {
        pci_0_cmd, u32, ReadWrite, 0xc00
    }
    mmio_reg! {
        pci_0_timeout, u32, ReadWrite, 0xc04
    }
    mmio_reg! {
        pci_1_cmd, u32, ReadWrite, 0xc80
    }
    mmio_reg! {
        pci_1_timeout, u32, ReadWrite, 0xc84
    }
    mmio_reg! {
        pci_0_config_addr, u32, ReadWrite, 0xcf8
    }
    mmio_reg! {
        pci_0_config_data, u32, ReadWrite, 0xcfc
    }
    mmio_reg! {
        pci_0_interrupt_ack, u32, ReadWrite, 0xc34
    }
    mmio_reg! {
        pci_1_config_addr, u32, ReadWrite, 0xcf0
    }
    mmio_reg! {
        pci_1_config_data, u32, ReadWrite, 0xcf4
    }
    mmio_reg! {
        pci_1_interrupt_ack, u32, ReadWrite, 0xc30
    }
    mmio_reg! {
        interrupt_cause, u32, ReadWrite, 0xc18
    }
    mmio_reg! {
        high_interrupt_cause, u32, ReadWrite, 0xc98
    }
    mmio_reg! {
        interrupt_mask, u32, ReadWrite, 0xc1c
    }
    mmio_reg! {
        high_interrupt_mask, u32, ReadWrite, 0xc9c
    }
    mmio_reg! {
        pci_0_interrupt_cause_mask, u32, ReadWrite, 0xc24
    }
    mmio_reg! {
        pci_0_high_interrupt_cause_mask, u32, ReadWrite, 0xca4
    }
    mmio_reg! {
        pci_0_serr_mask, u32, ReadWrite, 0xc28
    }
    mmio_reg! {
        pci_1_serr_mask, u32, ReadWrite, 0xca8
    }
    mmio_reg! {
        select_cause, u32, ReadWrite, 0xc70
    }
    mmio_reg! {
        pci_0_interrupt_select_cause, u32, ReadWrite, 0xc74
    }
}

#[bitpiece(32)]
pub struct SdramBankParams {
    pub cas_latency: B2,
    pub is_flow_through_enabled: bool,
    pub sras_precharge_time: B1,
    pub reserved4: B1,
    pub interleave: B1,
    pub is_64_bit: bool,
    pub location: B1,
    pub supports_ecc: bool,
    pub bypass: bool,
    pub sras_to_scas_delay: B1,
    pub size0: B1,
    pub reserved12: B1,
    pub burst_len: B1,
    pub size1: B1,
    pub reserved: B17,
}
