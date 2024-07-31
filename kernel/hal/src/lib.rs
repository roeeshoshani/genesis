#![no_std]
#![feature(asm_experimental_arch)]
#![feature(asm_const)]

use bitpiece::*;
use paste::paste;

macro_rules! impl_newtype_operator_full {
    { $newtype: ty, $operator: ident, $fn_name: expr, $operand: ty, $lhs_var_name: ident, $rhs_var_name: ident, $result: expr } => {
        paste! {
            impl ::core::ops::$operator < $operand > for $newtype {
                type Output = Self;
                fn $fn_name(self, $rhs_var_name: $operand) -> Self::Output {
                    let $lhs_var_name = self.0;
                    Self($result)
                }
            }
            impl ::core::ops::[<$operator Assign>] < $operand > for $newtype {
                fn [<$fn_name _assign>](&mut self, $rhs_var_name: $operand) {
                    let $lhs_var_name = self.0;
                    self.0 = $result;
                }
            }
        }
    };
}
macro_rules! impl_newtype_binary_operator {
    { $newtype: ty, $operator: ident, $fn_name: ident, $operand: ty } => {
        impl_newtype_operator_full! {
            $newtype,
            $operator,
            $fn_name,
            $operand,
            lhs,
            rhs,
            <$operand as ::core::ops::$operator>::$fn_name(lhs, rhs)
        }
    };
}
macro_rules! impl_newtype_unary_operator {
    { $newtype: ty, $operator: ident, $fn_name: ident, $inner_ty: ty } => {
        impl ::core::ops::$operator for $newtype {
            type Output = Self;
            fn $fn_name(self) -> Self::Output {
                Self(<$inner_ty as ::core::ops::$operator>::$fn_name(self.0))
            }
        }
    };
}
macro_rules! impl_newtype_all_operators {
    { $newtype: ty, $inner_ty: ty } => {
        impl_newtype_binary_operator! { $newtype, Add, add, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Sub, sub, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Div, div, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Rem, rem, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Mul, mul, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Shr, shr, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Shl, shl, $inner_ty }
        impl_newtype_binary_operator! { $newtype, BitAnd, bitand, $inner_ty }
        impl_newtype_binary_operator! { $newtype, BitOr, bitor, $inner_ty }
        impl_newtype_binary_operator! { $newtype, BitXor, bitxor, $inner_ty }
        impl_newtype_unary_operator! { $newtype, Not, not, $inner_ty }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct PhysAddr(pub usize);
impl_newtype_all_operators! { PhysAddr, usize }
impl PhysAddr {
    /// creates a new physical address with the given value.
    pub const fn new(value: usize) -> Self {
        Self(value)
    }

    /// returns the virtual kseg0 address which maps to this physical address, if such an address even exists.
    /// if the physical address is too large to fit in kseg0, `None` is returned.
    pub const fn kseg0_addr(self) -> Option<VirtAddr> {
        KSEG0.addr_at_offset(self.0)
    }

    /// returns the virtual kseg1 address which maps to this physical address, if such an address even exists.
    /// if the physical address is too large to fit in kseg1, `None` is returned.
    pub const fn kseg1_addr(self) -> Option<VirtAddr> {
        KSEG1.addr_at_offset(self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct VirtAddr(pub usize);
impl_newtype_all_operators! { VirtAddr, usize }
impl VirtAddr {
    /// creates a new virtual address with the given value.
    pub const fn new(value: usize) -> Self {
        Self(value)
    }

    /// returns the offset of this virtual address in kseg0, if it is in kseg0.
    pub const fn kseg0_off(self) -> Option<usize> {
        KSEG0.offset_of_addr(self)
    }

    /// returns the physical address of this virtual address in kseg0, if it is in kseg0.
    pub const fn kseg0_phys_addr(self) -> Option<PhysAddr> {
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(off) = self.kseg0_off() else {
            return None;
        };
        Some(PhysAddr(off))
    }

    /// returns the offset of this virtual address in kseg1, if it is in kseg1.
    pub const fn kseg1_off(self) -> Option<usize> {
        KSEG1.offset_of_addr(self)
    }

    /// returns the physical address of this virtual address in kseg1, if it is in kseg1.
    pub const fn kseg1_phys_addr(self) -> Option<PhysAddr> {
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(off) = self.kseg1_off() else {
            return None;
        };
        Some(PhysAddr(off))
    }
}

/// a region of virtual memory.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct VirtMemRegion {
    /// the start address of the region.
    pub start: VirtAddr,

    /// the end address of the region.
    pub end: VirtAddr,
}
impl VirtMemRegion {
    /// returns the size of the region
    pub const fn size(&self) -> usize {
        self.end.0 - self.start.0
    }

    /// returns whether this memory region contains the given address
    pub const fn contains(&self, addr: VirtAddr) -> bool {
        addr.0 >= self.start.0 && addr.0 < self.end.0
    }

    /// returns the offset of the given address within this memory region, if the address is within this memory region.
    pub const fn offset_of_addr(&self, addr: VirtAddr) -> Option<usize> {
        if !self.contains(addr) {
            return None;
        }
        Some(addr.0 - self.start.0)
    }

    /// returns the address at the given offset from the start of the region if the offset is within the bounds of the region.
    pub const fn addr_at_offset(&self, offset: usize) -> Option<VirtAddr> {
        if offset >= self.size() {
            return None;
        }
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(addr) = self.start.0.checked_add(offset) else {
            return None;
        };
        Some(VirtAddr(addr))
    }
}

/// the kseg0 memory region. this is an unmapped and uncached memory region.
pub const KSEG0: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0x8000_0000),
    end: VirtAddr(0xa000_0000),
};

/// the kseg1 memory region. this is an unmapped but cached memory region.
pub const KSEG1: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0xa000_0000),
    end: VirtAddr(0xc000_0000),
};

/// the size of the kernel stack.
pub const STACK_SIZE: usize = 4 * 1024 * 1024;

/// the register group of a coprocessor 0 register.
/// this can be combines with a `select` value to get a percise register address.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Cp0RegGroup {
    Config = 16,
    Cache = 28,
}

/// the address of a coprocessor 0 register.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Cp0Reg {
    /// the register group.
    pub group: Cp0RegGroup,

    /// the select value.
    pub select: u8,
}
impl Cp0Reg {
    pub const CONFIG_0: Self = Self {
        group: Cp0RegGroup::Config,
        select: 0,
    };
    pub const CONFIG_1: Self = Self {
        group: Cp0RegGroup::Config,
        select: 1,
    };
    pub const I_TAG_LO: Self = Self {
        group: Cp0RegGroup::Cache,
        select: 0,
    };
    pub const D_TAG_LO: Self = Self {
        group: Cp0RegGroup::Cache,
        select: 2,
    };
}

/// the cache type to be used in a cache instruction.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CacheType {
    PrimaryInstruction = 0b00,
    PrimaryData = 0b01,
    Tertiary = 0b10,
    Secondary = 0b11,
}

/// the operation type to be perfomed by a cache instruction.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CacheInsnOperationType {
    /// for instruction caches, this only invalidates the cache line with the given index.
    /// for data caches, this writes back the cache line with the given index if it is dirty, and invalidates it.
    IndexWritebackInvalidate = 0b000,

    /// load the parameters (tag, data, parity bits) of the cache entry with the given index.
    /// the parameters are loaded into the corresponding coprocessor 0 registers.
    IndexLoadTag = 0b001,

    /// store the tag value in the ?TagLo coprocessor 0 register into the cache entry with the given index.
    IndexStoreTag = 0b010,
}

/// the cache insn `op` operand.
pub struct CacheInsnOp {
    /// the cache type to which the cache operation should be applied.
    pub cache_type: CacheType,

    /// the cache operation to perform.
    pub operation_type: CacheInsnOperationType,
}
impl CacheInsnOp {
    /// encodes the cache op into its opcode binary representation.
    pub const fn encode(&self) -> u8 {
        self.cache_type as u8 | ((self.operation_type as u8) << 2)
    }
}

#[macro_export]
macro_rules! read_cp0_reg {
    ($reg: expr) => {
        {
            let _: Cp0Reg = $reg;
            let reg_value: u32;
            unsafe {
                ::core::arch::asm!(
                    ".set noat",
                    "mfc0 {res}, ${group}, {select}",
                    ".set at",
                    res = out(reg) reg_value,
                    group = const { $reg.group as u32 },
                    select = const { $reg.select },
                    options(nomem, preserves_flags, nostack),
                );
            }
            reg_value
        }

    };
}

#[macro_export]
macro_rules! write_cp0_reg {
    ($reg: expr, $value: expr) => {
        {
            let _: Cp0Reg = $reg;
            unsafe {
                ::core::arch::asm!(
                    ".set noat",
                    "mtc0 {value}, ${group}, {select}",
                    ".set at",
                    value = in(reg) $value,
                    group = const { $reg.group as u32 },
                    select = const { $reg.select },
                    options(nomem, preserves_flags, nostack),
                );
            }
        }

    };
}

#[macro_export]
macro_rules! cache_insn {
    ($op: expr, $addr: expr) => {
        {
            let _ : CacheInsnOp = $op;
            let _ : VirtAddr = $addr;
            unsafe {
                ::core::arch::asm!(
                    ".set noat",
                    "cache {op}, 0({addr})",
                    ".set at",
                    op = const { $op.encode() },
                    addr = in(reg) ($addr).0,
                    options(nomem, preserves_flags, nostack),
                )
            }
        }
    };
}

pub fn read_cp0_config1() -> Cp0Config1 {
    Cp0Config1::from_bits(read_cp0_reg!(Cp0Reg::CONFIG_1))
}

pub fn read_cp0_config0() -> Cp0Config0 {
    Cp0Config0::from_bits(read_cp0_reg!(Cp0Reg::CONFIG_0))
}

/// the associativity of a cache. this is the number of ways in the cache.
#[bitpiece(3)]
#[derive(Debug, Clone, Copy)]
pub struct CacheAssociativity {
    pub raw_val: B3,
}
impl CacheAssociativity {
    pub fn value(self) -> usize {
        self.raw_val().0 as usize + 1
    }
}

/// the size in bytes of a cache line.
#[bitpiece(3)]
#[derive(Debug, Clone, Copy)]
pub struct CacheLineSize {
    pub raw_val: B3,
}
impl CacheLineSize {
    pub fn value(self) -> usize {
        2 << self.raw_val().0
    }
}

/// the number of sets (cache lines) per way.
#[bitpiece(3)]
#[derive(Debug, Clone, Copy)]
pub struct CacheSetsPerWay {
    raw_val: B3,
}
impl CacheSetsPerWay {
    pub fn value(self) -> usize {
        64 << self.raw_val().0
    }
}

/// parameters of a cache.
///
/// here is some guide for how to decipher the meaning of these parameters.
///
/// the mips spec has weird way of looking at the structure of the cache.
///
/// it looks at it as if it is first split into some number of different "way"s.
/// for example, for the 24k core, the cache is split into 4 ways.
///
/// then, each "way" is split into some number of different "set"s, and each set has an associated "index" which can be derived
/// from the virtual address being accessed.
///
/// now let's look at what happens when we try to read from the cache.
/// - first we derive the "index" from the virtual address.
/// - then we iterate over each of the "way"s in the cache.
/// - for each "way", we choose the "set" with the calculated "index".
/// - if the "set" is valid, we compare its tag with our tag, and if it matches, we return its data.
///
/// the purpose of having multiple ways is that if we access a virtual address with index I, and the cache already contains
/// an address with index I, we won't necessarily have to evict the existing cache line, instead we could find an empty cache
/// entry with the same index in a different way. so, we can have 4 simultanous entries with the same index in the cache.
///
/// i found it more intuitive to look at it as first being split into different sets, and then each set containing 4 ways,
/// but they chose to look at it the opposite way.
#[bitpiece(9)]
#[derive(Debug, Clone, Copy)]
pub struct CacheParams {
    /// the associativity of the cache. this is the number of ways in the cache.
    pub associativity: CacheAssociativity,
    /// the size in bytes of a cache line.
    pub line_size: CacheLineSize,
    /// the amount of sets (cache lines) per way.
    pub sets_per_way: CacheSetsPerWay,
}
impl CacheParams {
    /// determines whether this cache is present.
    pub fn is_cache_present(&self) -> bool {
        self.line_size().raw_val().0 != 0
    }
    /// the total amount of cache lines in the cache.
    pub fn total_lines_amount(&self) -> usize {
        self.associativity().value() * self.sets_per_way().value()
    }
}

/// the config1 register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0Config1 {
    pub is_fpu_implemented: bool,
    pub is_jtag_implemented: bool,
    pub is_mips16_implemented: bool,
    pub are_watch_registers_implemented: bool,
    pub are_performance_counter_registers_implemented: bool,
    pub is_mdmx_implemented: bool,
    pub is_coprocessor_2_present: bool,

    pub dcache_params: CacheParams,
    pub icache_params: CacheParams,

    pub tlb_last_entry_index: B6,

    pub is_config2_register_present: bool,
}

/// the config0 register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0Config0 {
    pub k0_cache_config: CacheConfig,
    pub reserved3: B4,
    pub mmu_type: MmuType,
    pub arch_revision: B3,
    pub arch_type: B2,
    pub endianness: CpuEndianness,
    pub burst_order: CpuBurstOrder,
    pub reserved17: B1,
    pub is_write_through_merging_enabled: bool,
    pub reserved19: B2,
    pub is_simple_be_bus_mode_enabled: bool,
    pub are_cor_extend_user_defined_insns_implemented: bool,
    pub is_d_side_scrathpad_ram_present: bool,
    pub is_i_side_scrathpad_ram_present: bool,
    pub kuseg_cache_config: CacheConfig,
    pub kseg2_and_kseg3_cache_config: CacheConfig,
    pub is_config1_register_present: bool,
}

/// the burst order of the cpu.
#[bitpiece(1)]
#[derive(Debug, Clone, Copy)]
pub enum CpuBurstOrder {
    Sequential = 0,
    SubBlock = 1,
}

/// the endianness of the cpu.
#[bitpiece(1)]
#[derive(Debug, Clone, Copy)]
pub enum CpuEndianness {
    Little = 0,
    Big = 1,
}

/// the mmu type of the machine.
#[bitpiece(3)]
#[derive(Debug, Clone, Copy)]
pub enum MmuType {
    Reserved0 = 0,
    Tlb = 1,
    Reserved2 = 2,
    FixedMapping = 3,
    Reserved4 = 4,
    Reserved5 = 5,
    Reserved6 = 6,
    Reserved7 = 7,
}

/// the cache configuration of a memory segment.
#[bitpiece(3)]
#[derive(Debug, Clone, Copy)]
pub enum CacheConfig {
    /// cacheable, noncoherent, write-through, no write allocate
    CacheableWriteThrough = 0,
    /// reserved
    Reserved1 = 1,
    /// uncached
    Uncached = 2,
    /// cacheable, noncoherent, write-back, write allocate
    CacheableWriteBack = 3,
    /// reserved
    Reserved4 = 4,
    /// reserved
    Reserved5 = 5,
    /// reserved
    Reserved6 = 6,
    /// uncached accelerated
    UncachedAccelerated = 7,
}
