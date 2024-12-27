use bitpiece::*;

/// the register group of a coprocessor 0 register.
/// this can be combines with a `select` value to get a percise register address.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Cp0RegGroup {
    Status = 12,
    Cause = 13,
    Config = 16,
    Cache = 28,
}

/// the address of a coprocessor 0 register.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Cp0RegAddr {
    /// the register group.
    pub group: Cp0RegGroup,

    /// the select value.
    pub select: u8,
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

macro_rules! read_cp0_reg {
    ($reg: expr) => {
        {
            let _: Cp0RegAddr = $reg;
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

macro_rules! write_cp0_reg {
    ($reg: expr, $value: expr) => {
        {
            let _: Cp0RegAddr = $reg;
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

/// a trait which represents a coprocessor 0 register type.
/// this must be a 32-bit bitfield, since all coprocessor 0 registers are 32-bits.
pub trait Cp0Reg: BitPiece<Bits = u32> {
    /// the address of this coprocessor 0 register.
    const ADDR: Cp0RegAddr;

    /// reads the value of this coprocessor 0 register.
    fn read() -> Self {
        Self::from_bits(read_cp0_reg!(Self::ADDR))
    }

    /// writes the given value to this coprocessor 0 register.
    fn write(value: Self) {
        write_cp0_reg!(Self::ADDR, value.to_bits())
    }
}

/// the config1 register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0RegConfig1 {
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
impl Cp0Reg for Cp0RegConfig1 {
    const ADDR: Cp0RegAddr = Cp0RegAddr {
        group: Cp0RegGroup::Config,
        select: 1,
    };
}

/// the ITagLo register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0RegITagLo {
    pub value: u32,
}
impl Cp0Reg for Cp0RegITagLo {
    const ADDR: Cp0RegAddr = Cp0RegAddr {
        group: Cp0RegGroup::Cache,
        select: 0,
    };
}

/// the DTagLo register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0RegDTagLo {
    pub value: u32,
}
impl Cp0Reg for Cp0RegDTagLo {
    const ADDR: Cp0RegAddr = Cp0RegAddr {
        group: Cp0RegGroup::Cache,
        select: 2,
    };
}

/// the config0 register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0RegConfig0 {
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
impl Cp0Reg for Cp0RegConfig0 {
    const ADDR: Cp0RegAddr = Cp0RegAddr {
        group: Cp0RegGroup::Config,
        select: 0,
    };
}

/// the cause register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0RegCause {
    pub reserved0: B2,
    pub exception_code: ExceptionCode,
    pub reserved7: B1,
    pub pending_interrupts: InterruptBitmap,
    pub reserved16: B6,
    pub is_watch_exception_deferred: bool,
    pub use_special_interrupt_vector: bool,
    pub reserved24: B4,
    /// coprocessor unit number referenced when a coprocessor unusable exception is taken
    pub coprocessor_unusable_coprocessor_number: B2,
    pub reserved30: B1,
    pub was_last_exception_in_delay_slot: bool,
}
impl Cp0Reg for Cp0RegCause {
    const ADDR: Cp0RegAddr = Cp0RegAddr {
        group: Cp0RegGroup::Cause,
        select: 0,
    };
}

/// the status register of coprocessor 0
#[bitpiece(32)]
#[derive(Debug, Clone, Copy)]
pub struct Cp0RegStatus {
    pub are_interrupts_enabled: bool,
    pub exception_level: CpuExceptionLevel,
    pub error_level: CpuErrorLevel,
    pub operating_mode: OperatingMode,
    pub reserved5: B3,
    pub interrupt_mask: InterruptBitmap,
    pub reserved16: B3,
    /// indicates that the entry through the reset exception vector was due to an NMI
    pub in_nmi: bool,
    /// indicates that the entry through the reset exception vector was due to a soft reset
    pub is_soft_reset: bool,
    pub was_tlb_conflict_detected: bool,
    pub use_bootstrap_exception_vectors: bool,
    pub reserved23: B2,
    pub reverse_endianness: bool,
    pub reserved26: B1,
    pub reduced_power_mode: bool,
    pub allow_access_to_coprocessor_0: bool,
    pub allow_access_to_coprocessor_1: bool,
    pub allow_access_to_coprocessor_2: bool,
    pub allow_access_to_coprocessor_3: bool,
}
impl Cp0Reg for Cp0RegStatus {
    const ADDR: Cp0RegAddr = Cp0RegAddr {
        group: Cp0RegGroup::Status,
        select: 0,
    };
}

/// a bitmap of all interrupts. each bit is associated with one interrupt.
#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct InterruptBitmap {
    pub software: B2,
    pub hardware: B6,
}

/// a cpu exception code.
#[bitpiece(5)]
#[derive(Debug, Clone, Copy)]
pub enum ExceptionCode {
    Interrupt = 0,
    TlbModification = 1,
    TlbLoad = 2,
    TlbStore = 3,
    AddrErrorLoad = 4,
    AddrErrorStore = 5,
    InstructionBusError = 6,
    DataBusError = 7,
    Syscall = 8,
    Breakpoint = 9,
    ReservedInstruction = 10,
    CoprocessorUnusable = 11,
    ArithmeticOverflow = 12,
    Trap = 13,
    Reserved14 = 14,
    FloatingPoint = 15,
    Reserved16 = 16,
    Reserved17 = 17,
    Coprocessor2 = 18,
    Reserved19 = 19,
    Reserved20 = 20,
    Reserved21 = 21,
    Mdmx = 22,
    Watch = 23,
    MachineCheck = 24,
    Reserved25 = 25,
    Reserved26 = 26,
    Reserved27 = 27,
    Reserved28 = 28,
    Reserved29 = 29,
    Cache = 30,
    Reserved31 = 31,
}

/// the operating mode of the cpu.
#[bitpiece(2)]
#[derive(Debug, Clone, Copy)]
pub enum OperatingMode {
    KernelMode = 0,
    SupervisorMode = 1,
    UserMode = 2,
    Reserved = 3,
}

/// the exception level of the cpu.
#[bitpiece(1)]
#[derive(Debug, Clone, Copy)]
pub enum CpuExceptionLevel {
    NormalLevel = 0,
    ExceptionLevel = 1,
}

/// the error level of the cpu.
#[bitpiece(1)]
#[derive(Debug, Clone, Copy)]
pub enum CpuErrorLevel {
    NormalLevel = 0,
    ErrorLevel = 1,
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
