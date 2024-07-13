#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(asm_const)]

use core::{
    arch::{asm, global_asm},
    panic::PanicInfo,
    ptr::addr_of_mut,
};

use bitpiece::*;
use loader_shared::LoaderInfoHeader;

const KSEG0_START: usize = 0x800_0000;
const KSEG1_START: usize = 0xa00_0000;
const STACK_SIZE: usize = 4 * 1024 * 1024;

#[repr(C)]
struct EncodedRel {
    offset: usize,
    addend: isize,
}

/// the signature of the entry point of the wrapped code.
pub type Entrypoint = unsafe extern "C" fn();

extern "C" {
    static mut REFERENCE_POINT_OFFSET_FROM_END: u8;
    fn get_reference_point_addr() -> *mut u8;
}

global_asm!(include_str!("boot.S"));

/// the entrypoint of the shellcode loader.
#[no_mangle]
unsafe extern "C" fn loader_entrypoint() {
    // first, initialize the cache.
    initialize_cache();

    // now that the cache is initialized, switch the stack to use cachable memory.
    make_stack_cachable();

    let end_of_code_ptr =
        get_reference_point_addr().add(addr_of_mut!(REFERENCE_POINT_OFFSET_FROM_END) as usize);
    let mut cursor = Cursor::new(end_of_code_ptr);
    let info = cursor.align_and_extract_struct::<LoaderInfoHeader>();

    // parse the relocations
    let relocations =
        cursor.align_and_extract_slice::<EncodedRel>(info.relocations_amount as usize);

    // right after the relocation information comes the wrapped code.
    let wrapped_code_ptr = cursor.cur_ptr;

    // copy the wrapped kernel code from ROM to RAM so that we can relocate it.
    // put the kernel at a physical address which is right after the end of the stack. map it using kseg0 so that it will be cached.
    let kernel_dst_addr = (KSEG0_START + STACK_SIZE) as *mut u8;
    kernel_dst_addr.copy_from_nonoverlapping(wrapped_code_ptr, info.initialized_size as usize);

    for relocation in relocations {
        let relocated_value = &mut *kernel_dst_addr.add(relocation.offset).cast::<usize>();

        // to apply the relocation, just add the code address to the memory location.
        *relocated_value += kernel_dst_addr as usize;
    }

    // fill the uninitialized memory with zeroes.
    let uninitialized_data_ptr = kernel_dst_addr.add(info.initialized_size as usize);
    let uninitialized_data =
        core::slice::from_raw_parts_mut(uninitialized_data_ptr, info.uninitialized_size as usize);
    uninitialized_data.fill(0);

    let entrypoint: Entrypoint =
        core::mem::transmute(kernel_dst_addr.add(info.entry_point_offset as usize));
    entrypoint()
}

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

macro_rules! read_cp0_reg {
    ($reg: expr) => {
        {
            let _: Cp0Reg = $reg;
            let reg_value: u32;
            unsafe {
                asm!(
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
            let _: Cp0Reg = $reg;
            unsafe {
                asm!(
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

macro_rules! cache_insn {
    ($op: expr, $addr: expr) => {
        {
            let _ : CacheInsnOp = $op;
            unsafe {
                asm!(
                    ".set noat",
                    "cache {op}, 0({addr})",
                    ".set at",
                    op = const { $op.encode() },
                    addr = in(reg) ($addr),
                    options(nomem, preserves_flags, nostack),
                )
            }
        }
    };
}

fn read_cp0_config1() -> Cp0Config1 {
    Cp0Config1::from_bits(read_cp0_reg!(Cp0Reg::CONFIG_1))
}

/// the associativity of a cache. this is the number of ways in the cache.
#[bitpiece]
#[derive(Debug, Clone, Copy)]
pub struct CacheAssociativity {
    raw_val: B3,
}
impl CacheAssociativity {
    pub fn value(self) -> usize {
        self.raw_val().0 as usize + 1
    }
}

/// the size in bytes of a cache line.
#[bitpiece]
#[derive(Debug, Clone, Copy)]
pub struct CacheLineSize {
    raw_val: B3,
}
impl CacheLineSize {
    pub fn value(self) -> usize {
        2 << self.raw_val().0
    }
}

/// the number of sets (cache lines) per way.
#[bitpiece]
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
#[bitpiece]
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
#[bitpiece]
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
// #[bitfield(u32)]
// pub struct Cp0Config0 {}

/// switch the stack from pointing to kseg1 to pointing the same physical address but in kseg0 so that it points to cachable memory.
fn make_stack_cachable() {
    let stack_cachable_offset = KSEG0_START.wrapping_sub(KSEG1_START);
    unsafe {
        asm!(
            ".set noat",
            "addu $sp, {offset}",
            ".set at",
            offset = in(reg) stack_cachable_offset,
            options(nomem, preserves_flags, nostack)
        );
    }
}

fn initialize_cache() {
    let config1 = read_cp0_config1();
    let dcache = config1.dcache_params();
    if dcache.is_cache_present() {
        // fill the DTagLo register with zero bits. this will make the "valid" bit zero, which will allow us to invalidate
        // all cache entries.
        write_cp0_reg!(Cp0Reg::D_TAG_LO, 0);

        // write the zeroed out tag to all dcache entries
        for line_index in 0..dcache.total_lines_amount() {
            let flush_addr = KSEG0_START + line_index * dcache.line_size().value();
            cache_insn!(
                CacheInsnOp {
                    cache_type: CacheType::PrimaryData,
                    operation_type: CacheInsnOperationType::IndexStoreTag
                },
                flush_addr
            );
        }
    }

    let icache = config1.icache_params();
    if icache.is_cache_present() {
        // fill the ITagLo register with zero bits. this will make the "valid" bit zero, which will allow us to invalidate
        // all cache entries.
        write_cp0_reg!(Cp0Reg::I_TAG_LO, 0);

        // write the zeroed out tag to all icache entries
        for line_index in 0..icache.total_lines_amount() {
            let flush_addr = KSEG0_START + line_index * icache.line_size().value();
            cache_insn!(
                CacheInsnOp {
                    cache_type: CacheType::PrimaryInstruction,
                    operation_type: CacheInsnOperationType::IndexStoreTag
                },
                flush_addr
            );
        }
    }
}

global_asm!(
    ".globl get_reference_point_addr",
    "get_reference_point_addr:",
    // save the old return address
    "move $t0, $ra",
    // jump and link to the reference point, so that `ra` will contain the address of that label
    "bal reference_point",
    // define the reference point
    ".globl reference_point",
    "reference_point:",
    // put the address of the reference point, which is now in `ra`, as the return value
    "move $v0, $ra",
    // return to the caller
    "jr $t0",
);

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

struct Cursor {
    cur_ptr: *mut u8,
}
impl Cursor {
    fn new(ptr: *mut u8) -> Self {
        Self { cur_ptr: ptr }
    }
    unsafe fn advance(&mut self, amount: usize) {
        self.cur_ptr = self.cur_ptr.add(amount);
    }
    unsafe fn advance_struct<T>(&mut self) {
        self.advance(core::mem::size_of::<T>())
    }
    unsafe fn align_and_extract_struct<T>(&mut self) -> &'static T {
        self.align::<T>();
        self.extract_struct()
    }
    unsafe fn extract_struct<T>(&mut self) -> &'static T {
        let result = &*self.cur_ptr.cast();
        self.advance_struct::<T>();
        result
    }
    unsafe fn advance_slice<T>(&mut self, len: usize) {
        self.advance(core::mem::size_of::<T>() * len)
    }
    unsafe fn align_and_extract_slice<T>(&mut self, len: usize) -> &'static [T] {
        self.align::<T>();
        self.extract_slice(len)
    }
    unsafe fn extract_slice<T>(&mut self, len: usize) -> &'static [T] {
        let result = core::slice::from_raw_parts(self.cur_ptr.cast(), len);
        self.advance_slice::<T>(len);
        result
    }
    unsafe fn align<T>(&mut self) {
        self.cur_ptr = self
            .cur_ptr
            .add(self.cur_ptr.align_offset(core::mem::align_of::<T>()))
    }
}
