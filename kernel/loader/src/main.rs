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
use hal::*;
use loader_shared::LoaderInfoHeader;

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

    // now that the cache is initialized, make kseg0 cachable.
    make_kseg0_cachable();

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
    let kernel_dst_addr = (KSEG0.start.0 + STACK_SIZE) as *mut u8;
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

/// switch the stack from pointing to kseg1 to pointing the same physical address but in kseg0 so that it points to cachable memory.
fn make_stack_cachable() {
    let stack_cachable_offset = KSEG0.start.0.wrapping_sub(KSEG1.start.0);
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
            let flush_addr = KSEG0.start + line_index * dcache.line_size().value();
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
            let flush_addr = KSEG0.start + line_index * icache.line_size().value();
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

/// configure the cache behaviour of the kseg0 memory region.
fn make_kseg0_cachable() {
    let mut config0 = read_cp0_config0();
    config0.set_k0_cache_config(CacheConfig::CacheableWriteBack);
    write_cp0_reg!(Cp0Reg::CONFIG_0, config0.to_bits())
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
