use core::ptr::addr_of_mut;

use hal::mem::{PhysAddr, VirtAddr, KERNEL_CORE_ADDR};

pub mod allocator_utils;
pub mod page_alloc;
pub mod phys_alloc;

extern "C" {
    /// a pointer to the end of the kernel.
    static mut END_OF_CODE: u8;
}

/// returns the virtual end address of the kernel
pub fn kernel_end_virt() -> VirtAddr {
    VirtAddr(addr_of_mut!(END_OF_CODE) as usize)
}

/// returns the physical end address of the kernel
pub fn kernel_end_phys() -> PhysAddr {
    // the kernel is running in the kseg cachable memory region, based on that we can calculate its physical address
    kernel_end_virt().kseg_cachable_phys_addr().unwrap()
}

/// returns the size in bytes that the core kernel takes up in memory.
pub fn kernel_size() -> usize {
    // calculate the diff between the end of code and the kernel start to get the size
    kernel_end_phys().0 - KERNEL_CORE_ADDR.0
}

/// the size of a single page.
///
/// the mips memory management implementation allows the user to configure a different page size per tlb entry, but we completely
/// ignore this feature for the sake of simplicity, and instead just decide on a fixed page size.
///
/// the chosen value is the minimal possible page size. it was chosen so that fragmentation is less of a problem.
pub const PAGE_SIZE: usize = 4096;

/// aligns down the given value to the given alignment
pub const fn align_down(value: usize, align: usize) -> usize {
    (value / align) * align
}

/// aligns up the given value to the given alignment
pub const fn align_up(value: usize, align: usize) -> usize {
    align_down(value + align - 1, align)
}

/// returns whether the given value is aligned to the given alignment
pub const fn is_aligned(value: usize, align: usize) -> bool {
    (value % align) == 0
}

/// aligns down the given value to the given alignment
///
/// # safety
/// the alignment must be a power of 2
pub const unsafe fn align_down_p2(value: usize, align: usize) -> usize {
    value & (!(align - 1))
}

/// aligns up the given value to the given alignment
///
/// # safety
/// the alignment must be a power of 2
pub const unsafe fn align_up_p2(value: usize, align: usize) -> usize {
    (value.wrapping_sub(1) | (align - 1)).wrapping_add(1)
}

/// returns whether the given value is aligned to the given alignment
///
/// # safety
/// the alignment must be a power of 2
pub const unsafe fn is_aligned_p2(value: usize, align: usize) -> bool {
    (value & (align - 1)) == 0
}
