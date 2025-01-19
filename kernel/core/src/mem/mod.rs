use core::{
    marker::PhantomData,
    ptr::{addr_of_mut, NonNull},
};

use hal::mem::{PhysAddr, PhysMemRegion, VirtAddr, KERNEL_CORE_ADDR, RAM_0};

use crate::sync::IrqSpinlock;

pub mod allocator_utils;
pub mod page_alloc;

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

pub const fn align_down(value: usize, align: usize) -> usize {
    (value / align) * align
}

pub const fn align_up(value: usize, align: usize) -> usize {
    align_down(value + align - 1, align)
}

pub const fn is_aligned(value: usize, align: usize) -> bool {
    (value % align) == 0
}
