use core::{mem::ManuallyDrop, ptr::NonNull};

use hal::mem::{PhysAddr, PhysMemRegion, RAM_0};

use crate::sync::IrqLock;

use super::{
    align_down, align_up,
    allocator_utils::{AllocatorHdr, AllocatorHdrCursorSnapshot, AllocatorHdrList},
    kernel_end_phys, PAGE_SIZE,
};

/// the data of a free chunk header.
struct Chunk {
    pages_amount: usize,
}

// useful type definitions
type ChunkHdr = AllocatorHdr<Chunk>;
type ChunkList = AllocatorHdrList<Chunk>;
type ChunkCursorSnapshot = AllocatorHdrCursorSnapshot<Chunk>;

fn get_chunk_phys_addr(hdr: &ChunkHdr) -> PhysAddr {
    // we use the kseg cachable memory region for our virtual addressing
    hdr.virt_addr().kseg_cachable_phys_addr().unwrap()
}

fn get_chunk_phys_end_addr(hdr: &ChunkHdr) -> PhysAddr {
    get_chunk_phys_addr(hdr) + hdr.data.pages_amount * PAGE_SIZE
}

pub struct PageAllocator {
    freelist: ChunkList,
}
impl PageAllocator {
    /// creates a new empty page allocator.
    pub const fn new() -> Self {
        Self {
            freelist: ChunkList::new(),
        }
    }

    /// adds a memory region that the allocator can use when allocating
    ///
    /// # safety
    /// - the memory region must not be used by anyone else
    /// - the start and end addresses of the region must be accessible through the kseg cachable memory region
    pub unsafe fn add_region(&mut self, region: PhysMemRegion) {
        let aligned_start = PhysAddr(align_up(region.start.0, PAGE_SIZE));
        let aligned_end = PhysAddr(align_down(region.end().unwrap().0, PAGE_SIZE));

        // make sure that we have at least one page of memory space to allocate from.
        if aligned_start + PAGE_SIZE > aligned_end {
            // no space even for a single page, this memory region does not actually provide any space we can allocate from.
            return;
        }

        // make sure that both the start and end addresses can be accessed through the kseg cachable memory region.
        let _ = aligned_start.kseg_cachable_addr().unwrap();
        let _ = aligned_end.kseg_cachable_addr().unwrap();

        let pages_amount = (aligned_end.0 - aligned_start.0) / PAGE_SIZE;

        // add a chunk which represents the new region
        unsafe {
            // SAFETY: the address is aligned to a chunk header since we aligned it to a page, which is more than enough.
            //
            // additionally, this memory is currently unused, as it was provided for us to use in the allocator.
            self.add_free_chunk(aligned_start, pages_amount)
        };
    }

    /// allocate the given number of pages
    ///
    /// # safety
    /// the pages must be deallocated once you finish using them
    pub unsafe fn alloc(&mut self, pages_amount: usize) -> Option<PhysAddr> {
        let mut cursor = self.freelist.cursor();

        let mut cur_best_chunk: Option<ChosenChunk> = None;

        while let Some(chunk) = cursor.current() {
            // make sure that the chunk is big enough to satisfy this allocation
            if chunk.pages_amount >= pages_amount {
                match &cur_best_chunk {
                    Some(best_chunk) => {
                        // if the current chunk is smaller than the current best, then use it instead.
                        // we want to find the smallest chunk that can satisfy this allocation.
                        if chunk.pages_amount < best_chunk.page_size {
                            cur_best_chunk = Some(ChosenChunk {
                                page_size: chunk.pages_amount,
                                snapshot: cursor.snapshot(),
                            })
                        }
                    }
                    None => {
                        cur_best_chunk = Some(ChosenChunk {
                            page_size: chunk.pages_amount,
                            snapshot: cursor.snapshot(),
                        })
                    }
                }
            }

            // advance to the next chunk
            cursor.move_next();
        }

        // get the chosen best chunk, or if no chunk was found, fail the allocation.
        let chosen_chunk_info = cur_best_chunk?;

        // move the cursor to the chosen chunk so that we can access it.
        unsafe {
            // SAFETY: the list was not modified since we took the snapshot
            cursor.revert(chosen_chunk_info.snapshot);
        }

        // get access to the chosen chunk
        let chosen_chunk = cursor.current_view().unwrap();

        // allocate the pages from the end of the chunk by reducing its size.
        chosen_chunk.hdr.data.pages_amount -= pages_amount;

        // calculate the allocated address by finding the end of the chunk after shrinking its size.
        let allocated_addr = get_chunk_phys_end_addr(&chosen_chunk.hdr).0;

        // if the chunk is now empty, remove it from the list
        if chosen_chunk.hdr.data.pages_amount == 0 {
            chosen_chunk.unlink();
        }

        Some(PhysAddr(allocated_addr))
    }

    /// adds a new free chunk to the freelist.
    ///
    /// # safety
    /// - the address must be a valid chunk header pointer
    /// - the memory must be part of the memory region used by this allocator
    /// - the memory must not be used, and must not be part of another chunk
    unsafe fn add_free_chunk(&mut self, addr: PhysAddr, pages_amount: usize) {
        let virt_addr = addr.kseg_cachable_addr().unwrap();
        let hdr_ptr = unsafe {
            // SAFETY: kseg cachable addresses are never null.
            NonNull::new_unchecked(virt_addr.as_mut_ptr::<ChunkHdr>())
        };
        self.freelist
            .init_and_push_front(hdr_ptr, Chunk { pages_amount });
    }

    /// deallocated the given pages.
    ///
    /// # safety
    /// the paghes must have been previously returned from an allocation request to this allocator.
    pub unsafe fn dealloc(&mut self, addr: PhysAddr, mut pages_amount: usize) {
        let end_addr = addr + pages_amount * PAGE_SIZE;

        // handle the case where we have an adjacent free chunk right after the deallocated chunk
        let mut cursor = self.freelist.cursor();
        let maybe_chunk_after = cursor.find(|hdr| get_chunk_phys_addr(hdr) == end_addr);
        if let Some(chunk_after) = maybe_chunk_after {
            // in this case, what we want to do is:
            // - remove the existing chunk that is after the deallocated chunk
            // - increase the size of the deallocated chunk to include the existing chunk
            //
            // this way we can cover the entire combined region with a single chunk

            // count its pages as part of the deallocated chunk
            pages_amount += chunk_after.hdr.data.pages_amount;

            // remove it from the freelist
            chunk_after.unlink();

            // after doing this, we can continue with the regular deallocation flow, and the chunk that was after the
            // deallocated chunk will now be counted as part of it.
        }

        // now, decide how to deallocate based on whether we have a free chunk before the deallocated chunk
        let mut cursor = self.freelist.cursor();
        let maybe_chunk_before = cursor.find(|hdr| get_chunk_phys_end_addr(hdr) == addr);
        match maybe_chunk_before {
            Some(chunk_before) => {
                // we have a free chunk right before the deallocated chunk.
                //
                // so, to deallocate it, we can just increase the size of that chunk that comes before it to include the deallocated
                // chunk in it.
                chunk_before.hdr.data.pages_amount += pages_amount;
            }
            None => {
                // no adjacent free chunk before the deallocated chunk, just add it as a new chunk in the freelist.
                unsafe {
                    // SAFETY: this address is a valid header pointer since it was previously returned from our allocation,
                    // and we only return addresses that are valid header pointers.
                    //
                    // additionally, this address is no longer used because it is currently being deallocated.
                    self.add_free_chunk(addr, pages_amount);
                }
            }
        }
    }
}

unsafe impl Sync for PageAllocator {}
unsafe impl Send for PageAllocator {}

struct ChosenChunk {
    snapshot: ChunkCursorSnapshot,
    page_size: usize,
}

static PAGE_ALLOCATOR: IrqLock<PageAllocator> = IrqLock::new(PageAllocator::new());

/// initializes the page allocator.
pub fn page_allocator_init() {
    let mut allocator = PAGE_ALLOCATOR.lock();
    unsafe {
        // SAFETY: this memory region is unused
        allocator.add_region(PhysMemRegion {
            start: kernel_end_phys(),
            inclusive_end: RAM_0.inclusive_end,
        });
    }
}

pub fn alloc_pages(pages_amount: usize) -> Option<Pages> {
    let mut allocator = PAGE_ALLOCATOR.lock();
    let addr = unsafe {
        // SAFETY: the pages will be deallocated when the returned object is dropped
        allocator.alloc(pages_amount)
    }?;
    Some(Pages { addr, pages_amount })
}

pub struct PagesRaw {
    pub addr: PhysAddr,
    pub pages_amount: usize,
}

pub struct Pages {
    addr: PhysAddr,
    pages_amount: usize,
}

impl Pages {
    pub fn addr(&self) -> PhysAddr {
        self.addr
    }
    pub fn pages_amount(&self) -> usize {
        self.pages_amount
    }

    /// consumes the pages object, returning raw pages information.
    ///
    /// # safety
    /// you must make sure to properly free the pages once you finish using them.
    pub unsafe fn into_raw(self) -> PagesRaw {
        let pages = ManuallyDrop::new(self);
        PagesRaw {
            addr: pages.addr,
            pages_amount: pages.pages_amount,
        }
    }

    /// constructs a pages object from the given raw pages information.
    ///
    /// # safety
    /// the pages information must have been previously returned by converting a proper pages object into raw pages information.
    pub unsafe fn from_raw(info: PagesRaw) -> Self {
        Self {
            addr: info.addr,
            pages_amount: info.pages_amount,
        }
    }
}

impl Drop for Pages {
    fn drop(&mut self) {
        let mut allocator = PAGE_ALLOCATOR.lock();
        unsafe {
            // SAFETY: these pages were previously allocated using the same allocator.
            allocator.dealloc(self.addr, self.pages_amount)
        };
    }
}
