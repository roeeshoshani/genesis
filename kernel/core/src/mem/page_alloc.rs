use core::{marker::PhantomData, ptr::NonNull};

use hal::mem::{PhysAddr, PhysMemRegion, VirtAddr, RAM_0};

use crate::sync::IrqSpinlock;

use super::{align_down, align_up, kernel_end_phys, PAGE_SIZE};

pub struct PageAllocator {
    freelist: ChunkLink,
}
impl PageAllocator {
    /// creates a new empty page allocator.
    pub const fn new() -> Self {
        Self { freelist: None }
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

    fn cursor(&mut self) -> ChunkCursor {
        ChunkCursor {
            prev_chunk_link: &mut self.freelist,
        }
    }

    /// allocate the given number of pages
    ///
    /// # safety
    /// the pages must be deallocated once you finish using them
    pub unsafe fn alloc(&mut self, pages_amount: usize) -> Option<PhysAddr> {
        let mut cursor = self.cursor();

        let mut cur_best_chunk: Option<ChosenChunk<'_>> = None;

        loop {
            let Some(chunk_mut) = cursor.current() else {
                // finished iterating
                break;
            };

            // make sure that the chunk is big enough to satisfy this allocation
            if chunk_mut.chunk.pages_amount >= pages_amount {
                match &cur_best_chunk {
                    Some(best_chunk) => {
                        // if the current chunk is smaller than the current best, then use it instead.
                        // we want to find the smallest chunk that can satisfy this allocation.
                        if chunk_mut.chunk.pages_amount < best_chunk.page_size {
                            cur_best_chunk = Some(ChosenChunk {
                                page_size: chunk_mut.chunk.pages_amount,
                                snapshot: cursor.snapshot(),
                            })
                        }
                    }
                    None => {
                        cur_best_chunk = Some(ChosenChunk {
                            page_size: chunk_mut.chunk.pages_amount,
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
        cursor.revert(chosen_chunk_info.snapshot);

        // get access to the chosen chunk
        let chosen_chunk = cursor.current().unwrap();

        // sanity - make sure that the chosen chunk is large enough to satisfy the allocation.
        assert!(chosen_chunk.chunk.pages_amount >= pages_amount);

        // allocate the pages from the end of the chunk by reducing its size.
        chosen_chunk.chunk.pages_amount -= pages_amount;

        // calculate the allocated address by finding the end of the chunk after shrinking its size.
        let allocated_addr =
            chosen_chunk.chunk_phys_addr().0 + chosen_chunk.chunk.pages_amount * PAGE_SIZE;

        Some(PhysAddr(allocated_addr))
    }

    /// adds a new free chunk to the freelist.
    ///
    /// # safety
    /// - the address must be aligned to a chunk header
    /// - the memory must be part of the memory region used by this allocator
    /// - the memory must not be used, and must not be part of another chunk
    unsafe fn add_free_chunk(&mut self, addr: PhysAddr, pages_amount: usize) {
        let new_chunk = unsafe {
            // SAFETY: this chunk must have been previously allocated by us, and all pointers that we return from allocation
            // are valid chunk header pointers.
            addr.kseg_cachable_addr().unwrap().as_mut::<ChunkHdr>()
        };
        unsafe {
            // SAFETY: this is a valid mutable reference so we can write to it
            core::ptr::write_volatile(
                new_chunk,
                ChunkHdr {
                    link: self.freelist,
                    pages_amount,
                },
            );
        }
        self.freelist = Some(unsafe {
            // SAFETY: it is a reference, so it is not null
            NonNull::new_unchecked(new_chunk)
        });
    }

    pub unsafe fn dealloc(&mut self, addr: PhysAddr, mut pages_amount: usize) {
        let end_addr = addr + pages_amount * PAGE_SIZE;

        let mut cursor = self.cursor();

        // iterate over existing chunks and find chunks that are adjacnet to this deallocated chunk, so that we can merge them.
        let mut snapshot_of_chunk_before = None;
        let mut snapshot_of_chunk_after = None;
        loop {
            let Some(chunk_mut) = cursor.current() else {
                // finished iterating
                break;
            };

            if chunk_mut.chunk_phys_end_addr() == addr {
                // if this free chunk comes right before the deallocated chunk, we can merge the deallocated chunk into it.
                // take a cursor snapshot of this chunk
                snapshot_of_chunk_before = Some(cursor.snapshot());
            } else if chunk_mut.chunk_phys_addr() == end_addr {
                // if this free chunk comes right after the deallocated chunk, we can merge it into the deallocated chunk.
                // take a cursor snapshot of this chunk
                snapshot_of_chunk_after = Some(cursor.snapshot());
            }

            // advance to the next chunk
            cursor.move_next();
        }

        // handle the case where we have an adjacent free chunk right after the deallocated chunk
        if let Some(snapshot_of_chunk_after) = snapshot_of_chunk_after {
            // in this case, what we want to do is:
            // - remove the existing chunk that is after the deallocated chunk
            // - increase the size of the deallocated chunk to include the existing chunk
            //
            // this way we can cover the entire combined region with a single chunk

            // get access to the chunk we want to remove
            cursor.revert(snapshot_of_chunk_after);
            let mut chunk_after = cursor.current().unwrap();

            // count its pages as part of the deallocated chunk
            pages_amount += chunk_after.chunk.pages_amount;

            // remove it from the freelist
            chunk_after.unlink();

            // after doing this, we can continue with the regular deallocation flow, and the chunk that was after the
            // deallocated chunk will now be counted as part of it.
        }

        // now, decode how to deallocate based on whether we have a free chunk before the deallocated chunk
        match snapshot_of_chunk_before {
            Some(snapshot_of_chunk_before) => {
                // we have a free chunk right before the deallocated chunk.
                //
                // so, to deallocate it, we can just increase the size of that chunk that comes before it to include the deallocated
                // chunk in it.

                // get access to the chunk before the deallocated chunk
                cursor.revert(snapshot_of_chunk_before);
                let chunk_before = cursor.current().unwrap();

                // increase the size of the chunk to include the deallocated chunk.
                chunk_before.chunk.pages_amount += pages_amount;
            }
            None => {
                // no adjacent free chunk before the deallocated chunk, just add it as a new chunk in the freelist.
                unsafe {
                    // SAFETY: this address is aligned since it was previously returned from our allocation, and we only return
                    // addresses that are valid chunk header pointers.
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

struct ChosenChunk<'a> {
    snapshot: ChunkCursorSnapshot<'a>,
    page_size: usize,
}

/// a mutable reference to a free chunk in the freelist.
struct ChunkMut<'a> {
    /// the chunk
    chunk: &'a mut ChunkHdr,

    /// a pointer to the link of the previous chunk.
    /// this is used to remove the chunk from the linked list of free chunks.
    prev_chunk_link: &'a mut ChunkLink,
}
impl<'a> ChunkMut<'a> {
    fn unlink(&mut self) {
        *self.prev_chunk_link = self.chunk.link;
    }

    fn chunk_virt_addr(&self) -> VirtAddr {
        VirtAddr(self.chunk as *const _ as usize)
    }

    fn chunk_phys_addr(&self) -> PhysAddr {
        // when writing the chunk headers we use the kseg cachable memory region, so we can find the phys addr by just
        // calculating our offset in that region.
        self.chunk_virt_addr().kseg_cachable_phys_addr().unwrap()
    }

    fn chunk_phys_end_addr(&self) -> PhysAddr {
        self.chunk_phys_addr() + self.chunk.pages_amount * PAGE_SIZE
    }
}

struct ChunkCursorSnapshot<'a> {
    prev_chunk_link_ptr: NonNull<ChunkLink>,
    phantom: PhantomData<&'a ChunkLink>,
}

/// an iterator over free chunks.
struct ChunkCursor<'a> {
    /// a pointer to the link of the previous chunk.
    prev_chunk_link: &'a mut ChunkLink,
}
impl<'a> ChunkCursor<'a> {
    fn current<'s>(&'s mut self) -> Option<ChunkMut<'s>> {
        let link = *self.prev_chunk_link;

        let mut chunk_ptr = link?;

        Some(ChunkMut {
            chunk: unsafe {
                // SAFETY: we only insert valid pointers into the freelist, so this pointer is valid
                // also, there is no aliasing, since the only other mutable ref that currently exists is a pointer to the previous chunk.
                chunk_ptr.as_mut()
            },
            prev_chunk_link: self.prev_chunk_link,
        })
    }

    fn snapshot(&mut self) -> ChunkCursorSnapshot<'a> {
        ChunkCursorSnapshot {
            prev_chunk_link_ptr: unsafe {
                // SAFETY: a valid reference is not null
                NonNull::new_unchecked(self.prev_chunk_link)
            },
            phantom: PhantomData,
        }
    }

    fn revert(&mut self, mut snapshot: ChunkCursorSnapshot<'a>) {
        self.prev_chunk_link = unsafe {
            // SAFETY: the snapshot's pointer was constructed from a valid reference, so we can cast it back to a reference.
            // as for aliasing, this pointer will be the only existing mutable pointer into the freelist, so we are fine.
            snapshot.prev_chunk_link_ptr.as_mut()
        };
    }

    /// advance to the next node.
    fn move_next(&mut self) {
        let link = *self.prev_chunk_link;

        let Some(mut chunk_ptr) = link else {
            // we already reached the end of the list, do nothing
            return;
        };

        // move the prev chunk link pointer to the next chunk
        self.prev_chunk_link = {
            let chunk = unsafe {
                // SAFETY: we only insert valid pointers into the freelist, so this pointer is valid
                // also, there is no aliasing, since the only other mutable ref that currently exists is a pointer to the previous chunk.
                chunk_ptr.as_mut()
            };
            &mut chunk.link
        };
    }
}

/// the header of a free chunk.
#[repr(C)]
struct ChunkHdr {
    link: ChunkLink,
    pages_amount: usize,
}

/// the link of a chunk, which is a pointer to the next chunk in the linked list of free chunks.
type ChunkLink = Option<NonNull<ChunkHdr>>;

static PAGE_ALLOCATOR: IrqSpinlock<PageAllocator> = IrqSpinlock::new(PageAllocator::new());

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

pub struct Pages {
    addr: PhysAddr,
    pages_amount: usize,
}

impl Pages {
    pub fn addr(&self) -> PhysAddr {
        self.addr
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
