use core::{
    alloc::{AllocError, Allocator, GlobalAlloc, Layout},
    ptr::{null_mut, NonNull},
};

use hal::mem::{PhysAddr, VirtAddr};

use crate::{
    mem::{is_aligned, page_alloc::alloc_pages},
    sync::IrqSpinlock,
};

use super::{
    align_up, align_up_p2,
    allocator_utils::{AllocatorHdr, AllocatorHdrCursorSnapshot, AllocatorHdrList},
    PAGE_SIZE,
};

/// the data of a free chunk header.
struct Chunk {
    size: usize,
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
    get_chunk_phys_addr(hdr) + hdr.data.size
}

/// the maximum allowed size of an allocation, in pages
pub const MAX_ALLOCATION_SIZE_PAGES: usize = 4;

/// the maximum allowed size of an allocation.
pub const MAX_ALLOCATION_SIZE: usize = PAGE_SIZE * MAX_ALLOCATION_SIZE_PAGES;

/// the maximum allowed alignment of an allocation.
pub const MAX_ALLOCATION_ALIGN: usize = PAGE_SIZE;

pub struct RawPhysAllocator {
    freelist: ChunkList,
}
impl RawPhysAllocator {
    /// creates a new empty physical memory allocator.
    pub const fn new() -> Self {
        Self {
            freelist: ChunkList::new(),
        }
    }
    fn allocate_from_freelist(&mut self, layout: core::alloc::Layout) -> Option<PhysAddr> {
        let mut cursor = self.freelist.cursor();

        let mut cur_best_chunk: Option<ChosenChunk> = None;

        while let Some(chunk) = cursor.current_hdr() {
            let chunk_phys_addr = get_chunk_phys_addr(chunk);
            // align the chunk's address to the desired alignment
            let aligned_addr = unsafe {
                // SAFETY: the alignment value of a layout object must be a power of 2.
                PhysAddr(align_up_p2(chunk_phys_addr.0, layout.align()))
            };

            // calculate the amount of padding required from the start of the chunk for the allocation to be properly aligned.
            let alignment_padding = aligned_addr.0 - chunk_phys_addr.0;

            // make sure that the chunk is big enough to satisfy this allocation.
            // take into account the padding created due to alignment.
            if chunk.data.size >= alignment_padding + layout.size() {
                match &cur_best_chunk {
                    Some(best_chunk) => {
                        // check if the current chunk is a better fit for this allocation.
                        let is_chunk_better = if chunk.data.size < best_chunk.size {
                            // if the current chunk is smaller, it is a better fit
                            true
                        } else if chunk.data.size == best_chunk.size {
                            // if both chunks are the same size, but the current chunk requires less padding, it is a better fit
                            alignment_padding < best_chunk.alignment_padding
                        } else {
                            // the current chunk is smaller, it is **not** a better fit
                            false
                        };

                        // if the chunk was decided to be better, use it instead
                        if is_chunk_better {
                            cur_best_chunk = Some(ChosenChunk {
                                size: chunk.data.size,
                                snapshot: cursor.snapshot(),
                                alignment_padding,
                            })
                        }
                    }
                    None => {
                        // no current best chunk, use the new chunk as the initial best chunk
                        cur_best_chunk = Some(ChosenChunk {
                            size: chunk.data.size,
                            snapshot: cursor.snapshot(),
                            alignment_padding,
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

        // save the original size of the chunk because we modify it later
        let orig_size = chosen_chunk.hdr.data.size;

        // find the physical start address of the chunk
        let chunk_phys_addr = get_chunk_phys_addr(&chosen_chunk.hdr);

        // align up the address of the chunk to get the allocated address
        let allocated_addr = unsafe {
            // SAFETY: the alignment value of a layout object must be a power of 2.
            PhysAddr(align_up_p2(chunk_phys_addr.0, layout.align()))
        };

        // check if we have any padding at the start of the chunk.
        // if so, then we need to keep the chunk header, and sets its size to the size of the padding.
        let padding = allocated_addr.0 - chunk_phys_addr.0;
        if padding != 0 {
            // make sure that the padding is large enough to hold a chunk header.
            //
            // this should always be true since the alignment of every chunk must be aligned to the size of a chunk header, and
            // the alignment of each allocation request must also be aligned to that size, because we align it at the start of
            // the allocation.
            //
            // thus, the padding size must be a multiple of the chunk header size.
            //
            // and, since the padding is non-zero, it must be large enough to hold an entire chunk header.
            assert!(padding >= size_of::<ChunkHdr>());

            // set the size of the chunk to the size of the padding, so that it covers the entire padding area.
            chosen_chunk.hdr.data.size = padding;
        } else {
            // no padding at the start of the chunk. remove this chunk, since this memory is now allocated.
            chosen_chunk.unlink();
        }

        // calculate the size of the postfix. the postfix is the part of memory that is left at the end of the chunk after allocating.
        let postfix_size = orig_size - padding - layout.size();

        // check if we have any postfix padding at the end of the chunk.
        // if so, then we need to create a new chunk for it.
        if postfix_size != 0 {
            // make sure that the postfix padding is large enough to hold a chunk header.
            //
            // this should always be true since the original chunk size, the padding, and the requested allocation size, are all aligned
            // to a chunk header size.
            //
            // so, the postfix padding size must also be aligned to it.
            //
            // and, since the postfix padding amount is non-zero, it must be large enough to hold an entire chunk header.
            debug_assert!(postfix_size >= size_of::<ChunkHdr>());

            // calculate the address of the postfix.
            let postfix_addr = allocated_addr + layout.size();

            // make sure that the address of the postfix is aligned to the size of a chunk header.
            //
            // note that this check also makes sure that the address is aligned to the alignment of a chunk header.
            //
            // this should always be true since the chunk's original address, the padding size, and the requested allocation size, are
            // all aligned to a chunk header size.
            //
            // so, this address must also be aligned to it.
            debug_assert!(is_aligned(postfix_addr.0, size_of::<ChunkHdr>()));

            unsafe {
                // SAFETY:
                // as previously explained, the postfix padding is large enough to hold a chunk header, and the postfix
                // address is properly aligned for it.
                //
                // additionally, this memory is no longer used since we shrinked the original chunk to only cover the padding
                // at the start of the chunk.
                //
                // moreover, this memory was part of an existing chunk, so it must be accessible through the kseg cachable memory
                // segment.
                self.add_free_chunk(postfix_addr, postfix_size);
            };
        }

        Some(allocated_addr)
    }

    /// fix the layout of an allocation request so that it matches the layout expectations of this allocator.
    fn fix_layout(layout: Layout) -> Layout {
        // we align the size of the allocation request to the size of a chunk header.
        //
        // this is required because if this allocation is satisfied using a chunk which is properly aligned, and is larger
        // than the requested size, we want to make sure that the leftover bytes at the end of the chunk are large enough to hold
        // another chunk header, so that we don't lose that leftover space.
        //
        // by making sure that the size of all allocation requests is a multiple of the chunk header size, and by making sure that
        // the sizes of all chunks is also a multiple of that value, we guarantee that the leftover bytes amount is also itself a
        // multiple of the chunk header size.
        //
        // this means that the leftover bytes amount is either zero bytes, or a multiple of the chunk header size, which means that
        // we can fit a chunk header there.
        //
        // additionally, this is required to make sure that the leftover bytes at the end of a chunk start at an address that is
        // aligned to a chunk header size. this makes sure that we can properly store a chunk header header there.
        let size = align_up(layout.size(), size_of::<ChunkHdr>());

        // we align the alignment of the allocation request to the size of a chunk header.
        //
        // this is required because if this allocation is satisfied using a chunk which is large enough, but is not properly aligned,
        // we want to make sure that the padding that is left over at the start of the chunk is large enough to hold a chunk header,
        // so that we don't lose that leftover space.
        //
        // by making sure that the alignment of all allocation requests is a multiple of the chunk header size, and by making sure that
        // the alignment of all chunks is also a multiple of that value, we guarantee that the amount of padding bytes is also itself
        // a multiple of the chunk header size.
        //
        // this means that the padding bytes amount is either zero bytes, or a multiple of the chunk header size, which means that
        // we can fit a chunk header there.
        let align = align_up(layout.align(), size_of::<ChunkHdr>());

        Layout::from_size_align(size, align).unwrap()
    }

    /// allocates physical memory according to the given layout.
    ///
    /// # safety
    /// the memory must be deallocated once you finish using it.
    pub unsafe fn allocate(&mut self, layout: Layout) -> Option<PhysAddr> {
        // fix the layout according to our layout requirements
        let layout = Self::fix_layout(layout);

        // verify the size of the allocation.
        if layout.size() > MAX_ALLOCATION_SIZE {
            // we could just fail the allocation here, but panicking makes more sense in this case, since this allocator is meant
            // for allocating relatively small objects.
            panic!("allocation size {} is too big", layout.size());
        }

        // verify the alignment of the allocation.
        if layout.align() > MAX_ALLOCATION_ALIGN {
            // we could just fail the allocation here, but panicking makes more sense in this case, since this is a very unreasonable
            // memory allocation request.
            panic!("allocation alignment {} is too big", layout.align());
        }

        // first, try allocating from the freelist.
        if let Some(addr) = self.allocate_from_freelist(layout) {
            return Some(addr);
        }

        // not enough space in our freelist, allocate new pages and add to the freelist. hopefully these new pages would be able to
        // satisfy this allocation.
        //
        // if the allocation of the pages fails, fail the allocation.
        let pages = alloc_pages(MAX_ALLOCATION_SIZE_PAGES)?;

        // leak the pages for now. we will free them once this memory is no longer used by our allocator.
        let raw_pages = unsafe {
            // SAFETY: we will free those pages once we finish using them
            pages.into_raw()
        };

        // create a new chunk to represent the region covered by those pages
        unsafe {
            // SAFETY:
            // the address is page aligned, which should be enough of an alignment to store our chunk header.
            //
            // additionally, the memory region is composed of multiple pages, which is large enough to store our chunk header.
            //
            // moreover, it is unused memory since we just allocated it from the page allocator.
            //
            // furthermore, this region is accessible through the kseg cachable memory segment, since the page allocator only
            // returns pages that are accessible through that region.
            self.add_free_chunk(raw_pages.addr, MAX_ALLOCATION_SIZE);
        };

        // after adding that extra memory to the freelist, try once again to allocate from it, this time with no other fallback
        // option.
        self.allocate_from_freelist(layout)
    }

    /// adds a new free chunk to the freelist.
    ///
    /// # safety
    /// - the address must be a valid chunk header pointer
    /// - the memory must not be used, and must not be part of another chunk
    /// - the memory must be accessible through the kseg cachable memory segment
    unsafe fn add_free_chunk(&mut self, addr: PhysAddr, size: usize) {
        let virt_addr = addr.kseg_cachable_addr().unwrap();
        let hdr_ptr = unsafe {
            // SAFETY: kseg cachable addresses are never null.
            NonNull::new_unchecked(virt_addr.as_mut_ptr::<ChunkHdr>())
        };
        self.freelist.init_and_push_front(hdr_ptr, Chunk { size });
    }

    /// deallocated the given memory.
    ///
    /// # safety
    /// the memory must have been previously returned from an allocation request to this allocator.
    pub unsafe fn dealloc(&mut self, addr: PhysAddr, layout: Layout) {
        // fix the layout according to our layout requirements
        let layout = Self::fix_layout(layout);

        // extrat the size of the layout.
        // this is mutable since the size may be modified later as part of the deallocating algorithm.
        let mut size = layout.size();

        // calculate the end address of the deallocated region.
        let end_addr = addr + size;

        // handle the case where we have an adjacent free chunk right after the deallocated chunk
        let mut cursor = self.freelist.cursor();
        let maybe_chunk_after = cursor.find(|hdr| get_chunk_phys_addr(hdr) == end_addr);
        if let Some(chunk_after) = maybe_chunk_after {
            // in this case, what we want to do is:
            // - remove the existing chunk that is after the deallocated chunk
            // - increase the size of the deallocated chunk to include the existing chunk
            //
            // this way we can cover the entire combined region with a single chunk

            // count its bytes as part of the deallocated chunk
            size += chunk_after.hdr.data.size;

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
                chunk_before.hdr.data.size += size;
            }
            None => {
                // no adjacent free chunk before the deallocated chunk, just add it as a new chunk in the freelist.
                unsafe {
                    // SAFETY: this address is a valid header pointer since it was previously returned from our allocation,
                    // and we only return addresses that are valid header pointers.
                    //
                    // additionally, this address is no longer used because it is currently being deallocated.
                    //
                    // furthermore, this address is accessible through the kseg cachable memory region since it was previously
                    // returned from our allocation.
                    self.add_free_chunk(addr, size);
                }
            }
        }
    }
}

struct ChosenChunk {
    snapshot: ChunkCursorSnapshot,
    size: usize,
    alignment_padding: usize,
}

pub struct PhysAllocator(IrqSpinlock<RawPhysAllocator>);
impl PhysAllocator {
    pub const fn new() -> Self {
        Self(IrqSpinlock::new(RawPhysAllocator::new()))
    }
}
unsafe impl Allocator for PhysAllocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let mut allocator = self.0.lock();

        let phys_addr = unsafe {
            // SAFETY: the caller is responsible for deallocating this memory
            allocator.allocate(layout).ok_or(AllocError)?
        };

        let virt_addr = phys_addr.kseg_cachable_addr().unwrap();
        let allocated_ptr = unsafe { NonNull::new_unchecked(virt_addr.as_mut_ptr()) };
        Ok(NonNull::slice_from_raw_parts(allocated_ptr, layout.size()))
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        let virt_addr = VirtAddr(ptr.as_ptr() as usize);

        // we use the kseg cachable memory region for our virtual addressing
        let phys_addr = virt_addr.kseg_cachable_phys_addr().unwrap();

        let mut allocator = self.0.lock();
        unsafe {
            // SAFETY: the caller must have provided us with memory that was previously allocated using this allocator.
            allocator.dealloc(phys_addr, layout)
        };
    }
}

unsafe impl GlobalAlloc for PhysAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let mut allocator = self.0.lock();

        let maybe_phys_addr = unsafe {
            // SAFETY: the caller is responsible for deallocating this memory
            allocator.allocate(layout)
        };

        let Some(phys_addr) = maybe_phys_addr else {
            return null_mut();
        };

        let virt_addr = phys_addr.kseg_cachable_addr().unwrap();

        virt_addr.as_mut_ptr()
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let virt_addr = VirtAddr(ptr as usize);

        // we use the kseg cachable memory region for our virtual addressing
        let phys_addr = virt_addr.kseg_cachable_phys_addr().unwrap();

        let mut allocator = self.0.lock();
        unsafe {
            // SAFETY: the caller must have provided us with memory that was previously allocated using this allocator.
            allocator.dealloc(phys_addr, layout)
        };
    }
}

unsafe impl Sync for PhysAllocator {}
unsafe impl Send for PhysAllocator {}

#[global_allocator]
pub static PHYS_ALLOCATOR: PhysAllocator = PhysAllocator::new();
