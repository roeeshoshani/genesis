use core::{alloc::Layout, ptr::NonNull};

use hal::mem::PhysAddr;

use crate::{
    mem::{is_aligned, page_alloc::alloc_pages},
    println,
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

/// the maximum allowed size of an allocation, in pages
pub const MAX_ALLOCATION_SIZE_PAGES: usize = 4;

/// the maximum allowed size of an allocation.
pub const MAX_ALLOCATION_SIZE: usize = PAGE_SIZE * MAX_ALLOCATION_SIZE_PAGES;

/// the maximum allowed alignment of an allocation.
pub const MAX_ALLOCATION_ALIGN: usize = PAGE_SIZE;

pub struct PhysAllocator {
    freelist: ChunkList,
}
impl PhysAllocator {
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
                align_up_p2(chunk_phys_addr.0, layout.align())
            };

            // calculate the amount of padding required from the start of the chunk for the allocation to be properly aligned.
            let alignment_padding = aligned_addr - chunk_phys_addr.0;

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
            align_up_p2(chunk_phys_addr.0, layout.align())
        };

        // check if we have any padding at the start of the chunk.
        // if so, then we need to keep the chunk header, and sets its size to the size of the padding.
        let padding = allocated_addr - chunk_phys_addr.0;
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
            let postfix_addr = chosen_chunk.hdr.virt_addr() + padding + layout.size();

            // make sure that the address of the postfix is aligned to the size of a chunk header.
            //
            // note that this check also makes sure that the address is aligned to the alignment of a chunk header.
            //
            // this should always be true since the chunk's original address, the padding size, and the requested allocation size, are
            // all aligned to a chunk header size.
            //
            // so, this address must also be aligned to it.
            debug_assert!(is_aligned(postfix_addr.0, size_of::<ChunkHdr>()));

            // create a pointer to a chunk header which we will put at the postfix.
            let postfix_chunk_ptr = unsafe {
                // all of our virtual addresses are in the kseg cachable memory region, and addresses in that region can never
                // be null.
                NonNull::new_unchecked(postfix_addr.as_mut_ptr::<ChunkHdr>())
            };

            unsafe {
                // SAFETY:
                // as previously explained, the postfix padding is large enough to hold a chunk header, and the postfix
                // address is properly aligned for it.
                //
                // additionally, this memory is no longer used since we shrinked the original chunk to only cover the padding
                // at the start of the chunk.
                self.freelist
                    .init_and_push_front(postfix_chunk_ptr, Chunk { size: postfix_size })
            };
        }

        Some(PhysAddr(allocated_addr))
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

    pub fn allocate(&mut self, layout: Layout) -> Option<PhysAddr> {
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
        let new_chunk_addr = raw_pages.addr.kseg_cachable_addr().unwrap();
        let new_chunk_ptr = unsafe {
            // SAFETY: kseg cachable addresses are never null
            NonNull::new_unchecked(new_chunk_addr.as_mut_ptr::<ChunkHdr>())
        };
        unsafe {
            // SAFETY:
            // the address is page aligned, which should be enough of an alignment to store our chunk header.
            //
            // additionally, the memory region is composed of multiple pages, which is large enough to store our chunk header.
            //
            // moreover, it is unused memory since we just allocated it from the page allocator.
            self.freelist.init_and_push_front(
                new_chunk_ptr,
                Chunk {
                    size: MAX_ALLOCATION_SIZE,
                },
            )
        };

        // after adding that extra memory to the freelist, try once again to allocate from it, this time with no other fallback
        // option.
        self.allocate_from_freelist(layout)
    }
}

struct ChosenChunk {
    snapshot: ChunkCursorSnapshot,
    size: usize,
    alignment_padding: usize,
}
