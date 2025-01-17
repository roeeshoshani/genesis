use core::{cell::UnsafeCell, ptr::NonNull};

use hal::mem::{PhysAddr, PhysMemRegion, VirtAddr};
use static_assertions::const_assert;

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

pub struct BuddyAllocator {
    freelist: UnsafeCell<ChunkLink>,
}
impl BuddyAllocator {
    /// the minimum allocation size. any allocation request for a size smaller than this will allocate this many bytes.
    ///
    /// every allocation must be able to hold a free chunk header, so we determine the minimum size based on that.
    pub const MIN_ALLOC_SIZE: usize = size_of::<ChunkHdr>().checked_next_power_of_two().unwrap();

    /// the log2 of the minimum allocation size.
    pub const MIN_ALLOC_SIZE_LOG2: u8 = Self::MIN_ALLOC_SIZE.ilog2() as u8;

    /// the alignment of a free chunk header.
    const HDR_ALIGN: usize = align_of::<ChunkHdr>();

    // we want to make sure that the minimum allocation size is a multiple of the chunk header alignment. this is necessary because
    // otherwise, the addresses of chunks might have the wrong alignment, even if the start address of the allocator is aligned.
    //
    // we can represent each allocated address as `base + n * min_size`, where `base` is the start address of the entire memory region
    // used by the allocator, and `min_size` is the minimum allocation size.
    //
    // assuming that we align the `base` address to the alignment required by the chunk header, to make sure that every allocated address
    // is suitably aligned to fit a chunk header, we need `min_size` to also be aligned to the alignment of the chunk header.
    const _MIN_SIZE_ALIGN_ASSERTION: () = {
        assert!(is_aligned(Self::MIN_ALLOC_SIZE, Self::HDR_ALIGN));
    };

    pub fn new(region: PhysMemRegion) -> Self {
        // align the start address so that we can store our free chunk structure there
        let aligned_start = PhysAddr(align_up(region.start.0, Self::HDR_ALIGN));

        // make sure that we indeed have any space after aligning
        let end = region.end().unwrap();
        assert!(aligned_start < end);

        // make sure that both the start and end addresses are accessible through the kseg cachable memory region.
        let _ = aligned_start.kseg_cachable_addr().unwrap();
        let _ = end.kseg_cachable_addr().unwrap();

        // calculate the size of the region.
        let raw_size = end.0 - aligned_start.0;

        // calculate the log2 size of the region, since we represent sizes based on their log2 value.
        let size_log2 = raw_size.ilog2() as u8;

        // TODO: we will now use the rounded down log2 size as the size of the region. by doing this, we lose a lot of memory.
        //
        // for example, if we have a region of 127mb, after doing the log2, the size of the region we will use will be 64mb.
        // this is a huge waste.
        //
        // in the future, i should add support for initializing the allocator in a smarter way such that i can support chunks that
        // don't have buddies, and then i could cover all the currently unused space with smaller chunks.

        // make sure that we have enough space for the minimal allocation size, otherwise we can't allocate anything.
        assert!(size_log2 >= Self::MIN_ALLOC_SIZE_LOG2);

        // write the initial chunk
        let initial_chunk = NonNull::new(
            aligned_start
                .kseg_cachable_addr()
                .unwrap()
                .as_mut_ptr::<ChunkHdr>(),
        )
        .unwrap();
        unsafe {
            // SAFETY: the pointer is aligned because we use the aligned start address.
            //
            // also, this is writable memory since we are using an unmapped kseg segment, so there is no such thing as read only memory.
            // additionally, we made sure that we have enough space for the minimum allocation size, which takes into account the size
            // of the header, so we must have enough space for a full header.
            initial_chunk.write_volatile(ChunkHdr {
                link: UnsafeCell::new(None),
                size_log2,
            })
        };

        Self {
            freelist: UnsafeCell::new(Some(initial_chunk)),
        }
    }

    fn cursor(&self) -> ChunkCursor {
        ChunkCursor {
            prev_chunk_link: &self.freelist,
        }
    }

    fn alloc_choose_item(&self, size_log2: u8) -> Option<CursorItem> {
        let mut chooser = ChunkChooser {
            best_item: None,
            requested_size_log2: size_log2,
        };

        let mut cursor = self.cursor();
        while let Some(cur_chunk_ptr) = cursor.next() {
            chooser.process_chunk(cur_chunk_ptr);
        }

        chooser.best_item
    }

    fn alloc_choose_chunk(&mut self, size_log2: u8) -> Option<Chunk> {
        // choose a chunk and get its cursor item
        let item = self.alloc_choose_item(size_log2)?;

        let chunk_ptr = unsafe {
            // SAFETY: this function takes a mutable reference to self, thus ensuring that it provides exclusive access to `self`
            // and thus to the freelist. so, it is safe to convert this immutable reference to a mutable one.
            item.to_chunk_ptr()
        };

        Some(chunk_ptr)
    }

    pub unsafe fn alloc(&mut self, size: usize) -> Option<PhysAddr> {
        // first, calculate the minimal log2 size that is needed to satisfy the requested size
        let raw_size_log2 = size.checked_next_power_of_two().unwrap().ilog2() as u8;

        // take the allocator's minimum allocation size into account
        let size_log2 = core::cmp::max(Self::MIN_ALLOC_SIZE_LOG2, raw_size_log2);

        // try to find a chunk which we can use to allocate here. if no chunk is found, then we are out of memory, and return `None`.
        let chosen_chunk_ptr = self.alloc_choose_chunk(size_log2)?;

        // now, let's check if we need to split this chunk, or if it matches the size exactly.
        if chosen_chunk_ptr.chunk.size_log2 == size_log2 {
            // the size matches exactly, so just use this chunk.
            Some(chosen_chunk_ptr.allocate())
        } else {
            // the chunk is bigger than the allocation size
            Some(chosen_chunk_ptr.split_allocate(size_log2))
        }
    }
}

/// a chunk chooser. responsible for choosing a chunk to use during buddy allocation.
struct ChunkChooser<'a> {
    /// the current best free chunk item.
    best_item: Option<CursorItem<'a>>,

    /// the log2 size requested for allocation.
    requested_size_log2: u8,
}
impl<'a> ChunkChooser<'a> {
    fn process_chunk(&mut self, new_item: CursorItem<'a>) {
        if new_item.chunk.size_log2 < self.requested_size_log2 {
            // this chunk is too small to satisfy this allocation. ignore it.
            return;
        }

        match &self.best_item {
            Some(best_item) => {
                // we have an existing best item. compare the new item to it.
                if new_item.chunk.size_log2 < best_item.chunk.size_log2 {
                    // the new item is smaller, so it is a better fit for this allocation. use it instead.
                    self.best_item = Some(new_item)
                }
            }
            None => {
                // no current best item. use this item as the new best.
                self.best_item = Some(new_item)
            }
        }
    }
}

/// a type which represents free chunk in the buddy allocator freelist.
struct Chunk<'a> {
    /// the chunk
    chunk: &'a mut ChunkHdr,

    /// a pointer to the link of the previous chunk.
    /// this is used to remove the chunk from the linked list of free chunks.
    prev_chunk_link: &'a mut ChunkLink,
}
impl<'a> Chunk<'a> {
    pub fn unlink(&mut self) {
        *self.prev_chunk_link = unsafe {
            // SAFETY: there are no concurrent mutators, since the only other mutable reference is to the previous chunk.
            self.chunk.link.get().read()
        };
    }

    /// returns whether this chunk is big enough to be split into 2 smaller chunks.
    pub fn can_be_split(&self) -> bool {
        self.chunk.size_log2 > BuddyAllocator::MIN_ALLOC_SIZE_LOG2
    }

    /// splits this chunk into 2 smaller chunks. returns one of the 2 created chunks.
    pub fn split(self) -> Self {
        // make sure that this chunk can be split
        assert!(self.can_be_split());

        // calculate the new size for the 2 sub chunks. the new size is half the current size, which in terms of the log2 size,
        // can be calculated by just subtracting 1.
        let new_size_log2 = self.chunk.size_log2 - 1;
        let new_size = 1usize << new_size_log2;

        // update the size of the current chunk to half its original size. the next half of this chunk is now danling memory space.
        // TODO: do we need a volatile write here? can the compiler optimize this out?
        self.chunk.size_log2 = new_size_log2;

        // calculate the address of the new chunk at the second half of this chunk.
        let cur_chunk_ptr = self.chunk as *mut ChunkHdr;
        let new_chunk = unsafe {
            // SAFETY: this will not wrap around the address space since the memory region from which we allocate is properly
            // bounded, and every chunk is always inside of that region.
            //
            // additionally, this will be properly aligned.
            // the amount we add is a power of 2, and it is a greater (or equal) power than the minimum allocation size.
            // the minimum allocation size is itself a power of 2, and thus the added amount is a multiple of the minimum allocation size.
            // and, the minimum allocation size is a multiple of the chunk header alignment, so the added amount must also be aligned.
            // furthermore, the base pointer is aligned since it was previously a valid reference, so adding an aligned amount to it
            // will produce an aligned pointer.
            //
            // moreover, this is writable memory since all chunk header pointers are using an unmapped kseg segment, so there is
            // no such thing as read only memory.
            //
            // due to all of the above reasons, this is a valid chunk header pointer.
            &mut *cur_chunk_ptr.wrapping_byte_add(new_size)
        };

        // create non-null pointers from our references
        let cur_chunk_nonnull = unsafe {
            // SAFETY: this is a reference, so it is not null
            NonNull::new_unchecked(self.chunk)
        };
        let new_chunk_nonnull = unsafe {
            // SAFETY: this is a reference, so it is not null
            NonNull::new_unchecked(new_chunk)
        };

        // write the new chunk header.
        // we make the new chunk point to the current chunk, which is the first step in inserting it into the chunk freelist.
        unsafe {
            // SAFETY: the new chunk is a valid mutable reference so we can write to it.
            core::ptr::write_volatile(
                new_chunk,
                ChunkHdr {
                    link: UnsafeCell::new(Some(cur_chunk_nonnull)),
                    size_log2: new_size_log2,
                },
            );
        };

        // now make the prev chunk point to the new chunk to fully insert it into the freelist.
        // TODO: do we need a volatile write here? can the compiler optimize this out?
        *self.prev_chunk_link = Some(new_chunk_nonnull);

        // return a pointer to the new chunk.
        // it doesn't really matter which chunk, we can also return the current chunk here, this choice is arbitrary.
        Self {
            chunk: new_chunk,
            prev_chunk_link: self.prev_chunk_link,
        }
    }

    pub fn virt_addr(&self) -> VirtAddr {
        VirtAddr(self.chunk as *const _ as usize)
    }

    pub fn phys_addr(&self) -> PhysAddr {
        // when writing the chunk headers we use the kseg cachable memory region, so we can find the phys addr by just
        // calculating our offset in that region.
        self.virt_addr().kseg_cachable_phys_addr().unwrap()
    }

    /// allocate this chunk, by unlinking it from the freelist and returning its physical address.
    pub fn allocate(mut self) -> PhysAddr {
        self.unlink();
        self.phys_addr()
    }

    /// splits this chunk as much as needed and allocates the smallest possible piece of it to match the requested allocation size.
    pub fn split_allocate(self, requested_size_log2: u8) -> PhysAddr {
        // make sure that the size of this chunk is big enough to satisfy this allocation.
        assert!(self.chunk.size_log2 > requested_size_log2);

        // split the current chunk until we reach the desired size
        let mut cur_chunk = self;
        while cur_chunk.chunk.size_log2 > requested_size_log2 {
            cur_chunk = cur_chunk.split();
        }

        // sanity - make sure that we reached the desired size
        assert_eq!(cur_chunk.chunk.size_log2, requested_size_log2);

        cur_chunk.allocate()
    }
}

/// information about a chunk while iterating over the freelist using a cursor.
struct CursorItem<'a> {
    /// the chunk.
    chunk: &'a ChunkHdr,

    /// an unsafe cell pointer to the chunk, which can later be used to convert this to a mutable reference.
    chunk_raw: &'a UnsafeCell<ChunkHdr>,

    /// a pointer to the link of the previous chunk.
    /// this is used to remove the chunk from the linked list of free chunks.
    ///
    /// we use an unsafe cell here since this might later be converted to a mutable reference when using the chunk.
    prev_chunk_link: &'a UnsafeCell<ChunkLink>,
}
impl<'a> CursorItem<'a> {
    /// converts this cursor item to a mutable chunk pointer.
    ///
    /// # safety
    /// this function is really dangerous. the cursor only provides immutable iteration over the freelist, but this function converts
    /// the cursor's immutable references to mutable references.
    ///
    /// this means that you must avoid any access to the freelist outside of the returned pointer once you call this function.
    pub unsafe fn to_chunk_ptr(self) -> Chunk<'a> {
        Chunk {
            chunk: &mut *self.chunk_raw.get(),
            prev_chunk_link: &mut *self.prev_chunk_link.get(),
        }
    }
}

/// a cursor which allows iteration over the freelist of a buddy allocator
struct ChunkCursor<'a> {
    /// a pointer to the link of the previous chunk.
    prev_chunk_link: &'a UnsafeCell<ChunkLink>,
}
impl<'a> ChunkCursor<'a> {
    pub fn next(&mut self) -> Option<CursorItem<'a>> {
        // read the link of the previous chunk
        let link = unsafe {
            // SAFETY: there are no concurrent mutators, since the cursor is read only and does not mutate anything.
            // the only danger is that the items we return can be converted to mutable pointers, but at that point it is the caller's
            // reponsibility to stop using this cursor. so, we can assume that all accesses through the cursor are read only, and
            // thus this is ok.
            self.prev_chunk_link.get().read()
        };

        // if the link does not point to any chunk, return
        let chunk_ptr = link?;
        let chunk_raw = unsafe {
            // SAFETY: the pointers in our freelist are valid chunk header pointers. we never insert an invalid pointer into the freelist.
            &*(chunk_ptr.as_ptr() as *const UnsafeCell<ChunkHdr>)
        };

        // ok we have a chunk. construct the pointer to it.
        let result = CursorItem {
            chunk: unsafe {
                // SAFETY: there are no concurrent mutators, since the cursor is read only and does not mutate anything.
                // the only danger is that the items we return can be converted to mutable pointers, but at that point it is the caller's
                // reponsibility to stop using this cursor. so, we can assume that all accesses through the cursor are read only, and
                // thus this is ok.
                &*chunk_raw.get()
            },
            chunk_raw,
            prev_chunk_link: self.prev_chunk_link,
        };

        // move to the next chunk.
        self.prev_chunk_link = unsafe { &(*chunk_raw.get()).link };

        // return the result
        Some(result)
    }
}

/// the header of a free chunk in the buddy allocator.
#[repr(C)]
struct ChunkHdr {
    link: UnsafeCell<ChunkLink>,
    size_log2: u8,
}

pub type ChunkLink = Option<NonNull<ChunkHdr>>;
