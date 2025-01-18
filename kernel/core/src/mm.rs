use core::{
    marker::PhantomData,
    ptr::{addr_of_mut, NonNull},
};

use hal::mem::{PhysAddr, PhysMemRegion, VirtAddr, KERNEL_CORE_ADDR};

extern "C" {
    /// a pointer to the end of the kernel.
    static mut END_OF_CODE: u8;
}

/// returns the size in bytes that the core kernel takes up in memory.
pub fn kernel_size() -> usize {
    let end_of_code_virt_addr = VirtAddr(addr_of_mut!(END_OF_CODE) as usize);

    // the kernel is running in the kseg cachable memory region, based on that we can calculate its physical address
    let end_of_code_phys_addr = end_of_code_virt_addr.kseg_cachable_phys_addr().unwrap();

    // calculate the diff between the end of code and the kernel start to get the size
    end_of_code_phys_addr.0 - KERNEL_CORE_ADDR.0
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

pub struct PageAllocator {
    freelist: ChunkLink,
}
impl PageAllocator {
    pub fn new(region: PhysMemRegion) -> Self {
        let aligned_start = PhysAddr(align_up(region.start.0, PAGE_SIZE));
        let aligned_end = PhysAddr(align_down(region.end().unwrap().0, PAGE_SIZE));

        // make sure that we have at least one page of memory space to allocate from.
        assert!(aligned_start + PAGE_SIZE <= aligned_end);

        // make sure that both the start and end addresses can be accessed through the kseg cachable memory region.
        let _ = aligned_start.kseg_cachable_addr().unwrap();
        let _ = aligned_end.kseg_cachable_addr().unwrap();

        let pages_amount = (aligned_end.0 - aligned_start.0) / PAGE_SIZE;

        // write the initial chunk
        let initial_chunk_ptr = aligned_start
            .kseg_cachable_addr()
            .unwrap()
            .as_mut_ptr::<ChunkHdr>();
        let initial_chunk = unsafe {
            // SAFETY: the pointer is page aligned, which is more than enough for our chunk header.
            // also, the pointer is writable since we use an unmapped virtual address, so there's no concept of read only.
            &mut *initial_chunk_ptr
        };

        unsafe {
            // SAFETY: the initial chunk is a valid mutable reference, so we can write to it.
            core::ptr::write_volatile(
                initial_chunk,
                ChunkHdr {
                    link: None,
                    pages_amount,
                },
            );
        };

        Self {
            freelist: Some(unsafe {
                // SAFETY: the initial chunk is a valid reference so it is not null
                NonNull::new_unchecked(initial_chunk)
            }),
        }
    }

    fn cursor(&mut self) -> ChunkCursor {
        ChunkCursor {
            prev_chunk_link: &mut self.freelist,
        }
    }

    pub fn alloc(&mut self, pages_amount: usize) -> Option<PhysAddr> {
        let mut cursor = self.cursor();

        let mut cur_best_chunk: Option<ChosenChunk<'_>> = None;

        loop {
            let Some(chunk_mut) = cursor.current() else {
                // finished iterating
                break;
            };

            if chunk_mut.chunk.pages_amount < pages_amount {
                // chunk is too small to satisfy allocation
                continue;
            }

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
}

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
    pub fn unlink(&mut self) {
        *self.prev_chunk_link = self.chunk.link;
    }

    pub fn chunk_virt_addr(&self) -> VirtAddr {
        VirtAddr(self.chunk as *const _ as usize)
    }

    pub fn chunk_phys_addr(&self) -> PhysAddr {
        // when writing the chunk headers we use the kseg cachable memory region, so we can find the phys addr by just
        // calculating our offset in that region.
        self.chunk_virt_addr().kseg_cachable_phys_addr().unwrap()
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
