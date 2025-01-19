use core::ptr::NonNull;

use hal::mem::VirtAddr;

/// a list of allocation headers.
pub struct AllocatorHdrList<T> {
    /// the head of the list
    pub head: AllocatorHdrLink<T>,
}
impl<T> AllocatorHdrList<T> {
    /// creates a new empty list.
    pub const fn new() -> Self {
        Self { head: None }
    }

    /// returns a cursor into the list
    pub fn cursor(&mut self) -> AllocatorHdrCursor<T> {
        AllocatorHdrCursor {
            prev_hdr_link: &mut self.head,
        }
    }

    /// initializes and pushes a new header to the front of the list.
    ///
    /// # safety
    /// the header pointer must be a valid header pointer (we can safely write a header there).
    /// also, the header pointer must point to unused memory.
    pub unsafe fn init_and_push_front(&mut self, hdr: NonNull<AllocatorHdr<T>>, data: T) {
        unsafe {
            // SAFETY: this function requires that the pointer is valid, so it is up to the caller to guarantee this
            core::ptr::write_volatile(
                hdr.as_ptr(),
                AllocatorHdr {
                    link: self.head,
                    data,
                },
            );
        }
        self.head = Some(hdr);
    }
}

/// a mutable reference to an allocator header in the list of headers.
pub struct AllocatorHdrMut<'a, T> {
    /// the header
    pub hdr: &'a mut AllocatorHdr<T>,

    /// a pointer to the link of the previous header.
    /// this is used to remove the header from the list, if we ever wish to do so.
    pub prev_hdr_link: &'a mut AllocatorHdrLink<T>,
}
impl<'a, T> AllocatorHdrMut<'a, T> {
    /// unlinks this header from the list of headers.
    pub fn unlink(self) -> &'a mut AllocatorHdr<T> {
        *self.prev_hdr_link = self.hdr.link;
        self.hdr
    }
}

pub struct AllocatorHdrCursorSnapshot<T> {
    prev_hdr_link_ptr: NonNull<AllocatorHdrLink<T>>,
}

/// an iterator over allocator headers.
pub struct AllocatorHdrCursor<'a, T> {
    /// a pointer to the link of the previous header.
    prev_hdr_link: &'a mut AllocatorHdrLink<T>,
}
impl<'a, T> AllocatorHdrCursor<'a, T> {
    /// returns the current item of the cursor.
    pub fn current(&self) -> Option<&'a T> {
        let hdr = self.current_hdr()?;
        Some(&hdr.data)
    }

    /// returns the current header of the cursor.
    pub fn current_hdr(&self) -> Option<&'a AllocatorHdr<T>> {
        let link = *self.prev_hdr_link;

        let hdr_ptr = link?;

        Some(unsafe {
            // SAFETY: we only insert valid pointers into the list, so this pointer is valid
            // also, there is no aliasing, since the only other ref that currently exists is a pointer to the previous header.
            hdr_ptr.as_ref()
        })
    }

    /// returns a mutable reference to the current header of the cursor.
    pub fn current_hdr_mut<'s>(&'s mut self) -> Option<AllocatorHdrMut<'s, T>> {
        let link = *self.prev_hdr_link;

        let mut hdr_ptr = link?;

        Some(AllocatorHdrMut {
            hdr: unsafe {
                // SAFETY: we only insert valid pointers into the list, so this pointer is valid
                // also, there is no aliasing, since the only other mutable ref that currently exists is a pointer to the previous header.
                hdr_ptr.as_mut()
            },
            prev_hdr_link: self.prev_hdr_link,
        })
    }

    /// creates a snapshot of the current position of the cursor.
    pub fn snapshot(&mut self) -> AllocatorHdrCursorSnapshot<T> {
        AllocatorHdrCursorSnapshot {
            prev_hdr_link_ptr: self.prev_hdr_link.into(),
        }
    }

    /// reverts the cursor to the position of a snapshot that was previously taken.
    ///
    /// # safety
    /// - the snapshot must have been taken on this exact cursor. don't use a snapshot taken on another cursor.
    /// - the structure of the list must be exactly the same as it was at the moment when the snapshot was taken.
    pub unsafe fn revert(&mut self, mut snapshot: AllocatorHdrCursorSnapshot<T>) {
        self.prev_hdr_link = unsafe {
            // SAFETY: the snapshot's pointer was constructed from a valid reference, so we can cast it back to a reference.
            //
            // as for aliasing, this pointer will be the only existing mutable pointer into the freelist, so we are fine.
            //
            // also, the pointer must point to a valid item since the structure of the list hasn't changed.
            snapshot.prev_hdr_link_ptr.as_mut()
        };
    }

    /// advance to the next item.
    pub fn move_next(&mut self) {
        let link = *self.prev_hdr_link;

        let Some(mut hdr_ptr) = link else {
            // we already reached the end of the list, do nothing
            return;
        };

        // move the prev header link pointer to the next header
        self.prev_hdr_link = {
            let hdr = unsafe {
                // SAFETY: we only insert valid pointers into the list, so this pointer is valid.
                // also, there is no aliasing, since the only other mutable ref that currently exists is a pointer to the previous header.
                hdr_ptr.as_mut()
            };
            &mut hdr.link
        };
    }

    /// finds an item which matches the given predicate.
    pub fn find<'s, F>(&'s mut self, predicate: F) -> Option<AllocatorHdrMut<'s, T>>
    where
        F: FnMut(&AllocatorHdr<T>) -> bool,
    {
        if self.move_next_until(predicate) {
            self.current_hdr_mut()
        } else {
            None
        }
    }

    /// moves to the next item until an item which matches the given predicate is encountered.
    pub fn move_next_until<F>(&mut self, mut predicate: F) -> bool
    where
        F: FnMut(&AllocatorHdr<T>) -> bool,
    {
        while let Some(hdr) = self.current_hdr() {
            if predicate(hdr) {
                return true;
            }
            self.move_next();
        }
        false
    }
}

/// the header of an allocator.
pub struct AllocatorHdr<T> {
    pub link: AllocatorHdrLink<T>,
    pub data: T,
}
impl<T> AllocatorHdr<T> {
    /// returns the virtual address of the allocation header
    pub fn virt_addr(&self) -> VirtAddr {
        VirtAddr(self as *const Self as usize)
    }
}

/// the link of a chunk, which is a pointer to the next chunk in the linked list of free chunks.
pub type AllocatorHdrLink<T> = Option<NonNull<AllocatorHdr<T>>>;
