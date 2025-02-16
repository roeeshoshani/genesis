use core::pin::Pin;

use alloc::boxed::Box;
use hal::mem::{PhysAddr, VirtAddr};

use crate::mem::phys_alloc::BoxPhysAddr;

#[derive(Debug)]
pub struct DmaPtr<T>(Pin<Box<T>>);
impl<T> DmaPtr<T> {
    pub fn new(data: T) -> Self {
        Self(Box::pin(data))
    }
    pub fn into_inner(self) -> Pin<Box<T>> {
        self.0
    }
    pub fn phys_addr(&self) -> PhysAddr {
        self.0.phys_addr()
    }
    pub fn uncachable_addr(&self) -> VirtAddr {
        self.phys_addr().kseg_uncachable_addr().unwrap()
    }
    pub fn as_ref_cachable(&self) -> &T {
        &*self.0
    }
    pub fn as_ref(&self) -> &T {
        unsafe {
            // SAFETY: this is a valid pointer, we just made it uncachable.
            self.uncachable_addr().as_ref()
        }
    }
}
impl<T: Unpin> DmaPtr<T> {
    pub fn as_mut_cachable(&mut self) -> &mut T {
        &mut *self.0
    }
    pub fn as_mut(&mut self) -> &mut T {
        unsafe {
            // SAFETY: this is a valid pointer, we just made it uncachable.
            // additionally, it is `Unpin`, so we can mutable access it through the `Pin`.
            self.uncachable_addr().as_mut()
        }
    }
}
