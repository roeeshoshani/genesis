use core::{
    cell::UnsafeCell,
    ops::{Deref, DerefMut},
};

use crate::hw::interrupts::InterruptsDisabledGuard;

#[derive(Debug)]
pub struct IrqLock<T>(UnsafeCell<T>);
impl<T> IrqLock<T> {
    pub const fn new(data: T) -> Self {
        Self(UnsafeCell::new(data))
    }
    pub fn lock(&self) -> IrqLockGuard<T> {
        IrqLockGuard {
            _interrupts_guard: InterruptsDisabledGuard::new(),
            data: unsafe {
                // SAFETY: disabling interrupts, combined with the fact that we currently don't support multiple cores,
                // guarantees that we have exclusive access, as long as interrupts are disabled.
                &mut *self.0.get()
            },
        }
    }
}
unsafe impl<T> Send for IrqLock<T> {}
unsafe impl<T> Sync for IrqLock<T> {}

pub struct IrqLockGuard<'a, T: ?Sized + 'a> {
    data: &'a mut T,
    _interrupts_guard: InterruptsDisabledGuard,
}

impl<'a, T: ?Sized + core::fmt::Debug> core::fmt::Debug for IrqLockGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Debug::fmt(&**self, f)
    }
}

impl<'a, T: ?Sized + core::fmt::Display> core::fmt::Display for IrqLockGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(&**self, f)
    }
}

impl<'a, T: ?Sized> Deref for IrqLockGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.data
    }
}

impl<'a, T: ?Sized> DerefMut for IrqLockGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.data
    }
}
