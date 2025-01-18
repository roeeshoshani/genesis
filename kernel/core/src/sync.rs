use core::ops::{Deref, DerefMut};

use crate::hw::interrupts::InterruptsDisabledGuard;

pub struct IrqSpinlock<T>(spin::mutex::SpinMutex<T>);
impl<T> IrqSpinlock<T> {
    pub const fn new(data: T) -> Self {
        Self(spin::mutex::SpinMutex::new(data))
    }
    pub fn lock(&self) -> IrqSpinlockGuard<T> {
        let interrupts_guard = InterruptsDisabledGuard::new();
        IrqSpinlockGuard {
            spinlock_guard: self.0.lock(),
            interrupts_guard,
        }
    }
}

pub struct IrqSpinlockGuard<'a, T: ?Sized + 'a> {
    // NOTE: the order of these fields is really important. it dictates the order in which they will get dropped.
    // we first want to drop the spinlock guard to unlock the lock, and only then we want to re-enable interrupts.
    spinlock_guard: spin::mutex::SpinMutexGuard<'a, T>,
    interrupts_guard: InterruptsDisabledGuard,
}

impl<'a, T: ?Sized + core::fmt::Debug> core::fmt::Debug for IrqSpinlockGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Debug::fmt(&**self, f)
    }
}

impl<'a, T: ?Sized + core::fmt::Display> core::fmt::Display for IrqSpinlockGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(&**self, f)
    }
}

impl<'a, T: ?Sized> Deref for IrqSpinlockGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &*self.spinlock_guard
    }
}

impl<'a, T: ?Sized> DerefMut for IrqSpinlockGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut *self.spinlock_guard
    }
}
