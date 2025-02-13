use core::{
    cell::UnsafeCell,
    future::Future,
    ops::{Deref, DerefMut},
    sync::atomic::AtomicBool,
    task::Poll,
};

use crate::{executor::async_event::AsyncEventQueued, hw::interrupts::InterruptsDisabledGuard};

#[derive(Debug)]
pub struct Mutex<T> {
    data: UnsafeCell<T>,
    unlock_event: AsyncEventQueued,
    is_locked: AtomicBool,
}
impl<T> Mutex<T> {
    pub const fn new(data: T) -> Self {
        Self {
            data: UnsafeCell::new(data),
            unlock_event: AsyncEventQueued::new(),
            is_locked: AtomicBool::new(false),
        }
    }
    pub fn lock(&self) -> MutexLock<T> {
        MutexLock {
            mutex: self,
            already_registered_to_unlock_event: false,
        }
    }
}
unsafe impl<T> Send for Mutex<T> {}
unsafe impl<T> Sync for Mutex<T> {}

pub struct MutexLock<'a, T> {
    mutex: &'a Mutex<T>,
    already_registered_to_unlock_event: bool,
}
impl<'a, T> Future for MutexLock<'a, T> {
    type Output = MutexGuard<'a, T>;

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        let listener_handle = if !self.already_registered_to_unlock_event {
            Some(self.mutex.unlock_event.listen(cx.waker().clone()))
        } else {
            None
        };
        match self.mutex.is_locked.compare_exchange(
            false,
            true,
            // we want release ordering here to ensure that the registration to the unlock event happens before this compare and
            // swap, so that we are guaranteed to be woken up once the mutex is unlocked.
            core::sync::atomic::Ordering::Release,
            // in the failure case, we don't care about the memory ordering, and we will just try again.
            core::sync::atomic::Ordering::Relaxed,
        ) {
            Ok(_) => {
                // we successfully locked the lock. we don't need to listen for the unlock event anymore
                if let Some(mut listener_handle) = listener_handle {
                    listener_handle.stop_listening();
                }
                Poll::Ready(MutexGuard {
                    data: unsafe {
                        // SAFETY: we have successfully swapped a previous value of false with a value of true, which means that
                        // we now have exclusive access
                        &mut *self.mutex.data.get()
                    },
                    mutex: self.mutex,
                })
            }
            Err(_) => {
                // failed to lock, go to sleep until the mutex is unlocked and someone wakes us up.
                Poll::Pending
            }
        }
    }
}

pub struct MutexGuard<'a, T> {
    mutex: &'a Mutex<T>,
    data: &'a mut T,
}

impl<'a, T: core::fmt::Debug> core::fmt::Debug for MutexGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Debug::fmt(&**self, f)
    }
}

impl<'a, T: core::fmt::Display> core::fmt::Display for MutexGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(&**self, f)
    }
}

impl<'a, T> Deref for MutexGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.data
    }
}

impl<'a, T> DerefMut for MutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.data
    }
}

impl<'a, T> Drop for MutexGuard<'a, T> {
    fn drop(&mut self) {
        // we are finished holding the lock, mark the lock as unlocked.
        // we want acquire ordering here to ensure that the triggering of the event happens after setting this to false.
        self.mutex
            .is_locked
            .store(false, core::sync::atomic::Ordering::Acquire);

        // wake up the next listener in the queue of tasks waiting for us to unlock
        self.mutex.unlock_event.trigger_one();
    }
}

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
