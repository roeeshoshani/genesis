use core::{
    cell::UnsafeCell,
    future::Future,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::atomic::AtomicBool,
    task::Poll,
};

use crate::{
    executor::async_event::AsyncEventQueued,
    hw::interrupts::{is_in_interrupt, InterruptsDisabledGuard},
    utils::{PhantomUnsend, PhantomUnsendUnsync, PhantomUnsync},
};

/// a spinlock which can't be accessed in irq context, only in task context. this means that we don't need to disable interrupts
/// when locking it.
pub struct NonIrqLock<T> {
    data: spin::Mutex<T>,
}
impl<T> NonIrqLock<T> {
    pub const fn new(data: T) -> Self {
        Self {
            data: spin::Mutex::new(data),
        }
    }
    pub fn lock(&self) -> NonIrqLockGuard<T> {
        assert!(!is_in_interrupt());

        NonIrqLockGuard {
            spinlock_guard: self.data.lock(),
            phantom: PhantomUnsendUnsync::new(),
        }
    }
}

pub struct NonIrqLockGuard<'a, T: ?Sized + 'a> {
    spinlock_guard: spin::mutex::MutexGuard<'a, T>,

    /// mark the type as not `Send` and not `Sync` so that it cannot be held across an await point.
    ///
    /// this will help prevent bugs where we try to wait for a blocking operation while holding a spinlock,
    /// which may cause other tasks to busy poll while we are sleeping.
    phantom: PhantomUnsendUnsync,
}

impl<'a, T: ?Sized + core::fmt::Debug> core::fmt::Debug for NonIrqLockGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Debug::fmt(&**self, f)
    }
}

impl<'a, T: ?Sized + core::fmt::Display> core::fmt::Display for NonIrqLockGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Display::fmt(&**self, f)
    }
}

impl<'a, T: ?Sized> Deref for NonIrqLockGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.spinlock_guard
    }
}

impl<'a, T: ?Sized> DerefMut for NonIrqLockGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.spinlock_guard
    }
}

/// an async mutex which can only be accessed in task contexts, and not in irq contexts.
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
        assert!(!is_in_interrupt());
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
        //
        // we use relaxed ordering here since the triggering of the event provides release semantics with the woken task,
        // which guarantees that the task will see this store before it is woken up.
        self.mutex
            .is_locked
            .store(false, core::sync::atomic::Ordering::Relaxed);

        // wake up the next listener in the queue of tasks waiting for us to unlock
        self.mutex.unlock_event.trigger_one();
    }
}

#[derive(Debug)]
pub struct IrqLock<T>(spin::Mutex<T>);
impl<T> IrqLock<T> {
    pub const fn new(data: T) -> Self {
        Self(spin::Mutex::new(data))
    }
    pub fn lock(&self) -> IrqLockGuard<T> {
        IrqLockGuard {
            // NOTE: the order of the fields here is important. we first want to disable interrupts, and only then to lock
            // the spinlock.
            _interrupts_guard: InterruptsDisabledGuard::new(),
            spinlock_guard: self.0.lock(),
            phantom: PhantomUnsendUnsync::new(),
        }
    }
}

pub struct IrqLockGuard<'a, T: ?Sized + 'a> {
    // NOTE: the order of these fields is really important. it dictates the order in which they will get dropped.
    // we first want to drop the spinlock guard to unlock the lock, and only then we want to re-enable interrupts.
    spinlock_guard: spin::mutex::MutexGuard<'a, T>,
    _interrupts_guard: InterruptsDisabledGuard,

    /// mark the type as not `Send` and not `Sync` so that it cannot be held across an await point.
    ///
    /// this will help prevent bugs where we try to wait for a blocking operation while holding a spinlock,
    /// which may cause other tasks to busy poll while we are sleeping.
    phantom: PhantomUnsendUnsync,
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
        &self.spinlock_guard
    }
}

impl<'a, T: ?Sized> DerefMut for IrqLockGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.spinlock_guard
    }
}
