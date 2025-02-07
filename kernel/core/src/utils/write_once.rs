use core::{cell::UnsafeCell, mem::MaybeUninit, sync::atomic::AtomicU8};

#[repr(u8)]
enum WriteOnceState {
    Uninitialized,
    Initializing,
    Initialized,
}

pub struct WriteOnce<T> {
    value: UnsafeCell<MaybeUninit<T>>,
    state: AtomicU8,
}
impl<T> WriteOnce<T> {
    pub const fn new() -> Self {
        Self {
            value: UnsafeCell::new(MaybeUninit::uninit()),
            state: AtomicU8::new(WriteOnceState::Uninitialized as u8),
        }
    }

    /// write an initial value to this write once instance.
    pub fn write(&self, initial_value: T) {
        // try to being an initialization sequence.
        self.state
            .compare_exchange(
                WriteOnceState::Uninitialized as u8,
                WriteOnceState::Initializing as u8,
                // in the success case, we want acquire memory ordering, so that the write to the value is strictly after
                // we see the uninit value.
                core::sync::atomic::Ordering::Acquire,
                // in the failure case, we don't require any special ordering.
                core::sync::atomic::Ordering::Relaxed,
            )
            .expect("write once value was written to twice");

        // write the initial value
        let value = unsafe {
            // SAFETY: once we have set the state to initializing, we have exclusive access.
            // readers and writers that will try to access the value while in that state will panic.
            &mut *self.value.get()
        };
        value.write(initial_value);

        // mark it as initialized.
        //
        // we don't need a compare exchange since setting the state to initializing guarantees exclusive access,
        // so the state here is still set to initializing.
        self.state.store(
            WriteOnceState::Initialized as u8,
            // we want release memory ordering here so that the write to the value happens strictly before this store.
            core::sync::atomic::Ordering::Release,
        );
    }

    /// asserts that the instance is fully initialized before reading.
    fn read_assert_initialized(&self) {
        assert_eq!(
            // we want acquire memory ordering here so that the reading of the value is strictly after this read, that is
            // after we make sure that the value is properly initialized.
            self.state.load(core::sync::atomic::Ordering::Acquire),
            WriteOnceState::Initialized as u8,
            "write once value is being read before being fully initialized"
        );
    }

    /// gets a reference to the underlying value.
    pub fn get(&self) -> &T {
        self.read_assert_initialized();
        let value = unsafe {
            // SAFETY: we verified that the value is initialized, so there won't be any further writes to this value.
            &*self.value.get()
        };
        unsafe {
            // SAFETY: we verified that the value is initialized
            value.assume_init_ref()
        }
    }
}
unsafe impl<T: Send> Send for WriteOnce<T> {}
unsafe impl<T: Sync> Sync for WriteOnce<T> {}
impl<T> Drop for WriteOnce<T> {
    fn drop(&mut self) {
        // we want acquire memory ordering here so that the reading of the value is strictly after this read, that is
        // after we make sure that the value is properly initialized.
        if self.state.load(core::sync::atomic::Ordering::Acquire)
            == WriteOnceState::Initialized as u8
        {
            let value = self.value.get();
            unsafe {
                // SAFETY: the value is properly initialized, and is only dropped when the write once instance is dropped, so it
                // wasn't dropped yet.
                value.drop_in_place()
            };
        }
    }
}
