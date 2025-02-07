use core::task::Waker;

use alloc::vec::Vec;

use crate::sync::{IrqSpinlock, IrqSpinlockGuard};

pub struct AsyncEvent {
    wakers: IrqSpinlock<Vec<Waker>>,
}
impl AsyncEvent {
    pub const fn new() -> Self {
        Self {
            wakers: IrqSpinlock::new(Vec::new()),
        }
    }
    /// start listening to this event, such that once the event is triggered, the given waker will be
    /// woken up.
    ///
    /// returns a handle to the listener. the handle allows for removing the listener in case you realize
    /// that you don't need it after starting to listen.
    ///
    /// in the case of edge triggered interrupts, it is a common pattern to start listening and only then try to poll,
    /// and if the poll succeeds, you should remove the listener to free up the memory used by it.
    pub fn listen(&self, waker: Waker) -> AsyncEventListenerHandle {
        let mut wakers = self.wakers.lock();
        wakers.push(waker);
        AsyncEventListenerHandle { wakers }
    }

    /// triggers the event, waking up all wakers that have registered.
    pub fn trigger(&self) {
        let mut wakers = self.wakers.lock();
        for waker in wakers.drain(..) {
            waker.wake();
        }
    }
}

pub struct AsyncEventListenerHandle<'a> {
    wakers: IrqSpinlockGuard<'a, Vec<Waker>>,
}
impl<'a> AsyncEventListenerHandle<'a> {
    pub fn stop_listening(&mut self) {
        // we create the listener handle right after pushing the waker into the waker list.
        // so, we can just pop the last waker to stop listening.
        let _ = self.wakers.pop();
    }
}
