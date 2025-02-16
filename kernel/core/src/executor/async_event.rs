use core::{
    future::Future,
    task::{Poll, Waker},
};

use alloc::{collections::VecDeque, vec::Vec};

use crate::sync::{IrqLock, IrqLockGuard};

#[derive(Debug)]
pub struct AsyncEvent {
    /// a list of wakers to wake once the event is triggered.
    ///
    /// we use an irq lock since we want to be able to wake the tasks up from irq context.
    wakers: IrqLock<Vec<Waker>>,
}
impl AsyncEvent {
    pub const fn new() -> Self {
        Self {
            wakers: IrqLock::new(Vec::new()),
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
    ///
    /// this provides an atomic release semantic with the woken tasks.
    pub fn trigger(&self) {
        let mut wakers = self.wakers.lock();
        for waker in wakers.drain(..) {
            waker.wake();
        }
    }

    pub fn wait(&self) -> AsyncEventWait {
        AsyncEventWait {
            event: self,
            is_first_time: true,
        }
    }
    pub fn wait_prepare<F: Fn()>(&self, prepare: F) -> AsyncEventWaitPrepare<F> {
        AsyncEventWaitPrepare {
            event: self,
            is_first_time: true,
            prepare,
        }
    }
}

#[must_use]
pub struct AsyncEventWaitPrepare<'a, F: Fn()> {
    event: &'a AsyncEvent,
    is_first_time: bool,
    prepare: F,
}
impl<'a, F: Fn()> Future for AsyncEventWaitPrepare<'a, F> {
    type Output = ();

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> Poll<Self::Output> {
        if self.is_first_time {
            // if this is the first time we are being polled, just register for the event and wait for it to happen
            self.event.listen(cx.waker().clone());

            (self.prepare)();

            Poll::Pending
        } else {
            // if we got polled again after registering, it means that the event was triggered and woke us up.
            Poll::Ready(())
        }
    }
}

#[must_use]
pub struct AsyncEventWait<'a> {
    event: &'a AsyncEvent,
    is_first_time: bool,
}
impl<'a> Future for AsyncEventWait<'a> {
    type Output = ();

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> Poll<Self::Output> {
        if self.is_first_time {
            // if this is the first time we are being polled, just register for the event and wait for it to happen
            self.event.listen(cx.waker().clone());
            Poll::Pending
        } else {
            // if we got polled again after registering, it means that the event was triggered and woke us up.
            Poll::Ready(())
        }
    }
}

pub struct AsyncEventListenerHandle<'a> {
    wakers: IrqLockGuard<'a, Vec<Waker>>,
}
impl<'a> AsyncEventListenerHandle<'a> {
    pub fn stop_listening(&mut self) {
        // we create the listener handle right after pushing the waker into the waker list.
        // so, we can just pop the last waker to stop listening.
        let _ = self.wakers.pop();
    }
}

#[derive(Debug)]
pub struct AsyncEventQueued {
    /// a queue of wakers waiting for the lock to be unlocked.
    ///
    /// we use an irq lock since we want to be able to wake the tasks up from irq context.
    wakers: IrqLock<VecDeque<Waker>>,
}
impl AsyncEventQueued {
    pub const fn new() -> Self {
        Self {
            wakers: IrqLock::new(VecDeque::new()),
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
    pub fn listen(&self, waker: Waker) -> AsyncEventQueuedListenerHandle {
        let mut wakers = self.wakers.lock();
        wakers.push_back(waker);
        AsyncEventQueuedListenerHandle { wakers }
    }

    /// triggers the event once, waking up the next waker in the queue.
    ///
    /// this provides an atomic release semantic with the woken task.
    pub fn trigger_one(&self) {
        let mut wakers = self.wakers.lock();
        for waker in wakers.drain(..) {
            waker.wake();
        }
    }
}

pub struct AsyncEventQueuedListenerHandle<'a> {
    wakers: IrqLockGuard<'a, VecDeque<Waker>>,
}
impl<'a> AsyncEventQueuedListenerHandle<'a> {
    pub fn stop_listening(&mut self) {
        // we create the listener handle right after pushing the waker into the waker list.
        // so, we can just pop the last waker to stop listening.
        let _ = self.wakers.pop_back();
    }
}
