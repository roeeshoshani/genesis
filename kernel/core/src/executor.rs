use core::{
    future::Future,
    pin::Pin,
    ptr::null,
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
};

use alloc::{boxed::Box, vec::Vec};

pub trait TaskTrait: Future<Output = ()> + 'static + Send {
    fn should_be_polled(&self) -> bool;
}

pub struct Task<F: Future<Output = ()> + 'static + Send> {
    should_be_polled: bool,
    future: F,
}
impl<F: Future<Output = ()> + 'static + Send> Future for Task<F> {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let future = unsafe {
            // SAFETY: if self is pinned, then its fields are also pinned
            self.map_unchecked_mut(|task| &mut task.future)
        };
        Future::poll(future, cx)
    }
}
impl<F: Future<Output = ()> + 'static + Send> TaskTrait for Task<F> {
    fn should_be_polled(&self) -> bool {
        self.should_be_polled
    }
}

pub struct Executor {
    tasks: Vec<Pin<Box<dyn TaskTrait>>>,
}
impl Executor {
    pub const fn new() -> Self {
        Self { tasks: Vec::new() }
    }

    pub fn spawn<F>(&mut self, task: F)
    where
        F: Future<Output = ()> + 'static + Send,
    {
        self.tasks.push(Box::pin(Task {
            should_be_polled: true,
            future: Box::pin(task),
        }));
    }

    pub fn poll(&mut self) {
        self.tasks.retain_mut(|task| {
            if !task.should_be_polled() {
                // task is not ready to be polled yet, keep it in the list
                return true;
            }
            let waker = unsafe {
                // SAFETY: todo...
                Waker::new(null(), &WAKER_VTABLE)
            };
            let mut context = Context::from_waker(&waker);
            let poll_res = Future::poll(task.as_mut(), &mut context);
            match poll_res {
                Poll::Ready(()) => {
                    // task has finished executing. don't keep it in the list
                    false
                }
                Poll::Pending => {
                    // task is not ready yet. keep it in the list for now.
                    true
                }
            }
        });
    }

    pub fn is_empty(&self) -> bool {
        self.tasks.is_empty()
    }
}

unsafe fn vtable_clone(data: *const ()) -> RawWaker {
    todo!();
}

unsafe fn vtable_wake(data: *const ()) {
    todo!();
}

unsafe fn vtable_wake_by_ref(data: *const ()) {
    todo!();
}

unsafe fn vtable_drop(data: *const ()) {
    todo!();
}

static WAKER_VTABLE: RawWakerVTable =
    RawWakerVTable::new(vtable_clone, vtable_wake, vtable_wake_by_ref, vtable_drop);
