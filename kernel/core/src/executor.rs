use core::{
    cell::UnsafeCell,
    future::Future,
    ops::Deref,
    pin::Pin,
    ptr::null,
    sync::atomic::{AtomicBool, Ordering},
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
};

use alloc::{boxed::Box, sync::Arc, vec::Vec};

use crate::sync::IrqSpinlock;

pub struct Task {
    should_be_polled: AtomicBool,

    // using an unsafe cell here since this part is only accessed by the executor, so it doesn't need synchronization.
    future: UnsafeCell<Pin<Box<dyn Future<Output = ()> + 'static + Send>>>,
}

// SAFETY: we only access the future from within the executor.
unsafe impl Sync for Task {}

fn poll_task_retain(task: &mut Arc<Task>) -> bool {
    if task.should_be_polled.swap(false, Ordering::Relaxed) {
        // task is not ready to be polled yet, keep it in the list
        return true;
    }

    // get a refcount on the task for this waker
    let task_clone = task.clone();
    let waker = unsafe {
        // SAFETY: todo...
        Waker::new(Arc::into_raw(task_clone).cast(), &WAKER_VTABLE)
    };

    let mut context = Context::from_waker(&waker);
    let future = unsafe {
        // SAFETY: we only access the future from within the executor.
        (*task.future.get()).as_mut()
    };
    let poll_res = Future::poll(future, &mut context);
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
}

pub struct Executor {
    // the pinning of the task here is so that we can pass a pointer to the task as the data pointer of the waker.
    tasks: Vec<Arc<Task>>,
}
impl Executor {
    // do not construct manually. use the global executor. only one instance may exist.
    const fn new() -> Self {
        Self { tasks: Vec::new() }
    }

    pub fn spawn<F>(&mut self, task: F)
    where
        F: Future<Output = ()> + 'static + Send,
    {
        self.tasks.push(Arc::new(Task {
            should_be_polled: AtomicBool::new(true),
            future: UnsafeCell::new(Box::pin(task)),
        }));
    }

    pub fn poll(&mut self) {
        self.tasks.retain_mut(poll_task_retain);
    }

    pub fn is_empty(&self) -> bool {
        self.tasks.is_empty()
    }

    pub fn wake_up(&mut self, waker_data: *const ()) {
        todo!()
    }
}

pub static EXECUTOR: IrqSpinlock<Executor> = IrqSpinlock::new(Executor::new());

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
