pub mod async_event;

use core::{
    future::Future,
    pin::Pin,
    sync::atomic::{AtomicBool, Ordering},
    task::{Context, Poll, Waker},
};

use alloc::{boxed::Box, sync::Arc, task::Wake, vec::Vec};

use crate::sync::IrqSpinlock;

pub struct Task {
    should_be_polled: AtomicBool,
    future: IrqSpinlock<Pin<Box<dyn Future<Output = ()> + 'static + Send>>>,
}
impl Wake for Task {
    fn wake(self: Arc<Self>) {
        self.should_be_polled.store(true, Ordering::Relaxed);
    }

    fn wake_by_ref(self: &Arc<Self>) {
        self.should_be_polled.store(true, Ordering::Relaxed);
    }
}

fn poll_task_retain(task: &mut Arc<Task>) -> bool {
    if !task.should_be_polled.swap(false, Ordering::Relaxed) {
        // task is not ready to be polled yet, keep it in the list
        return true;
    }

    let waker = Waker::from(task.clone());
    let mut context = Context::from_waker(&waker);

    let mut future = task.future.lock();

    let poll_res = Future::poll(future.as_mut(), &mut context);
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
    pub const fn new() -> Self {
        Self { tasks: Vec::new() }
    }

    pub fn spawn<F>(&mut self, task: F)
    where
        F: Future<Output = ()> + 'static + Send,
    {
        self.tasks.push(Arc::new(Task {
            should_be_polled: AtomicBool::new(true),
            future: IrqSpinlock::new(Box::pin(task)),
        }));
    }

    pub fn poll(&mut self) {
        self.tasks.retain_mut(poll_task_retain);
    }

    pub fn is_empty(&self) -> bool {
        self.tasks.is_empty()
    }
}

pub static EXECUTOR: IrqSpinlock<Executor> = IrqSpinlock::new(Executor::new());

pub struct SleepForever;
pub async fn sleep_forever() -> SleepForever {
    SleepForever
}
impl Future for SleepForever {
    type Output = ();

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Pending
    }
}
