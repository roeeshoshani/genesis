pub mod async_event;

use core::{
    future::Future,
    pin::Pin,
    sync::atomic::{AtomicBool, Ordering},
    task::{Context, Poll, Waker},
};

use alloc::{boxed::Box, sync::Arc, task::Wake, vec::Vec};

use crate::sync::{IrqLock, NonIrqLock};

pub struct Task {
    should_be_polled: AtomicBool,

    /// the underlying future of this task.
    ///
    /// we use a non irq lock since futures can only be polled in task context by definition.
    future: NonIrqLock<Pin<Box<dyn Future<Output = ()> + 'static + Send>>>,
}
impl Wake for Task {
    fn wake(self: Arc<Self>) {
        // we use release ordering because we want the task, once it wakes up, to see all preperations that were made before it
        // was woken up.
        self.should_be_polled.store(true, Ordering::Release);
    }

    fn wake_by_ref(self: &Arc<Self>) {
        // we use release ordering because we want the task, once it wakes up, to see all preperations that were made before it
        // was woken up.
        self.should_be_polled.store(true, Ordering::Release);
    }
}

fn poll_task_retain(task: &mut Arc<Task>, did_anything: &mut bool) -> bool {
    if !task.should_be_polled.swap(false, Ordering::Relaxed) {
        // task is not ready to be polled yet, keep it in the list
        return true;
    }

    let waker = Waker::from(task.clone());
    let mut context = Context::from_waker(&waker);

    let mut future = task.future.lock();

    let poll_res = Future::poll(future.as_mut(), &mut context);

    // we polled a task, which counts as progress
    *did_anything = true;

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
            future: NonIrqLock::new(Box::pin(task)),
        }));
    }

    pub fn poll(&mut self) {
        loop {
            let mut did_anything = false;
            self.tasks
                .retain_mut(|task| poll_task_retain(task, &mut did_anything));
            if !did_anything {
                break;
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.tasks.is_empty()
    }
}

/// the global executor.
///
/// it is protected using a non irq lock because we must be able to run tasks with interrupts enabled.
/// this means that it can't be accessed from interrupt context, which means that we can't spawn tasks from interrupt context.
pub static EXECUTOR: NonIrqLock<Executor> = NonIrqLock::new(Executor::new());

pub struct SleepForever;
pub fn sleep_forever() -> SleepForever {
    SleepForever
}
impl Future for SleepForever {
    type Output = ();

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Pending
    }
}
