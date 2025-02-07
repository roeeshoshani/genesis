use core::{pin::Pin, ptr::NonNull};

use alloc::boxed::Box;

use crate::sync::IrqLock;

type CallbackLink = Option<NonNull<Callback>>;

pub trait CallbackChainFn: Fn() + 'static + Send + Sync {}
impl<T: Fn() + 'static + Send + Sync> CallbackChainFn for T {}

struct Callback {
    link: CallbackLink,
    link_of_prev: NonNull<CallbackLink>,
    function: Box<dyn CallbackChainFn>,
    chain: &'static CallbackChain,
}

pub struct CallbackChain {
    head_link: IrqLock<CallbackLink>,
}
impl CallbackChain {
    pub const fn new() -> Self {
        Self {
            head_link: IrqLock::new(None),
        }
    }
    pub fn register<F>(&'static self, callback: F) -> CallbackChainNode
    where
        F: CallbackChainFn,
    {
        let mut head_link = self.head_link.lock();
        let mut callback = Box::pin(Callback {
            link: *head_link,
            link_of_prev: NonNull::from(&mut *head_link),
            function: Box::new(callback),
            chain: self,
        });
        *head_link = Some(NonNull::from(&mut *callback));
        CallbackChainNode { callback }
    }

    pub fn trigger(&'static self) {
        let head_link = self.head_link.lock();

        let mut cursor = *head_link;
        while let Some(cur_link) = cursor {
            let cur_callback = unsafe {
                // SAFETY: all pointers in the list are valid. also, we took the lock, which guarantees that we have exclusive access.
                cur_link.as_ref()
            };
            (cur_callback.function)();
            cursor = cur_callback.link;
        }
    }
}

unsafe impl Send for CallbackChain {}
unsafe impl Sync for CallbackChain {}

pub struct CallbackChainNode {
    callback: Pin<Box<Callback>>,
}
impl Drop for CallbackChainNode {
    fn drop(&mut self) {
        // when dropping the node, we want to unlink it from the list.

        // grab the chain's lock before modifying pointers
        let chain = self.callback.chain;
        let _head_link = chain.head_link.lock();

        // make the previous node point to the next node.
        unsafe {
            // SAFETY: all pointers in the list are valid. also, we took the lock, which guarantees that we have exclusive access.
            self.callback.link_of_prev.write(self.callback.link);
        }

        // make the next node point back to the previous node.
        if let Some(mut link_ptr) = self.callback.link {
            let link = unsafe { link_ptr.as_mut() };
            link.link_of_prev = self.callback.link_of_prev;
        }
    }
}
