use core::{
    fmt::{Display, LowerHex},
    marker::PhantomData,
};

pub mod callback_chain;
pub mod write_once;

/// returns a bitmask of the given bit length
#[allow(unused_macros)]
macro_rules! bitmask_of_len {
    ($len: expr) => {
        ((1 << ($len)) - 1)
    };
}
#[allow(unused_imports)]
pub(crate) use bitmask_of_len;

/// returns the max possible value for the given bit length
#[allow(unused_macros)]
macro_rules! max_val_of_bit_len {
    ($len: expr) => {
        ((1 << ($len)) - 1)
    };
}
#[allow(unused_imports)]
pub(crate) use max_val_of_bit_len;

/// a wrapper which when formatted, displayes the wrapped value as hex.
pub struct HexDisplay<T: LowerHex>(pub T);
impl<T: LowerHex> Display for HexDisplay<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "0x{:x}", self.0)
    }
}
impl<T: LowerHex> LowerHex for HexDisplay<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}
impl<T: LowerHex> core::fmt::Debug for HexDisplay<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "0x{:x}", self.0)
    }
}

/// a phantom type which is not `Send`.
pub struct PhantomUnsend {
    phantom: PhantomData<*const ()>,
}
impl PhantomUnsend {
    pub const fn new() -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}
unsafe impl Sync for PhantomUnsend {}

/// a phantom type which is not `Sync`.
pub struct PhantomUnsync {
    phantom: PhantomData<*const ()>,
}
impl PhantomUnsync {
    pub const fn new() -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}
unsafe impl Send for PhantomUnsend {}

/// a phantom type which is not `Send` and also not `Sync`.
pub struct PhantomUnsendUnsync {
    phantom: PhantomData<*const ()>,
}
impl PhantomUnsendUnsync {
    pub const fn new() -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}
