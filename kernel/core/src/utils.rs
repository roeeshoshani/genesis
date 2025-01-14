use core::fmt::{Display, LowerHex};

/// returns a bitmask of the given bit length
macro_rules! bitmask_of_len {
    ($len: expr) => {
        ((1 << ($len)) - 1)
    };
}
pub(crate) use bitmask_of_len;

/// returns the max possible value for the given bit length
macro_rules! max_val_of_bit_len {
    ($len: expr) => {
        ((1 << ($len)) - 1)
    };
}
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
