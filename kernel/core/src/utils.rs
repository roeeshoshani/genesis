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
