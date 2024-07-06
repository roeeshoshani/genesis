#![cfg_attr(not(feature = "std"), no_std)]

use binary_serde::BinarySerde;

#[derive(BinarySerde, Debug, Clone)]
#[repr(C)]
pub struct LoaderInfoHeader {
    /// the amount of relative relocations.
    pub relocations_amount: u32,

    /// the total size of the info provided to the loader, including this header and all the information that follows it.
    pub loader_info_total_size: u32,

    /// the offset of the entry point inside the wrapped shellcode blob.
    pub entry_point_offset: u32,
}
