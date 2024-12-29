#![cfg_attr(not(feature = "std"), no_std)]

use binary_serde::BinarySerde;

#[derive(BinarySerde, Debug, Clone)]
#[repr(C)]
pub struct LoaderInfoHeader {
    /// the amount of relative relocations.
    pub relocations_amount: u32,

    /// the size of the initialized part of the wrapped code.
    pub initialized_size: u32,

    /// the size of the uninitialized part following the initialized part of the wrapped code.
    pub uninitialized_size: u32,

    /// the offset of the entry point inside the wrapped code.
    pub entry_point_offset: u32,
}

#[derive(BinarySerde, Debug, Clone)]
#[repr(C)]
pub struct LoaderEncodedRel {
    /// the offset, from the start of the shellcode that is being loaded, of the location to which the relocation is to be applied.
    pub offset: u32,
}
