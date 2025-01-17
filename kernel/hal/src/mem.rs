use paste::paste;

macro_rules! impl_newtype_operator_full {
    { $newtype: ty, $operator: ident, $fn_name: expr, $operand: ty, $lhs_var_name: ident, $rhs_var_name: ident, $result: expr } => {
        paste! {
            impl ::core::ops::$operator < $operand > for $newtype {
                type Output = Self;
                fn $fn_name(self, $rhs_var_name: $operand) -> Self::Output {
                    let $lhs_var_name = self.0;
                    Self($result)
                }
            }
            impl ::core::ops::[<$operator Assign>] < $operand > for $newtype {
                fn [<$fn_name _assign>](&mut self, $rhs_var_name: $operand) {
                    let $lhs_var_name = self.0;
                    self.0 = $result;
                }
            }
        }
    };
}
macro_rules! impl_newtype_binary_operator {
    { $newtype: ty, $operator: ident, $fn_name: ident, $operand: ty } => {
        impl_newtype_operator_full! {
            $newtype,
            $operator,
            $fn_name,
            $operand,
            lhs,
            rhs,
            <$operand as ::core::ops::$operator>::$fn_name(lhs, rhs)
        }
    };
}
macro_rules! impl_newtype_unary_operator {
    { $newtype: ty, $operator: ident, $fn_name: ident, $inner_ty: ty } => {
        impl ::core::ops::$operator for $newtype {
            type Output = Self;
            fn $fn_name(self) -> Self::Output {
                Self(<$inner_ty as ::core::ops::$operator>::$fn_name(self.0))
            }
        }
    };
}
macro_rules! impl_newtype_all_operators {
    { $newtype: ty, $inner_ty: ty } => {
        impl_newtype_binary_operator! { $newtype, Add, add, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Sub, sub, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Div, div, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Rem, rem, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Mul, mul, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Shr, shr, $inner_ty }
        impl_newtype_binary_operator! { $newtype, Shl, shl, $inner_ty }
        impl_newtype_binary_operator! { $newtype, BitAnd, bitand, $inner_ty }
        impl_newtype_binary_operator! { $newtype, BitOr, bitor, $inner_ty }
        impl_newtype_binary_operator! { $newtype, BitXor, bitxor, $inner_ty }
        impl_newtype_unary_operator! { $newtype, Not, not, $inner_ty }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct PhysAddr(pub usize);
impl_newtype_all_operators! { PhysAddr, usize }
impl PhysAddr {
    /// creates a new physical address with the given value.
    pub const fn new(value: usize) -> Self {
        Self(value)
    }

    /// returns the virtual cachable kseg0 address which maps to this physical address, if such an address even exists.
    /// if the physical address is too large to fit in kseg0, `None` is returned.
    pub const fn kseg_cachable_addr(self) -> Option<VirtAddr> {
        KSEG0.addr_at_offset(self.0)
    }

    /// returns the virtual uncachable kseg1 address which maps to this physical address, if such an address even exists.
    /// if the physical address is too large to fit in kseg1, `None` is returned.
    pub const fn kseg_uncachable_addr(self) -> Option<VirtAddr> {
        KSEG1.addr_at_offset(self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct VirtAddr(pub usize);
impl_newtype_all_operators! { VirtAddr, usize }
impl VirtAddr {
    /// creates a new virtual address with the given value.
    pub const fn new(value: usize) -> Self {
        Self(value)
    }

    /// returns the offset of this virtual address in kseg0, if it is in kseg0.
    pub const fn kseg0_off(self) -> Option<usize> {
        KSEG0.offset_of_addr(self)
    }

    /// returns the physical address of this virtual address in kseg0, if it is in kseg0.
    pub const fn kseg0_phys_addr(self) -> Option<PhysAddr> {
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(off) = self.kseg0_off() else {
            return None;
        };
        Some(PhysAddr(off))
    }

    /// returns the offset of this virtual address in kseg1, if it is in kseg1.
    pub const fn kseg1_off(self) -> Option<usize> {
        KSEG1.offset_of_addr(self)
    }

    /// returns the physical address of this virtual address in kseg1, if it is in kseg1.
    pub const fn kseg1_phys_addr(self) -> Option<PhysAddr> {
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(off) = self.kseg1_off() else {
            return None;
        };
        Some(PhysAddr(off))
    }

    /// converts this virtual address to a pointer.
    pub const fn as_ptr<T>(self) -> *const T {
        self.0 as *const T
    }

    /// converts this virtual address to a mutable pointer.
    pub const fn as_mut_ptr<T>(self) -> *mut T {
        self.0 as *mut T
    }

    /// converts this virtual address to a reference.
    pub const unsafe fn as_ref<T>(self) -> &'static T {
        &*(self.0 as *const T)
    }

    /// converts this virtual address to a mutable reference.
    pub const unsafe fn as_mut<T>(self) -> &'static mut T {
        &mut *(self.0 as *mut T)
    }
}

/// a region of virtual memory.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct VirtMemRegion {
    /// the start address of the region.
    pub start: VirtAddr,

    /// the inclusive end address of the region.
    ///
    /// we use an inclusive end address instead of an exclusive one, since if we use an exclusive end address
    /// we won't be able to represent memory regions that extend to the end of the address space, since their
    /// exclusive end address can't be represented inside a pointer sized integer.
    pub inclusive_end: VirtAddr,
}
impl VirtMemRegion {
    /// creates a new region with the given start address and size
    pub const fn new(start: VirtAddr, size: usize) -> Self {
        Self {
            start,
            inclusive_end: VirtAddr(start.0 + size - 1),
        }
    }

    /// returns the size of the memory region.
    ///
    /// this may fail if the size is too big to be represented as a `usize`.
    pub const fn size(&self) -> Option<usize> {
        (self.inclusive_end.0 - self.start.0).checked_add(1)
    }

    /// returns the exclusive end address of the memory region.
    ///
    /// this may fail if the end address is too big to be represented as a `usize`.
    pub const fn end(&self) -> Option<usize> {
        self.inclusive_end.0.checked_add(1)
    }

    /// returns whether this memory region contains the byte at the given memory address
    pub const fn contains(&self, addr: VirtAddr) -> bool {
        addr.0 >= self.start.0 && addr.0 <= self.inclusive_end.0
    }

    /// returns the offset of the given address within this memory region, if the address is within this memory region.
    pub const fn offset_of_addr(&self, addr: VirtAddr) -> Option<usize> {
        if !self.contains(addr) {
            return None;
        }
        Some(addr.0 - self.start.0)
    }

    /// returns the address at the given offset from the start of the region if the offset is within the bounds of the region.
    pub const fn addr_at_offset(&self, offset: usize) -> Option<VirtAddr> {
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(addr) = self.start.0.checked_add(offset) else {
            return None;
        };
        if addr <= self.inclusive_end.0 {
            Some(VirtAddr(addr))
        } else {
            None
        }
    }
}

/// a region of physical memory.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PhysMemRegion {
    /// the start address of the region.
    pub start: PhysAddr,

    /// the inclusive end address of the region.
    ///
    /// we use an inclusive end address instead of an exclusive one, since if we use an exclusive end address
    /// we won't be able to represent memory regions that extend to the end of the address space, since their
    /// exclusive end address can't be represented inside a pointer sized integer.
    pub inclusive_end: PhysAddr,
}
impl PhysMemRegion {
    /// creates a new region with the given start address and size
    pub const fn new(start: PhysAddr, size: usize) -> Self {
        Self {
            start,
            inclusive_end: PhysAddr(start.0 + size - 1),
        }
    }

    /// returns the size of the memory region.
    ///
    /// this may fail if the size is too big to be represented as a `usize`.
    pub const fn size(&self) -> Option<usize> {
        (self.inclusive_end.0 - self.start.0).checked_add(1)
    }

    /// returns the exclusive end address of the memory region.
    ///
    /// this may fail if the end address is too big to be represented as a `usize`.
    pub const fn end(&self) -> Option<usize> {
        self.inclusive_end.0.checked_add(1)
    }

    /// returns whether this memory region contains the given address
    pub const fn contains(&self, addr: PhysAddr) -> bool {
        addr.0 >= self.start.0 && addr.0 <= self.inclusive_end.0
    }

    /// returns the offset of the given address within this memory region, if the address is within this memory region.
    pub const fn offset_of_addr(&self, addr: PhysAddr) -> Option<usize> {
        if !self.contains(addr) {
            return None;
        }
        Some(addr.0 - self.start.0)
    }

    /// returns the address at the given offset from the start of the region if the offset is within the bounds of the region.
    pub const fn addr_at_offset(&self, offset: usize) -> Option<PhysAddr> {
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(addr) = self.start.0.checked_add(offset) else {
            return None;
        };
        if addr <= self.inclusive_end.0 {
            Some(PhysAddr(addr))
        } else {
            None
        }
    }
}

/// the kuseg memory region. this is a mapped and cacheable memory region accessible from usermode.
pub const KUSEG: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0x0000_0000),
    inclusive_end: VirtAddr(0x7fff_ffff),
};

/// the kseg0 memory region. this is an unmapped and cacheable memory region.
pub const KSEG0: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0x8000_0000),
    inclusive_end: VirtAddr(0x9fff_ffff),
};

/// the kseg1 memory region. this is an unmapped and uncached memory region.
pub const KSEG1: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0xa000_0000),
    inclusive_end: VirtAddr(0xbfff_ffff),
};

/// the kseg2 memory region. this is a mapped and cacheable memory region accessible from supervisor mode.
pub const KSEG2: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0xc000_0000),
    inclusive_end: VirtAddr(0xdfff_ffff),
};

/// the kseg3 memory region. this is a mapped and cacheable memory region.
pub const KSEG3: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0xe000_0000),
    inclusive_end: VirtAddr(0xffff_ffff),
};

/// a memory region which is a concatenation of kseg2 and kseg3.
///
/// kseg2 and kseg3 are both mapped and cachable. the only difference between them is that kseg2 can also be
/// accessed from supervisor mode, and not only from kernelmode.
///
/// but, we do not use supervisor mode in this kernel, so as far as we are concerened, these 2 regions have
/// the exact same properties. so, we treat them as a single, continuous region.
pub const KSEG23: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0xc000_0000),
    inclusive_end: VirtAddr(0xffff_ffff),
};

/// the base address of the exception vector.
pub const EXCEPTION_VECTOR_BASE: PhysAddr = PhysAddr(0);

/// the offset of the general exception vector from the exception vector base address.
pub const GENERAL_EXCEPTION_VECTOR_OFFSET: usize = 0x180;

/// the address of the general exception vector
pub const GENERAL_EXCEPTION_VECTOR_ADDR: PhysAddr =
    PhysAddr(EXCEPTION_VECTOR_BASE.0 + GENERAL_EXCEPTION_VECTOR_OFFSET);

/// the region of padding to leave at the start of the physical address space to avoid overwriting the exception vector.
pub const EXCEPTION_VECTOR_PADDING: PhysMemRegion =
    PhysMemRegion::new(EXCEPTION_VECTOR_BASE, 0x300);

/// the physical memory region of the kernel stack.
/// we point the stack to the start of the physical address space right after the exception vector.
/// the first 128MB at the start of the physical address space are all mapped to ram, so this ensures that our stack will use ram.
pub const KERNEL_STACK: PhysMemRegion =
    PhysMemRegion::new(EXCEPTION_VECTOR_PADDING.inclusive_end, 8 * 1024 * 1024);

/// the physical address of the memory mapped mips revision register.
pub const MIPS_REVISION_REG_ADDR: PhysAddr = PhysAddr(0x1FC00010);

/// the physical memory region which represents the memory mapped region which provides access to the PCI memory
/// space of the PCI bus through the PCI_0 device.
///
/// the PCI_0 device is a PCI device which is part of the GT-64120A controller, and is controlled by the cpu.
/// this device can thus be used by the cpu to access the PCI bus of the board.
///
/// the memory region defined here uses the default addresses for this memory region, although the ranges can be
/// configured using specific hardware registers. but, we never write to these registers, and we just use the
/// default memory region addresses.
pub const PCI_0_MEM: PhysMemRegion = PhysMemRegion {
    start: PhysAddr(0x12000000),
    inclusive_end: PhysAddr(0x14000000),
};

/// the physical memory region which represents the memory mapped region which provides access to the PCI IO
/// space of the PCI bus through the PCI_0 device.
///
/// the PCI_0 device is a PCI device which is part of the GT-64120A controller, and is controlled by the cpu.
/// this device can thus be used by the cpu to access the PCI bus of the board.
///
/// the memory region defined here uses the default addresses for this memory region, although the ranges can be
/// configured using specific hardware registers. but, we never write to these registers, and we just use the
/// default memory region addresses.
pub const PCI_0_IO: PhysMemRegion = PhysMemRegion {
    start: PhysAddr(0x10000000),
    inclusive_end: PhysAddr(0x12000000),
};
