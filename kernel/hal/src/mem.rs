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

    /// returns the virtual kseg0 address which maps to this physical address, if such an address even exists.
    /// if the physical address is too large to fit in kseg0, `None` is returned.
    pub const fn kseg0_addr(self) -> Option<VirtAddr> {
        KSEG0.addr_at_offset(self.0)
    }

    /// returns the virtual kseg1 address which maps to this physical address, if such an address even exists.
    /// if the physical address is too large to fit in kseg1, `None` is returned.
    pub const fn kseg1_addr(self) -> Option<VirtAddr> {
        KSEG1.addr_at_offset(self.0)
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

    /// the end address of the region.
    pub end: VirtAddr,
}
impl VirtMemRegion {
    /// creates a new region with the given start address and size
    pub const fn new(start: VirtAddr, size: usize) -> Self {
        Self {
            start,
            end: VirtAddr(start.0 + size),
        }
    }

    /// returns the size of the region
    pub const fn size(&self) -> usize {
        self.end.0 - self.start.0
    }

    /// returns whether this memory region contains the given address
    pub const fn contains(&self, addr: VirtAddr) -> bool {
        addr.0 >= self.start.0 && addr.0 < self.end.0
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
        if offset >= self.size() {
            return None;
        }
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(addr) = self.start.0.checked_add(offset) else {
            return None;
        };
        Some(VirtAddr(addr))
    }
}

/// a region of physical memory.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PhysMemRegion {
    /// the start address of the region.
    pub start: PhysAddr,

    /// the end address of the region.
    pub end: PhysAddr,
}
impl PhysMemRegion {
    /// creates a new region with the given start address and size
    pub const fn new(start: PhysAddr, size: usize) -> Self {
        Self {
            start,
            end: PhysAddr(start.0 + size),
        }
    }

    /// returns the size of the region
    pub const fn size(&self) -> usize {
        self.end.0 - self.start.0
    }

    /// returns whether this memory region contains the given address
    pub const fn contains(&self, addr: PhysAddr) -> bool {
        addr.0 >= self.start.0 && addr.0 < self.end.0
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
        if offset >= self.size() {
            return None;
        }
        // can't use `?` here since using it in const functions is not stable yet.
        let Some(addr) = self.start.0.checked_add(offset) else {
            return None;
        };
        Some(PhysAddr(addr))
    }
}

/// the kseg0 memory region. this is an unmapped and cacheable memory region.
pub const KSEG0: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0x8000_0000),
    end: VirtAddr(0xa000_0000),
};

/// the kseg1 memory region. this is an unmapped and uncached memory region.
pub const KSEG1: VirtMemRegion = VirtMemRegion {
    start: VirtAddr(0xa000_0000),
    end: VirtAddr(0xc000_0000),
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
    PhysMemRegion::new(EXCEPTION_VECTOR_PADDING.end, 8 * 1024 * 1024);

/// the physical address of the memory mapped mips revision register.
pub const MIPS_REVISION_REG_ADDR: PhysAddr = PhysAddr(0x1FC00010);
