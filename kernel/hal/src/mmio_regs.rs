macro_rules! mmio_base {
    {$base_phys_addr: expr} => {
        /// the physical base address of the memory mapped registers.
        pub const BASE_PHYS_ADDR: $crate::mem::PhysAddr = $base_phys_addr;

        /// the virtual base address of the memory mapped registers.
        /// we use an uncachable address, which is important for mmio addresses.
        pub const BASE_VIRT_ADDR: $crate::mem::VirtAddr = Self::BASE_PHYS_ADDR.kseg_uncachable_addr().unwrap();
    };
}
pub(crate) use mmio_base;

macro_rules! mmio_reg {
    {
        $(#[$outer:meta])*
        $name: ident, $value_ty: ty, $access: ident, $offset: literal
    } => {
        $(#[$outer])*
        pub const fn $name() -> volatile::VolatilePtr<'static, $value_ty, volatile::access::$access> {
            unsafe {
                volatile::VolatilePtr::new_restricted(
                    volatile::access::$access,
                    core::ptr::NonNull::new_unchecked($crate::mem::VirtAddr(Self::BASE_VIRT_ADDR.0 + $offset).as_mut()),
                )
            }
        }

    };
}
pub(crate) use mmio_reg;
