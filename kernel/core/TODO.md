# TODO list
- implement `NonZeroPhysAddr` or make `PhysAddr` nonzero. this will greatly improve of `Option<PhysAddr>`. the same goes for `VirtAddr`.
- when deallocating memory in the physical memory allocator, actually free the pages back to the page allocator.
- think about implementing a destructor for the physical memory allocator. is it even relevant though? i mean it is a global
  variable anway...
- add support for arrays in bitpiece. probably really hard, but also really useful.
- move all stuff that is only used in `core` and not in `loader` outside of `hal`.
- convert `IrqSpinlock` to `IrqLock`. all we need is to disable interrupts, we don't need the spinlock, since we are single core.
