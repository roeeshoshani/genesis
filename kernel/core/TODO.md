# TODO list
- implement `NonZeroPhysAddr` or make `PhysAddr` nonzero. this will greatly improve of `Option<PhysAddr>`. the same goes for `VirtAddr`.
- when deallocating memory in the physical memory allocator, actually free the pages back to the page allocator.
- think about implementing a destructor for the physical memory allocator. is it even relevant though? i mean it is a global
  variable anway...
- add support for arrays in bitpiece. probably really hard, but also really useful.
