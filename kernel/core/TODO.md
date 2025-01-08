# TODO list
- there is a bug with PCI BAR size calculation. when taking the `address` of the BAR, due to how `bitpiece` works,
  i get the address value shifted to the right from its original position within the value.

  due to this fact, when i then apply bitwise not to this value, its msbs, which are zero due to the shift right,
  now become 1 bits, which makes it a huge value.

  the solution to this is to add a feature to bitpiece which adds another getter method for each field, but with
  a `_shifted` postfix. for example, for the pci bar, i will have `address_shifted`. what this method will do,
  is it will take the storage value, and mask it according to the shifted mask for the specific field. this
  will return only the bits relevant to this field, but in their original location in the storage. this is exactly
  what i want here, and currently there's no straightforward way to get this value.
  the return value of the method is the storage type used by the bitpiece.

  then, the code will work. apparently, i don't even need to shift anything to calculate the BAR size, i only need
  to mask out the lsbs, then bitwise not and add 1.
