.globl _start
_start:
// make the stack point to the top of the kseg1 memory range.
// the kernel boot code is at `0xbfc00000`, so this gives us a 4MB stack.
li $sp, 0xc0000000

