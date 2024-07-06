.section .text.boot

.globl _start
_start:
// currently we are at address 0xbfc00000.
// at 0xbfc00010 we have a whole in the flash mmmio, which instead decodes to register address revision.
// we don't want our code to collide with this hole, so we skip it.
b after_hole
nop
nop
nop
after_hole:
// make the stack point to physical address 0x400000. this address is mapped to ram and thus can be used as stack space.
li $sp, 0xa0400000

.previous

