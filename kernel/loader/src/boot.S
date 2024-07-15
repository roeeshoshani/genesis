.set KSEG0_START, (0x80000000)
.set KSEG1_START, (0xa0000000)
.set STACK_SIZE, (4 * 1024 * 1024)

.section .text.boot

.globl loader_start
loader_start:
// currently we are at address 0xbfc00000.
// at 0xbfc00010 we have a whole in the flash mmmio, which instead decodes to register address revision.
// we don't want our code to collide with this hole, so we skip it.
b after_hole
nop
nop
nop
after_hole:
// set up the stack by making it point to some offset from the start of the physical address space.
// the first 128MB at the start of the physical address space is all mapped to ram, so this ensures that our stack will use ram.
// we are pointing to that physical address through kseg1, which is uncached, since cache is not yet initialized.
li $sp, (KSEG1_START + STACK_SIZE)
b loader_entrypoint

.previous

