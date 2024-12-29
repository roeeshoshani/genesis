ENTRY(_start)

PHDRS {
    all PT_LOAD;
}

SECTIONS
{
    .initialized : { *(.text*) *(.rodata*) *(.sdata*) *(.data*) } : all
    .got : { *(.got*) } : all
    .uninitialized : { *(.bss*) *(.sbss*) } : all

    .MIPS.abiflags : { *(.MIPS.abiflags) } : NONE
    .reginfo : { *(.reginfo) } : NONE
    .dynsym : { *(.dynsym) } : NONE
    .hash : { *(.hash) } : NONE
    .dynamic : { *(.dynamic) } : NONE
    .dynstr : { *(.dynstr) } : NONE
    .rld_map : { *(.rld_map) } : NONE
    .rel.dyn : { *(.rel.*) } : NONE
}
