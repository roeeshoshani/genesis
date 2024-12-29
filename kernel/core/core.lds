ENTRY(_start)

PHDRS {
    all PT_LOAD;
}

SECTIONS
{
    .text : { *(.text*) } : all
    .rodata : { *(.rodata*) } : all
    .data : { *(.data*) } : all
    .sdata : { *(.sdata*) } : all
    .got : { *(.got*) } : all
    .sbss : { *(.sbss*) } : all
    .bss : { *(.bss*) } : all

    .MIPS.abiflags : { *(.MIPS.abiflags) } : NONE
    .reginfo : { *(.reginfo) } : NONE
    .dynsym : { *(.dynsym) } : NONE
    .hash : { *(.hash) } : NONE
    .dynamic : { *(.dynamic) } : NONE
    .dynstr : { *(.dynstr) } : NONE
    .rld_map : { *(.rld_map) } : NONE
    .rel.dyn : { *(.rel.*) } : NONE
}
