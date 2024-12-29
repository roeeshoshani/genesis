ENTRY(_start)

PHDRS {
    all PT_LOAD;
    trash PT_LOAD;
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

    .MIPS.abiflags : { *(.MIPS.abiflags) } : trash
    .reginfo : { *(.reginfo) } : trash
    .dynsym : { *(.dynsym) } : trash
    .hash : { *(.hash) } : trash
    .dynamic : { *(.dynamic) } : trash
    .dynstr : { *(.dynstr) } : trash
    .rld_map : { *(.rld_map) } : trash
    .rel.dyn : { *(.rel.*) } : trash
}
