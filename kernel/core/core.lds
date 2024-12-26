ENTRY(_start)

PHDRS {
    all PT_LOAD;
}

SECTIONS
{
    .text : {
        *(.text) *(.text.*)
    } : all

    .rodata : {
        *(.rodata)
        *(.rodata.*)
    } : all

    .got : {
        *(.got)
        *(.got.*)
    } : all

    .data : {
        *(.data)
        *(.data.*)
    } : all

    .bss : {
        *(.bss)
        *(.bss.*)
    } : all

    .rel : {
        *(.rel.*)
    }

    /DISCARD/ : {
        *(.eh_frame)
        *(.note.gnu.build-id)
        *(.MIPS.abiflags)
        *(.reginfo)
        *(.hash)
        *(.mdebug.abi32)
        *(.pdr)
        *(.dynamic)
        *(.dynsym)
        *(.dynstr)
        *(.rld_map)
        *(.comment)
        *(.debug_abbrev)
        *(.debug_info)
        *(.debug_aranges)
        *(.debug_ranges)
        *(.debug_str)
        *(.debug_frame)
        *(.debug_line)
    }
}
