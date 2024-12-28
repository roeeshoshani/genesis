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
        *(COMMON)
    } : all

    .rel (NOLOAD) : {
        *(.rel.*)
    } : NONE

    .debug_abbrev (NOLOAD) : {
        *(.debug_abbrev)
    } : NONE
    .debug_info (NOLOAD) : {
        *(.debug_info)
    } : NONE
    .debug_aranges (NOLOAD) : {
        *(.debug_aranges)
    } : NONE
    .debug_ranges (NOLOAD) : {
        *(.debug_ranges)
    } : NONE
    .debug_str (NOLOAD) : {
        *(.debug_str)
    } : NONE
    .debug_frame (NOLOAD) : {
        *(.debug_frame)
    } : NONE
    .debug_line (NOLOAD) : {
        *(.debug_line)
    } : NONE
    .debug_loc (NOLOAD) : {
        *(.debug_loc)
    } : NONE

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
    }
}
