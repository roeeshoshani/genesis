ENTRY(_start)

SECTIONS
{
    /* 
     * we use an address that is only page aligned, and not aligned to any higher value, to prevent the compiler from making
     * assumptions about the alignment of the program counter, which can cause problems with pc-region relative instructions.
     */
    . = 0xbfc00000;
    .text : {
        *(.text.boot)
        *(.text) *(.text.*)
    } :code
    REFERENCE_POINT_OFFSET_FROM_END = . - reference_point;

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
        *(.got)
    }
}

PHDRS {
    code PT_LOAD;
}
