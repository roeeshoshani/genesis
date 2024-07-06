ENTRY(_start)

SECTIONS
{
    . = 0;
    .text : {
        *(.text.loader_entrypoint)
        *(.text) *(.text.*)
    } :code
    END_OF_CODE = .;

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
