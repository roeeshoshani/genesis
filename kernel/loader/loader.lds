ENTRY(_start)

SECTIONS
{
    . = 0;
    .text : {
        *(.text) *(.text.*)
    }
    END_OF_CODE = .;

    .rodata : {
        *(.rodata)
        *(.rodata.*)
        *(.got)
        *(.got.*)
    }

    .data : {
        *(.data)
        *(.data.*)
    }

    .bss : {
        *(.bss)
        *(.bss.*)
    }

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
