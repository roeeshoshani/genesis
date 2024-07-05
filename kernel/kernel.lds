ENTRY(_start)

SECTIONS
{
    . = 0x4000000;
    .text : {
        *(.text) *(.text.*)
    }

    .rodata : {
        *(.rodata)
    }

    .data : {
        *(.data)
    }

    .bss : {
        *(.bss)
    }

    /DISCARD/ : {
        *(.eh_frame)
        *(.note.gnu.build-id)
    }
}
