#![no_std]
#![no_main]

use core::{panic::PanicInfo, ptr::addr_of_mut};

use loader_shared::LoaderInfoHeader;

#[repr(C)]
struct EncodedRel {
    offset: usize,
    addend: isize,
}

/// the signature of the entry point of the wrapped code.
pub type Entrypoint = unsafe extern "C" fn();

extern "C" {
    static mut END_OF_CODE: u8;
}

/// the entrypoint of the shellcode loader.
///
/// this must be the first function defined in this file, so that it will be placed at offset 0.
#[no_mangle]
unsafe extern "C" fn _start() {
    let end_of_code_ptr = addr_of_mut!(END_OF_CODE);
    let mut cursor = Cursor::new(end_of_code_ptr);
    let info = cursor.align_and_extract_struct::<LoaderInfoHeader>();

    // find the address of the wrapped code by skipping the entire loader info
    let wrapped_code_ptr = end_of_code_ptr.add(info.loader_info_total_size as usize);

    // fill the uninitialized memory with zeroes.
    let uninitialized_data_ptr = wrapped_code_ptr.add(info.initialized_size as usize);
    let uninitialized_data =
        core::slice::from_raw_parts(uninitialized_data_ptr, info.uninitialized_size as usize);
    uninitialized_data.fill(0);

    // parse and apply relocations
    let relocations =
        cursor.align_and_extract_slice::<EncodedRel>(info.relocations_amount as usize);
    for relocation in relocations {
        let relocated_value = &mut *wrapped_code_ptr.add(relocation.offset).cast::<usize>();

        // to apply the relocation, just add the code address to the memory location.
        *relocated_value += wrapped_code_ptr as usize;
    }

    let entrypoint: Entrypoint =
        core::mem::transmute(wrapped_code_ptr.add(info.entry_point_offset as usize));
    entrypoint()
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    loop {}
}

struct Cursor {
    cur_ptr: *mut u8,
}
impl Cursor {
    fn new(ptr: *mut u8) -> Self {
        Self { cur_ptr: ptr }
    }
    unsafe fn advance(&mut self, amount: usize) {
        self.cur_ptr = self.cur_ptr.add(amount);
    }
    unsafe fn advance_struct<T>(&mut self) {
        self.advance(core::mem::size_of::<T>())
    }
    unsafe fn align_and_extract_struct<T>(&mut self) -> &'static T {
        self.align::<T>();
        self.extract_struct()
    }
    unsafe fn extract_struct<T>(&mut self) -> &'static T {
        let result = &*self.cur_ptr.cast();
        self.advance_struct::<T>();
        result
    }
    unsafe fn advance_slice<T>(&mut self, len: usize) {
        self.advance(core::mem::size_of::<T>() * len)
    }
    unsafe fn align_and_extract_slice<T>(&mut self, len: usize) -> &'static [T] {
        self.align::<T>();
        self.extract_slice(len)
    }
    unsafe fn extract_slice<T>(&mut self, len: usize) -> &'static [T] {
        let result = core::slice::from_raw_parts(self.cur_ptr.cast(), len);
        self.advance_slice::<T>(len);
        result
    }
    unsafe fn align<T>(&mut self) {
        self.cur_ptr = self
            .cur_ptr
            .add(self.cur_ptr.align_offset(core::mem::align_of::<T>()))
    }
}
