use std::{os::unix::process::CommandExt, path::Path, str::FromStr};

use anyhow::{anyhow, bail, Context, Result};
use binary_serde::{BinarySerde, BinarySerializerToVec};
use clap::Parser;
use devx_cmd::{cmd, run};
use loader_shared::{LoaderEncodedRel, LoaderInfoHeader};
use object::{
    elf::{
        Dyn32, DT_MIPS_BASE_ADDRESS, DT_MIPS_CONFLICT, DT_MIPS_LOCAL_GOTNO, PT_LOAD, R_MIPS_REL32,
        SHT_REL, SHT_RELA,
    },
    read::elf::{Dyn, ElfFile, ElfFile32, FileHeader, ProgramHeader, Rel, SectionHeader},
    Endian, Object,
};

const PRE_POST_PROCESSING_KERNEL_FILE_PATH: &str =
    "target/target/release/pre_post_processing_kernel";
const FINAL_KERNEL_FILE_PATH: &str = "target/target/release/kernel";
const KERNEL_ELF_FILE_PATH: &str = "core/target/target/release/core";
const LOADER_ELF_FILE_PATH: &str = "loader/target/target/release/loader";

#[derive(Parser)]
enum Cli {
    Build,
    Run,
    Gdb,
}
fn main() {
    if let Err(err) = main_fallible() {
        eprintln!("error: {:?}", err);
    }
}

fn main_fallible() -> Result<()> {
    let cli = Cli::parse();
    match cli {
        Cli::Build => build()?,
        Cli::Run => run()?,
        Cli::Gdb => gdb()?,
    }
    Ok(())
}

/// a wrapper which provides nicer errors.
fn read_file_to_string<P: AsRef<Path>>(path: P) -> Result<String> {
    std::fs::read_to_string(path.as_ref())
        .context(format!("failed to read file {}", path.as_ref().display()))
}

/// a wrapper which provides nicer errors.
fn read_file<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    std::fs::read(path.as_ref()).context(format!("failed to read file {}", path.as_ref().display()))
}

fn read_and_parse_file<T: FromStr, P: AsRef<Path>>(path: P) -> Result<T>
where
    <T as FromStr>::Err: Sync + Send + std::error::Error + 'static,
{
    read_file_to_string(path.as_ref())?.parse().context(format!(
        "failed to parse content of file {}",
        path.as_ref().display()
    ))
}

fn gdb() -> Result<()> {
    let error = std::process::Command::new("gdb-multiarch")
        .arg("-x")
        .arg("src/gdb_script")
        .exec();
    Err(error).context("failed to run gdb command")
}

fn run() -> Result<()> {
    build()?;
    run!(
        "qemu-system-mipsel",
        // use the malta machine
        "-M",
        "malta",
        // the malta FPGA UART serial uses the third serial port, and we don't really care about the first two serial ports,
        // so ignore them.
        "-serial",
        "null",
        "-serial",
        "null",
        // open the FPGA UART serial in a virtual console (as part of qemu's GUI).
        "-serial",
        "vc",
        // provide the final kernel file as the firmware for the emulator
        "-bios",
        FINAL_KERNEL_FILE_PATH,
        // gdb stub on tcp port 1234
        "-s",
    )?;
    Ok(())
}

fn creare_parent_dirs_and_write_file<P: AsRef<Path>, C: AsRef<[u8]>>(
    path: P,
    content: C,
) -> Result<()> {
    let path = path.as_ref();
    std::fs::create_dir_all(path.parent().context(format!(
        "failed to get parent dir of path {}",
        path.display()
    ))?)
    .context(format!(
        "failed to create parent directory of path {}",
        path.display()
    ))?;
    std::fs::write(path, content).context(format!("failed to write to file {}", path.display()))?;
    Ok(())
}

fn build() -> Result<()> {
    // build and pack the kernel elf into a shellcode
    let kernel_content = build_and_pack_kelf_content()?;

    // write the pre post-processing content
    creare_parent_dirs_and_write_file(
        PRE_POST_PROCESSING_KERNEL_FILE_PATH,
        kernel_content.as_slice(),
    )?;

    // post process the content
    let post_processed_content = post_process_kernel_content(kernel_content);

    // write the final content to a file.
    creare_parent_dirs_and_write_file(FINAL_KERNEL_FILE_PATH, post_processed_content)?;

    Ok(())
}

fn post_process_kernel_content(mut content: Vec<u8>) -> Vec<u8> {
    // for some reason, when passing the kernel image using the `-bios` flag to `qemu`, it decides to swap the endianness
    // of every 4 bytes of the image. to undo this effect, we swap the bytes ourselves.
    buf_align_len(&mut content, 4);
    for chunk in content.chunks_mut(4) {
        let value = u32::from_le_bytes(chunk.try_into().unwrap());
        let swapped_bytes = value.to_be_bytes();
        chunk.copy_from_slice(&swapped_bytes);
    }
    content
}

fn build_and_pack_kelf_content() -> Result<Vec<u8>> {
    let loader_code = build_and_extract_loader_code()?;
    cmd!("cargo", "build", "--release")
        .current_dir("core")
        .run()?;
    let kelf_content = read_file(KERNEL_ELF_FILE_PATH)?;
    pack_kelf_file(&kelf_content, &loader_code).context(format!(
        "failed to pack kernel elf file {KERNEL_ELF_FILE_PATH}"
    ))
}

fn build_and_extract_loader_code() -> Result<Vec<u8>> {
    // build the loader. this must be done in release mode so that the compiler will optimize everything out and we are only left
    // with a .text section. if we don't do this we get a whole bunch of extra sections due to linking with rust's stdlib.
    cmd!("cargo", "build", "--release")
        .current_dir("loader")
        .run()?;
    let loader_elf_content = read_file(LOADER_ELF_FILE_PATH)?;
    Ok(extract_loader_code(&loader_elf_content)
        .context(format!(
            "failed to extract loader code from loader elf file {LOADER_ELF_FILE_PATH}"
        ))?
        .to_vec())
}

fn extract_loader_code(loader_elf_content: &[u8]) -> Result<&[u8]> {
    let elf = ElfFile32::<object::Endianness>::parse(loader_elf_content)
        .context("failed to parse elf")?;

    let phdrs = elf.elf_program_headers();
    if phdrs.len() != 1 {
        bail!("expected a single phdr but found multiple");
    }

    let phdr0 = &phdrs[0];

    let content = phdr0
        .data(elf.endian(), elf.data())
        .map_err(|_| anyhow!("failed to get content phdr"))?;

    Ok(content)
}

fn object_endianness_to_binary_serde(endianness: object::Endianness) -> binary_serde::Endianness {
    match endianness {
        object::Endianness::Little => binary_serde::Endianness::Little,
        object::Endianness::Big => binary_serde::Endianness::Big,
    }
}

fn pack_kelf_file(kelf_content: &[u8], loader_code: &[u8]) -> Result<Vec<u8>> {
    let elf =
        ElfFile32::<object::Endianness>::parse(kelf_content).context("failed to parse elf")?;
    let phdrs_info = process_elf_phdrs(&elf)?;
    let rels_info = process_elf_relocs(&elf)?;
    let loader_info = LoaderInfoHeader {
        relocations_amount: rels_info.encoded_rels_amount as u32,
        initialized_size: phdrs_info.content.len() as u32,
        uninitialized_size: phdrs_info.uninitialized_size as u32,
        entry_point_offset: elf.elf_header().e_entry(elf.endian()),
    };

    let mut serializer =
        BinarySerializerToVec::new(object_endianness_to_binary_serde(elf.endian()));

    // add the code of the loader
    serializer.buffer_mut().extend_from_slice(loader_code);

    // add the loader info header
    buf_align_len_to_type::<LoaderInfoHeader>(serializer.buffer_mut());
    serializer.serialize(&loader_info);

    // add the encoded relocation
    buf_align_len(serializer.buffer_mut(), 4);
    serializer
        .buffer_mut()
        .extend_from_slice(&rels_info.encoded_rels);

    // add the wrapped code
    serializer
        .buffer_mut()
        .extend_from_slice(&phdrs_info.content);

    Ok(serializer.into_buffer())
}

fn process_elf_phdrs<'a>(elf: &ElfFile32<'a>) -> Result<PhdrsInfo<'a>> {
    let phdrs = elf.elf_program_headers();

    if phdrs.is_empty() {
        bail!("no program header");
    }

    // take the first phdr
    let phdr0 = &phdrs[0];

    // make sure that the first phdr is loadable
    if phdr0.p_type(elf.endian()) != PT_LOAD {
        bail!("the first phdr must be loadable");
    }

    // make sure that the first phdr starts at address 0
    if phdr0.p_vaddr(elf.endian()) != 0 {
        bail!("the virtual address of the first phdr must be 0");
    }

    // make sure that the rest of the phdrs are not loadable
    if phdrs
        .iter()
        .skip(1)
        .any(|phdr| phdr.p_type(elf.endian()) == PT_LOAD)
    {
        bail!("there is more than one loadable phdr");
    }

    Ok(PhdrsInfo {
        content: phdr0
            .data(elf.endian(), elf.data())
            .map_err(|_| anyhow!("failed to get phdr content"))?,
        uninitialized_size: (phdr0.p_memsz(elf.endian()) - phdr0.p_filesz(elf.endian())) as usize,
    })
}

fn find_dynamic_entry<E: Endian>(dynamic_entries: &[Dyn32<E>], tag: u32, endian: E) -> Result<u32> {
    let entry = dynamic_entries
        .iter()
        .find(|entry| entry.d_tag.get(endian) == tag)
        .context(format!("failed to find dynamic entry with tag 0x{:x}", tag))?;

    Ok(entry.d_val(endian))
}

fn process_elf_relocs(elf: &ElfFile32) -> Result<RelsInfo> {
    let mut rel_encoder = RelEncoder::new(elf);

    let shdrs = elf.elf_section_table();

    // make sure that we don't have any rela sections.
    // mips does not support rela, only rel
    if shdrs
        .iter()
        .any(|shdr| shdr.sh_type(elf.endian()) == SHT_RELA)
    {
        bail!("rela sections are not supported");
    }

    for shdr in shdrs.iter() {
        if shdr.sh_type(elf.endian()) != SHT_REL {
            continue;
        }

        let (rels, _) = shdr
            .rel(elf.endian(), elf.data())
            .context("failed to parse relocation section")?
            .unwrap();

        for rel in rels {
            let rel_type = rel.r_type(elf.endian());
            if rel_type != R_MIPS_REL32 {
                bail!("unsupported relocation type {}", rel_type);
            }

            // encode the relocation.
            rel_encoder.encode(rel.r_offset(elf.endian()));
        }
    }

    // find the GOT section
    let (_, got_shdr) = shdrs
        .section_by_name(elf.endian(), b".got")
        .context("no .got section")?;

    // find and parse the dynamic section
    let (_, dynamic_shdr) = shdrs
        .section_by_name(elf.endian(), b".dynamic")
        .context("no .dynamic section")?;

    let (dynamic_entries, _) = dynamic_shdr
        .dynamic(elf.endian(), elf.data())
        .context("failed to parse dynamic section")?
        .unwrap();

    // now we want to encode the GOT relocations.
    // to do that, we first need to verify some of the assumptions that we make about the elf.

    // calculate the amount of GOT entries according to the size of the GOT
    let got_entries_amount = got_shdr.sh_size(elf.endian()) / 4;

    // find the amount of local GOT entries
    let got_local_entries_amount =
        find_dynamic_entry(dynamic_entries, DT_MIPS_LOCAL_GOTNO, elf.endian())
            .context("failed to find number of local GOT entries")?;

    // verify that all GOT entries are local
    if got_entries_amount != got_local_entries_amount {
        bail!("the GOT contains non-local entries");
    }

    // make sure that the expected base address of the elf is 0.
    // this makes sure that GOT relocations don't require subtracting an original assumed base address, and only require adding
    // the new base address.
    let expected_base_address =
        find_dynamic_entry(dynamic_entries, DT_MIPS_BASE_ADDRESS, elf.endian())
            .context("failed to find expected base address")?;
    if expected_base_address != 0 {
        bail!("the expected base address of the elf is not 0");
    }

    // relocate each entry in the GOT
    let got_addr = got_shdr.sh_addr(elf.endian());
    for i in 0..got_entries_amount {
        rel_encoder.encode(got_addr + 4 * i)
    }

    Ok(RelsInfo {
        encoded_rels_amount: rel_encoder.encoded_rels_amount,
        encoded_rels: rel_encoder.serializer.into_buffer(),
    })
}

fn buf_align_len(buf: &mut Vec<u8>, alignment: usize) {
    let aligned_len = ((buf.len() + alignment - 1) / alignment) * alignment;
    if buf.len() != aligned_len {
        buf.resize(aligned_len, 0);
    }
}

fn buf_align_len_to_type<T>(buf: &mut Vec<u8>) {
    buf_align_len(buf, core::mem::align_of::<T>())
}

struct RelEncoder {
    serializer: BinarySerializerToVec,
    encoded_rels_amount: usize,
}
impl RelEncoder {
    pub fn new(elf: &ElfFile32) -> Self {
        Self {
            serializer: BinarySerializerToVec::new(object_endianness_to_binary_serde(elf.endian())),
            encoded_rels_amount: 0,
        }
    }
    pub fn encode(&mut self, offset: u32) {
        self.serializer.serialize(&LoaderEncodedRel { offset });
        self.encoded_rels_amount += 1;
    }
}

/// information extracted from the elfs relocations's
#[derive(Debug)]
struct RelsInfo {
    /// the encoded relocations information.
    encoded_rels: Vec<u8>,

    /// the amount of encoded relocations.
    encoded_rels_amount: usize,
}

/// information extracted from the elf's phdrs
#[derive(Debug)]
struct PhdrsInfo<'a> {
    /// the entire loadable content of the file, extracted from the elf's phdrs.
    content: &'a [u8],

    /// the size of the uninitialized data at the end of the initialized content.
    uninitialized_size: usize,
}
