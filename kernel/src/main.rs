use std::{os::unix::process::CommandExt, path::Path};

use anyhow::{bail, Context, Result};
use binary_serde::{BinarySerde, BinarySerializerToVec};
use clap::Parser;
use devx_cmd::{cmd, run, Cmd};
use elflib::{ArchBitLength, ElfParser, Rel, RelocationType, SectionData, SectionHeaderType};
use loader_shared::LoaderInfoHeader;

#[derive(Parser)]
enum Cli {
    Build,
    Run,
    Gdb,
}
fn main() {
    if let Err(err) = main_fallible() {
        eprintln!("error: {}", err);
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
        "-bios",
        "target/target/debug/kernel",
        "-S",
        "-s",
        "--nographic"
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
    let pre_post_processing_kernel_path = "target/target/debug/pre_post_processing_kernel";
    creare_parent_dirs_and_write_file(pre_post_processing_kernel_path, kernel_content.as_slice())
        .context(format!(
        "failed to write pre post-processing kernel file {pre_post_processing_kernel_path}"
    ))?;

    // post process the content
    let post_processed_content = post_process_kernel_content(kernel_content);

    // write the final content to a file.
    let packed_kernel_path = "target/target/debug/kernel";
    creare_parent_dirs_and_write_file(packed_kernel_path, post_processed_content)
        .context(format!("failed to write kernel file {packed_kernel_path}"))?;

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
    cmd!("cargo", "build").current_dir("core").run()?;
    let kelf_path = "core/target/target/debug/core";
    let kelf_content =
        std::fs::read(kelf_path).context(format!("failed to read kernel elf file {kelf_path}"))?;
    pack_kelf_file(&kelf_content, &loader_code)
        .context(format!("failed to pack kernel elf file {kelf_path}"))
}

fn build_and_extract_loader_code() -> Result<Vec<u8>> {
    // build the loader. this must be done in release mode so that the compiler will optimize everything out and we are only left
    // with a .text section. if we don't do this we get a whole bunch of extra sections due to linking with rust's stdlib.
    cmd!("cargo", "build", "--release")
        .current_dir("loader")
        .run()?;
    let loader_elf_path = "loader/target/target/release/loader";
    let loader_elf_content = std::fs::read(loader_elf_path)
        .context(format!("failed to read loader elf file {loader_elf_path}"))?;
    Ok(get_first_phdr_content(&loader_elf_content)
        .context(format!(
            "failed to extract loader code from loader elf file {loader_elf_path}"
        ))?
        .to_vec())
}

fn get_first_phdr_content(loader_elf_content: &[u8]) -> Result<&[u8]> {
    let elf = ElfParser::new(loader_elf_content)?;
    Ok(elf.program_headers()?.get(0)?.content_in_file()?)
}

fn pack_kelf_file(kelf_content: &[u8], loader_code: &[u8]) -> Result<Vec<u8>> {
    let elf = ElfParser::new(kelf_content)?;
    let phdrs_info = process_elf_phdrs(&elf)?;
    let rels_info = process_elf_relocs(&elf)?;
    let loader_info = LoaderInfoHeader {
        relocations_amount: rels_info.encoded_rels_amount as u32,
        initialized_size: phdrs_info.full_content.len() as u32,
        uninitialized_size: phdrs_info.uninitialized_size as u32,
        entry_point_offset: elf.header()?.entry() as u32,
    };

    let mut serializer = BinarySerializerToVec::new(elf.file_info().endianness);

    // add the code of the loader
    serializer.buffer_mut().extend_from_slice(loader_code);

    // add the loader info header
    buf_align_len_to_type::<LoaderInfoHeader>(serializer.buffer_mut());
    serializer.serialize(&loader_info);

    // add the encoded relocation
    buf_align_len(serializer.buffer_mut(), rels_info.alignment);
    serializer
        .buffer_mut()
        .extend_from_slice(&rels_info.encoded_rels);

    // add the wrapper code
    serializer
        .buffer_mut()
        .extend_from_slice(&phdrs_info.full_content);

    Ok(serializer.into_buffer())
}

fn process_elf_phdrs(elf: &ElfParser) -> Result<PhdrsInfo> {
    let mut full_content = Vec::new();
    let phdrs = elf.program_headers()?;
    let mut uninitialized_size = 0;

    // make sure that the first phdr starts at address 0
    if phdrs.get(0)?.virt_addr() != 0 {
        bail!("the virtual address of the first phdr must be 0");
    }

    for (phdr_idx, phdr_res) in phdrs.iter().enumerate() {
        let phdr = phdr_res?;
        let is_last_phdr = phdr_idx + 1 == phdrs.len();

        // account for padding between the previous section and the current one
        if phdr.virt_addr() > full_content.len() as u64 {
            full_content.resize(phdr.virt_addr() as usize, 0);
        }

        // add the content of the phdr
        full_content.extend_from_slice(phdr.content_in_file()?);

        if phdr.size_in_memory() > phdr.size_in_file() {
            // if the section's size in memory is larger than its size in file, then this section contains uninitialized data.
            // this is only allowed in the last section, otherwise we would have gaps in our packed elf file.
            if !is_last_phdr {
                bail!(
                    "phdr at index {} contains uninitialized data but is not the last phdr",
                    phdr_idx
                );
            }
            uninitialized_size = (phdr.size_in_memory() - phdr.size_in_file()) as usize;
        }
    }
    Ok(PhdrsInfo {
        full_content,
        uninitialized_size,
    })
}

fn process_elf_relocs(elf: &ElfParser) -> Result<RelsInfo> {
    let mut rel_encoder = RelEncoder::new(elf);
    for shdr_res in elf.section_headers()? {
        let shdr = shdr_res?;
        let SectionData::RelocationSection(rel_section) = shdr.data()? else {
            // not a relocation section, skip it.
            continue;
        };
        for rel_entry_res in rel_section.entries {
            let rel_entry = rel_entry_res?;
            let Rel::RelRegular(regular_rel) = &rel_entry.rel else {
                bail!("mips64 is not supported");
            };
            if regular_rel.info().ty != RelocationType::MipsRel32 {
                bail!("unsupported relocation type {:?}", regular_rel.info().ty);
            };

            // encode the relocation.
            rel_encoder.encode(regular_rel.offset(), rel_entry.addend.unwrap_or_default());
        }
    }
    Ok(RelsInfo {
        alignment: rel_encoder.alignment(),
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
    bit_length: ArchBitLength,
    serializer: BinarySerializerToVec,
    encoded_rels_amount: usize,
}
impl RelEncoder {
    pub fn new(elf: &ElfParser) -> Self {
        Self {
            bit_length: elf.file_info().bit_length,
            serializer: BinarySerializerToVec::new(elf.file_info().endianness),
            encoded_rels_amount: 0,
        }
    }
    pub fn encode(&mut self, addr: u64, addend: i64) {
        match self.bit_length {
            elflib::ArchBitLength::Arch32Bit => self.serializer.serialize(&EncodedRel32 {
                addr: addr as u32,
                addend: addend as i32,
            }),
            elflib::ArchBitLength::Arch64Bit => {
                self.serializer.serialize(&EncodedRel64 { addr, addend })
            }
        };
        self.encoded_rels_amount += 1;
    }
    pub fn alignment(&self) -> usize {
        match self.bit_length {
            ArchBitLength::Arch32Bit => 4,
            ArchBitLength::Arch64Bit => 8,
        }
    }
}

#[derive(BinarySerde)]
struct EncodedRel32 {
    addr: u32,
    addend: i32,
}

#[derive(BinarySerde)]
struct EncodedRel64 {
    addr: u64,
    addend: i64,
}

/// information extracted from the elfs relocations's
#[derive(Debug)]
struct RelsInfo {
    /// the encoded relocations information.
    encoded_rels: Vec<u8>,

    /// the amount of encoded relocations.
    encoded_rels_amount: usize,

    /// the required alignment of the encoded relocations information.
    alignment: usize,
}

/// information extracted from the elfs phdr's
#[derive(Debug)]
struct PhdrsInfo {
    /// the entire content of all phdrs of the elf, as laid out in memory.
    full_content: Vec<u8>,

    /// the size of the uninitialized data at the end of the initialized content.
    uninitialized_size: usize,
}
