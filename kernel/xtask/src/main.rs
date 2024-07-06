use anyhow::{bail, Context, Result};
use binary_serde::{BinarySerde, BinarySerializerToVec};
use clap::Parser;
use devx_cmd::{cmd, run, Cmd};
use elflib::{ArchBitLength, ElfParser, Rel, RelocationType, SectionData, SectionHeaderType};
use loader_shared::LoaderInfoHeader;

#[derive(Parser)]
enum Cli {
    Build,
}
fn main() {
    if let Err(err) = run() {
        eprintln!("error: {:#?}", err);
    }
}

fn run() -> Result<()> {
    let cli = Cli::parse();
    match cli {
        Cli::Build => build()?,
    }
    Ok(())
}

fn build() -> Result<()> {
    cmd!("cargo", "build").current_dir("core").run()?;
    let kelf_path = "target/target/debug/core";
    let kelf_content =
        std::fs::read(kelf_path).context(format!("failed to read kernel elf file {kelf_path}"))?;
    pack_kelf_file(&kelf_content).context(format!("failed to pack kernel elf file {kelf_path}"))?;
    Ok(())
}

fn pack_kelf_file(kelf_content: &[u8]) -> Result<()> {
    let elf = ElfParser::new(kelf_content)?;
    let phdrs_info = process_elf_phdrs(&elf)?;
    let rels_info = process_elf_relocs(&elf)?;
    let loader_info = LoaderInfoHeader {
        relocations_amount: rels_info.encoded_rels_amount as u32,
        initialized_size: phdrs_info.full_content.len() as u32,
        uninitialized_size: phdrs_info.uninitialized_size as u32,
        entry_point_offset: todo!(),
    };
    println!("{:?}", rels_info);
    Ok(())
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
        full_content.resize(phdr.virt_addr() as usize, 0);

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
        encoded_rels: rel_encoder.serializer.into_buffer(),
        encoded_rels_amount: rel_encoder.encoded_rels_amount,
    })
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
}

/// information extracted from the elfs phdr's
#[derive(Debug)]
struct PhdrsInfo {
    /// the entire content of all phdrs of the elf, as laid out in memory.
    full_content: Vec<u8>,

    /// the size of the uninitialized data at the end of the initialized content.
    uninitialized_size: usize,
}
