use crate::mem::VirtAddr;
use bitpiece::*;

/// the size, in bytes, of a mips instruction.
pub const MIPS_INSN_SIZE: usize = size_of::<u32>();

pub type MipsInsnEncodedReg = B5;

fn encode_reg(index: u8) -> MipsInsnEncodedReg {
    B5(index)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum MipsInsnReg {
    Zero = 0,
    At = 1,
    V0 = 2,
    V1 = 3,
    A0 = 4,
    A1 = 5,
    A2 = 6,
    A3 = 7,
    T0 = 8,
    T1 = 9,
    T2 = 10,
    T3 = 11,
    T4 = 12,
    T5 = 13,
    T6 = 14,
    T7 = 15,
    S0 = 16,
    S1 = 17,
    S2 = 18,
    S3 = 19,
    S4 = 20,
    S5 = 21,
    S6 = 22,
    S7 = 23,
    T8 = 24,
    T9 = 25,
    K0 = 26,
    K1 = 27,
    Gp = 28,
    Sp = 29,
    S8 = 30,
    Ra = 31,
}

impl MipsInsnReg {
    pub const REGS_AMOUNT: usize = 32;

    pub const CALLEE_SAVED_REGS: [Self; 8] = [
        Self::S0,
        Self::S1,
        Self::S2,
        Self::S3,
        Self::S4,
        Self::S5,
        Self::S6,
        Self::S7,
    ];

    pub fn encode(self) -> MipsInsnEncodedReg {
        encode_reg(self as u8)
    }
}

/// a mips instruction which moves one register into another
#[repr(C)]
pub struct MipsMoveInsn {
    pub ori: MipsInsnIType,
}
impl MipsMoveInsn {
    pub fn new(dst_reg: MipsInsnReg, src_reg: MipsInsnReg) -> Self {
        Self {
            ori: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: 0,
                rt: dst_reg.encode(),
                rs: src_reg.encode(),
                opcode: MipsInsnOpcode::Ori,
            }),
        }
    }
}

/// a mips instruction sequence which pushes a register onto the stack
#[repr(C)]
pub struct MipsPushReg {
    pub addiu: MipsInsnIType,
    pub sw: MipsInsnIType,
}
impl MipsPushReg {
    pub fn new(reg: MipsInsnReg) -> Self {
        Self {
            addiu: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: -4i16 as u16,
                rt: MipsInsnReg::Sp.encode(),
                rs: MipsInsnReg::Sp.encode(),
                opcode: MipsInsnOpcode::Addiu,
            }),
            sw: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: 0,
                rt: reg.encode(),
                rs: MipsInsnReg::Sp.encode(),
                opcode: MipsInsnOpcode::Sw,
            }),
        }
    }
}

/// a mips instruction sequence which pops a value from the stack into a register
#[repr(C)]
pub struct MipsPopReg {
    pub lw: MipsInsnIType,
    pub addiu: MipsInsnIType,
}
impl MipsPopReg {
    pub fn new(reg: MipsInsnReg) -> Self {
        Self {
            lw: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: 0,
                rt: reg.encode(),
                rs: MipsInsnReg::Sp.encode(),
                opcode: MipsInsnOpcode::Lw,
            }),
            addiu: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: 4,
                rt: MipsInsnReg::Sp.encode(),
                rs: MipsInsnReg::Sp.encode(),
                opcode: MipsInsnOpcode::Addiu,
            }),
        }
    }
}

/// a mips instruction sequence which loads a 32-bit immediate into a register
#[repr(C)]
pub struct MipsLoadImm32ToReg {
    pub lui: MipsInsnIType,
    pub ori: MipsInsnIType,
}
impl MipsLoadImm32ToReg {
    pub fn new(reg: MipsInsnReg, imm32: u32) -> Self {
        Self {
            lui: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: (imm32 >> 16) as u16,
                rt: reg.encode(),
                rs: encode_reg(0),
                opcode: MipsInsnOpcode::Lui,
            }),
            ori: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: (imm32 & 0xffff) as u16,
                rt: reg.encode(),
                rs: reg.encode(),
                opcode: MipsInsnOpcode::Ori,
            }),
        }
    }
}

/// a mips instruction sequence which jumps to the given virtual address by performing a relative jump to it.
#[repr(C)]
pub struct MipsRelJump {
    pub beq: MipsInsnIType,
    pub nop: MipsInsnRType,
}
impl MipsRelJump {
    pub fn new(target_addr: VirtAddr, rel_jump_code_addr: VirtAddr) -> Self {
        // make sure that the addresses are properly aligned
        assert!(target_addr.0 % MIPS_INSN_SIZE == 0);
        assert!(rel_jump_code_addr.0 % MIPS_INSN_SIZE == 0);

        let delay_slot_insn_addr = rel_jump_code_addr + MIPS_INSN_SIZE;
        let diff32 = (target_addr.0 as u32).wrapping_sub(delay_slot_insn_addr.0 as u32) as i32;

        let branch_imm32 = diff32 >> 2;
        let branch_imm16 = branch_imm32 as i16;

        // make sure that the branch immediate fits in 16 bits.
        assert_eq!(branch_imm16 as i32, branch_imm32);

        Self {
            beq: MipsInsnIType::from_fields(MipsInsnITypeFields {
                imm: branch_imm16 as u16,
                rt: encode_reg(0),
                rs: encode_reg(0),
                opcode: MipsInsnOpcode::Beq,
            }),
            nop: MipsInsnRType::ones(),
        }
    }
}

/// a mips instruction sequence which jumps to the given virtual address by loading it into a register and jumping to it.
#[repr(C)]
pub struct MipsAbsJump {
    pub load_imm_to_reg: MipsLoadImm32ToReg,
    pub jr: MipsInsnRType,
}
impl MipsAbsJump {
    pub fn new(target_addr: VirtAddr, reg: MipsInsnReg) -> Self {
        // make sure that the target address is properly aligned
        assert!(target_addr.0 % MIPS_INSN_SIZE == 0);

        Self {
            load_imm_to_reg: MipsLoadImm32ToReg::new(reg, target_addr.0 as u32),
            jr: MipsInsnRType::from_fields(MipsInsnRTypeFields {
                function: MipsInsnFunction::Jr,
                shift_amount: BitPiece::zeroes(),
                rd: encode_reg(0),
                rt: encode_reg(0),
                rs: reg.encode(),
                opcode: MipsInsnOpcode::Special,
            }),
        }
    }
}

/// a mips instruction sequence which calls a function at the given virtual address by loading it into a register and calling to it.
#[repr(C)]
pub struct MipsAbsCall {
    pub load_imm_to_reg: MipsLoadImm32ToReg,
    pub jalr: MipsInsnRType,
}
impl MipsAbsCall {
    pub fn new(reg: MipsInsnReg, target_addr: VirtAddr) -> Self {
        // make sure that the target address is properly aligned
        assert!(target_addr.0 % MIPS_INSN_SIZE == 0);

        Self {
            load_imm_to_reg: MipsLoadImm32ToReg::new(reg, target_addr.0 as u32),
            jalr: MipsInsnRType::from_fields(MipsInsnRTypeFields {
                function: MipsInsnFunction::Jalr,
                shift_amount: BitPiece::zeroes(),
                rd: MipsInsnReg::Ra.encode(),
                rt: encode_reg(0),
                rs: reg.encode(),
                opcode: MipsInsnOpcode::Special,
            }),
        }
    }
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MipsInsnIType {
    pub imm: u16,
    pub rt: MipsInsnEncodedReg,
    pub rs: MipsInsnEncodedReg,
    pub opcode: MipsInsnOpcode,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MipsInsnJType {
    pub index: B26,
    pub opcode: MipsInsnOpcode,
}

#[bitpiece(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MipsInsnRType {
    pub function: MipsInsnFunction,
    pub shift_amount: B5,
    pub rd: MipsInsnEncodedReg,
    pub rt: MipsInsnEncodedReg,
    pub rs: MipsInsnEncodedReg,
    pub opcode: MipsInsnOpcode,
}

#[bitpiece(6)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum MipsInsnFunction {
    Sll = 0,
    Reserved1 = 1,
    Srl = 2,
    Sra = 3,
    Sllv = 4,
    Reserved5 = 5,
    Srlv = 6,
    Srav = 7,
    Jr = 8,
    Jalr = 9,
    Reserved10 = 10,
    Reserved11 = 11,
    Syscall = 12,
    Break = 13,
    Reserved14 = 14,
    Reserved15 = 15,
    Mfhi = 16,
    Mthi = 17,
    Mflo = 18,
    Mtlo = 19,
    Reserved20 = 20,
    Reserved21 = 21,
    Reserved22 = 22,
    Reserved23 = 23,
    Mult = 24,
    Multu = 25,
    Div = 26,
    Divu = 27,
    Reserved28 = 28,
    Reserved29 = 29,
    Reserved30 = 30,
    Reserved31 = 31,
    Add = 32,
    Addu = 33,
    Sub = 34,
    Subu = 35,
    And = 36,
    Or = 37,
    Xor = 38,
    Nor = 39,
    Reserved40 = 40,
    Reserved41 = 41,
    Slt = 42,
    Sltu = 43,
    Reserved44 = 44,
    Reserved45 = 45,
    Reserved46 = 46,
    Reserved47 = 47,
    Tge = 48,
    Tgeu = 49,
    Tlt = 50,
    Tltu = 51,
    Teq = 52,
    Reserved53 = 53,
    Tne = 54,
    Reserved55 = 55,
    Reserved56 = 56,
    Reserved57 = 57,
    Reserved58 = 58,
    Reserved59 = 59,
    Reserved60 = 60,
    Reserved61 = 61,
    Reserved62 = 62,
    Reserved63 = 63,
}

#[bitpiece(6)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum MipsInsnOpcode {
    Special = 0,
    RegImm = 1,
    J = 2,
    Jal = 3,
    Beq = 4,
    Bne = 5,
    Blez = 6,
    Bgtz = 7,
    Addi = 8,
    Addiu = 9,
    Slti = 10,
    Sltiu = 11,
    Andi = 12,
    Ori = 13,
    Xori = 14,
    Lui = 15,
    Cop0 = 16,
    Cop1 = 17,
    Cop2 = 18,
    Reserved19 = 19,
    Reserved20 = 20,
    Reserved21 = 21,
    Reserved22 = 22,
    Reserved23 = 23,
    Reserved24 = 24,
    Reserved25 = 25,
    Reserved26 = 26,
    Reserved27 = 27,
    Reserved28 = 28,
    Reserved29 = 29,
    Reserved30 = 30,
    Reserved31 = 31,
    Lb = 32,
    Lh = 33,
    Lwl = 34,
    Lw = 35,
    Lbu = 36,
    Lhu = 37,
    Lwr = 38,
    Reserved39 = 39,
    Sb = 40,
    Sh = 41,
    Swl = 42,
    Sw = 43,
    Reserved36 = 44,
    Reserved37 = 45,
    Swr = 46,
    Reserved47 = 47,
    Reserved48 = 48,
    Lwc1 = 49,
    Lwc2 = 50,
    Reserved51 = 51,
    Reserved52 = 52,
    Reserved53 = 53,
    Reserved54 = 54,
    Reserved55 = 55,
    Reserved56 = 56,
    Swc1 = 57,
    Swc2 = 58,
    Reserved59 = 59,
    Reserved60 = 60,
    Reserved61 = 61,
    Reserved62 = 62,
    Reserved63 = 63,
}
