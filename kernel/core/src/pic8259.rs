use bitpiece::*;

#[bitpiece(8)]
#[derive(Debug, Clone, Copy)]
pub struct Pic8259OpCmd3 {
    pub reg_read_cmd: Pic8259RegReadCmd,
    pub poll_mode_cmd: bool,
    pub must_be_true: bool,
    pub must_be_false: bool,
    pub enable_special_mask_mode: bool,
    pub special_mask_mode: bool,
    pub reserved7: B1,
}

#[bitpiece(2)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Pic8259RegReadCmd {
    NoAction0 = 0b00,
    NoAction1 = 0b01,
    ReadIrrReg = 0b10,
    ReadIsrReg = 0b11,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pic8259InitCmd1 {
    pub icw4_write_required: bool,
    pub reserved3: B3,
    pub must_be_true: bool,
    pub reserved5: B3,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pic8259InitCmd2 {
    pub reserved0: B3,
    pub interrupt_vector_base: B5,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pic8259InitCmd3Master {
    pub reserved0: B2,
    pub enable_cascaded_mode: bool,
    pub reserved3: B5,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pic8259InitCmd3Slave {
    pub slave_identification_code: B3,
    pub reserved3: B5,
}

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pic8259InitCmd4 {
    pub microprocessor_mode: bool,
    pub automatic_end_of_interrupt: bool,
    pub reserved2: B2,
    pub special_fully_nested_mode: bool,
    pub reserved5: B3,
}
