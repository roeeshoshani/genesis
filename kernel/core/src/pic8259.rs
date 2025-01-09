use bitpiece::*;

#[bitpiece(8)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pic8259InitCmd1 {
    pub icw4_write_required: bool,
    pub is_single: bool,
    pub reserved2: B2,
    pub start_initialization: bool,
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
