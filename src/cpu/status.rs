use bitflags::bitflags;

bitflags! {
    #[derive(Default, Debug)]
    pub struct CpuStatus: u8 {
        const CARRY             = 0b0000_0001;
        const ZERO              = 0b0000_0010;
        const INTERRUPT_DISABLE = 0b0000_0100;
        const DECIMAL           = 0b0000_1000;
        const BREAK             = 0b0001_0000;
        const OVERFLOW          = 0b0100_0000;
        const NEGATIVE          = 0b1000_0000;
    }
}
