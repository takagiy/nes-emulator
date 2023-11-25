pub mod status;
use status::CpuStatus;

pub mod addressing;
use addressing::AddressingMode;

pub mod memory;
use memory::CpuMemoryMapper;

#[derive(Debug)]
pub struct Cpu {
    /// Accumulator
    pub register_a: u8,
    /// Index register X
    pub register_x: u8,
    /// Index register Y
    pub register_y: u8,
    /// Program counter
    pub register_pc: u16,
    /// Stack pointer
    pub register_s: u8,
    /// Status register holding some bit flags
    pub register_p: CpuStatus,
    /// Memory
    pub memory: CpuMemoryMapper,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            register_pc: 0,
            register_s: 0,
            register_p: Default::default(),
            memory: Default::default(),
        }
    }

    pub fn run(&mut self) {
        loop {
            let opcode = self.take_u8();

            todo!();
        }
    }

    /// Get the byte under the program counter as an `u8` and increment the program counter.
    fn take_u8(&mut self) -> u8 {
        let byte = self.memory.load(self.register_pc);
        self.register_pc += 1;
        byte
    }

    /// Get the byte under the program counter as an `i8` and increment the program counter.
    fn take_i8(&mut self) -> i8 {
        i8::from_le_bytes([self.take_u8()])
    }

    /// Get the two bytes under the program counter as an `u16` and increment the program counter.
    fn take_u16(&mut self) -> u16 {
        let low = self.take_u8();
        let high = self.take_u8();
        u16::from_le_bytes([low, high])
    }

    fn take_operand_address(&mut self, addressing: AddressingMode) -> u16 {
        use AddressingMode::*;

        match addressing {
            Immediate => {
                let pc = self.register_pc;
                self.register_pc += 1;
                pc
            }
            ZeroPage => self.take_u8() as u16,
            ZeroPageX => self.take_u8().wrapping_add(self.register_x) as u16,
            ZeroPageY => self.take_u8().wrapping_add(self.register_y) as u16,
            Relative => {
                let offset = self.take_i8() as i16;
                self.register_pc.wrapping_add_signed(offset)
            }
            Absolute => self.take_u16(),
            AbsoluteX => self.take_u16().wrapping_add(self.register_x as u16),
            AbsoluteY => self.take_u16().wrapping_add(self.register_y as u16),
            Indirect => {
                let address_address = self.take_u16();
                self.memory.load_u16(address_address)
            }
            IndirectX => {
                let address_address = self.take_u8().wrapping_add(self.register_x) as u16;
                self.memory.load_u16(address_address)
            }
            IndirectY => {
                let address_address = self.take_u8() as u16;
                self.memory
                    .load_u16(address_address)
                    .wrapping_add(self.register_y as u16)
            }
        }
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}
