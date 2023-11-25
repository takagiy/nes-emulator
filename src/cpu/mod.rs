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
            let opcode = self.take_byte();

            todo!();
        }
    }

    /// Get the byte under the program counter and increment the program counter.
    fn take_byte(&mut self) -> u8 {
        let byte = self.memory.load(self.register_pc);
        self.register_pc += 1;
        byte
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}
