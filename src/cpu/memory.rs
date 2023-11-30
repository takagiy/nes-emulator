/// Resolver which maps the address space of the 2 byte address which is available to the CPU to
/// the actual devices.
#[derive(Debug)]
pub struct CpuMemoryMapper {
    pub memory: [u8; 2048],
}

impl CpuMemoryMapper {
    pub fn new() -> CpuMemoryMapper {
        CpuMemoryMapper { memory: [0; 2048] }
    }

    pub fn load(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    pub fn load_u16(&self, address: u16) -> u16 {
        let low = self.load(address);
        let high = self.load(address + 1);
        u16::from_le_bytes([low, high])
    }

    pub fn load_i8(&self, address: u16) -> i8 {
        i8::from_le_bytes([self.load(address)])
    }

    pub fn store(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value;
    }

    pub fn store_u16(&mut self, address: u16, value: u16) {
        let [low, high] = value.to_le_bytes();
        self.store(address, low);
        self.store(address + 1, high);
    }
}

impl Default for CpuMemoryMapper {
    fn default() -> Self {
        Self::new()
    }
}

pub const STACK_BOTTOM: u16 = 0x0100;
