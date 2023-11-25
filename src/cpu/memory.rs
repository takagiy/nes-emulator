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
}

impl Default for CpuMemoryMapper {
    fn default() -> Self {
        Self::new()
    }
}
