pub mod status;
use status::CpuStatus;

pub mod addressing;
use addressing::AddressingMode;

pub mod memory;
use memory::{CpuMemoryMapper, STACK_BOTTOM};

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

            match opcode {
                // ADC
                0x69 => self.adc(AddressingMode::Immediate),
                0x65 => self.adc(AddressingMode::ZeroPage),
                0x75 => self.adc(AddressingMode::ZeroPageX),
                0x6D => self.adc(AddressingMode::Absolute),
                0x7D => self.adc(AddressingMode::AbsoluteX),
                0x79 => self.adc(AddressingMode::AbsoluteY),
                0x61 => self.adc(AddressingMode::IndirectX),
                0x71 => self.adc(AddressingMode::IndirectY),
                // AND
                0x29 => self.and(AddressingMode::Immediate),
                0x25 => self.and(AddressingMode::ZeroPage),
                0x35 => self.and(AddressingMode::ZeroPageX),
                0x2D => self.and(AddressingMode::Absolute),
                0x3D => self.and(AddressingMode::AbsoluteX),
                0x39 => self.and(AddressingMode::AbsoluteY),
                0x21 => self.and(AddressingMode::IndirectX),
                0x32 => self.and(AddressingMode::IndirectY),
                // ASL
                0x0A => self.asl(None),
                0x06 => self.asl(Some(AddressingMode::ZeroPage)),
                0x16 => self.asl(Some(AddressingMode::ZeroPageX)),
                0x0E => self.asl(Some(AddressingMode::Absolute)),
                0x1E => self.asl(Some(AddressingMode::AbsoluteX)),
                // BCC
                0x90 => self.bcc(AddressingMode::Relative),
                // BCS
                0xB0 => self.bcs(AddressingMode::Relative),
                // BEQ
                0xF0 => self.beq(AddressingMode::Relative),

                _ => todo!("opcode {:02X}", opcode),
            }
        }
    }

    /// Add with carry
    fn adc(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        let (result, carry) = self.register_a.overflowing_add(operand);
        // bit 7 of the two inputs and the result must be the same if there is no overflow
        let overflow = (self.register_a ^ result) & (operand ^ result) & 0b1000_0000 > 0;

        self.register_a = result;
        self.register_p.set(CpuStatus::CARRY, carry);
        self.register_p.set(CpuStatus::OVERFLOW, overflow);
        self.set_zero_and_negative_flags();
    }

    /// Logical AND
    fn and(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.register_a &= operand;
        self.set_zero_and_negative_flags();
    }

    /// Arithmetic shift left
    fn asl(&mut self, addressing: Option<AddressingMode>) {
        let operand_address = addressing.map(|a| self.take_operand_address(a));
        let operand = match operand_address {
            Some(address) => self.memory.load(address),
            None => self.register_a,
        };

        let (result, carry) = operand.overflowing_shl(1);

        match operand_address {
            Some(address) => self.memory.store(address, result),
            None => self.register_a = result,
        }
        self.register_p.set(CpuStatus::CARRY, carry);
        self.set_zero_and_negative_flags_by(result);
    }

    /// Branch if carry clear
    fn bcc(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(!self.register_p.contains(CpuStatus::CARRY), operand_address);
    }

    // Branch if carry set
    fn bcs(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(self.register_p.contains(CpuStatus::CARRY), operand_address);
    }

    /// Branch if equal
    fn beq(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(self.register_p.contains(CpuStatus::ZERO), operand_address);
    }

    /// Bit test
    fn bit(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        let result = self.register_a & operand;

        self.register_p.set(CpuStatus::ZERO, result == 0);
        self.register_p
            .set(CpuStatus::OVERFLOW, operand & 0b0100_0000 > 0);
        self.register_p
            .set(CpuStatus::NEGATIVE, operand & 0b1000_0000 > 0);
    }

    /// Branch if minus
    fn bmi(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(
            self.register_p.contains(CpuStatus::NEGATIVE),
            operand_address,
        );
    }

    /// Branch if not equal
    fn bne(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(!self.register_p.contains(CpuStatus::ZERO), operand_address);
    }

    /// Branch if positive
    fn bpl(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(
            !self.register_p.contains(CpuStatus::NEGATIVE),
            operand_address,
        );
    }

    /// Break
    fn brk(&mut self) {
        let status = self.register_p;
        self.push_u16(self.register_pc.wrapping_add(2));
        self.push_status(status, true);
    }

    /// Branch if overflow clear
    fn bvc(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(
            !self.register_p.contains(CpuStatus::OVERFLOW),
            operand_address,
        );
    }

    /// Branch if overflow set
    fn bvs(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.branch_if(
            self.register_p.contains(CpuStatus::OVERFLOW),
            operand_address,
        );
    }

    /// Clear carry flag
    fn clc(&mut self) {
        self.register_p.set(CpuStatus::CARRY, false);
    }

    /// Clear decimal mode
    fn cld(&mut self) {
        self.register_p.set(CpuStatus::DECIMAL, false);
    }

    /// Clear interrupt disable
    fn cli(&mut self) {
        self.register_p.set(CpuStatus::INTERRUPT_DISABLE, false);
    }

    /// Clear overflow flag
    fn clv(&mut self) {
        self.register_p.set(CpuStatus::OVERFLOW, false);
    }

    /// Compare
    fn cmp(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.compare(self.register_a, operand);
    }

    /// Compare X register
    fn cpx(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.compare(self.register_x, operand);
    }

    /// Compare Y register
    fn cpy(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.compare(self.register_y, operand);
    }

    /// Decrement memory
    fn dec(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        let result = operand.wrapping_sub(1);

        self.memory.store(operand_address, result);
        self.set_zero_and_negative_flags_by(result);
    }

    /// Decrement X register
    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.set_zero_and_negative_flags_by(self.register_x);
    }

    /// Decrement Y register
    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.set_zero_and_negative_flags_by(self.register_y);
    }

    /// Compare two values and set the flags accordingly.
    ///
    /// - CARRY: Set if `lhs >= rhs`.
    /// - ZERO: Set if `lhs == rhs`.
    /// - NEGATIVE: Set if 7th bit of `lhs - rhs` is set.
    fn compare(&mut self, lhs: u8, rhs: u8) {
        let (result, overflow) = lhs.overflowing_sub(rhs);

        self.register_p.set(CpuStatus::CARRY, !overflow);
        self.set_zero_and_negative_flags_by(result);
    }

    /// Push the given status onto the stack.
    /// Set the break flag of the status on the stack if `set_break_flag` is `true`.
    fn push_status(&mut self, mut status: CpuStatus, set_break_flag: bool) {
        status.set(CpuStatus::BREAK, set_break_flag);
        status.set(CpuStatus::UNUSED, true);

        self.push_u8(status.bits());
    }

    /// Push the given value onto the stack.
    fn push_u8(&mut self, value: u8) {
        let address = STACK_BOTTOM.wrapping_add(self.register_s as u16);
        self.memory.store(address, value);
        self.register_s -= 1;
    }

    /// Push the given value onto the stack.
    fn push_u16(&mut self, value: u16) {
        let address = STACK_BOTTOM
            .wrapping_add(self.register_s as u16)
            .wrapping_sub(1);
        self.memory.store_u16(address, value);
        self.register_s -= 2;
    }

    /// Add the value stored at the given address to the program counter if the condition is met.
    fn branch_if(&mut self, condition: bool, operand_address: u16) {
        if condition {
            let operand = self.memory.load_i8(operand_address);
            self.register_pc = self.register_pc.wrapping_add_signed(operand as i16);
        }
    }

    /// Set the zero and negative flags based on the value of the accumulator.
    ///
    /// - ZERO: Set if the accumulator is zero.
    /// - NEGATIVE: Set if bit 7 of the accumulator is set.
    ///
    /// see [`set_zero_and_negative_flags_by`]
    fn set_zero_and_negative_flags(&mut self) {
        self.set_zero_and_negative_flags_by(self.register_a);
    }

    /// Set the zero and negative flags based on the given value.
    ///
    /// - ZERO: Set if the value is zero.
    /// - NEGATIVE: Set if bit 7 of the value is set.
    ///
    /// see [`set_zero_and_negative_flags`]
    fn set_zero_and_negative_flags_by(&mut self, value: u8) {
        self.register_p.set(CpuStatus::ZERO, value == 0);
        self.register_p
            .set(CpuStatus::NEGATIVE, value & 0b1000_0000 > 0);
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

    /// Get the address of the operand based on the addressing mode and increment the program counter.
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
