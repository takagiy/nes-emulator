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
                // BIT
                0x24 => self.bit(AddressingMode::ZeroPage),
                0x2C => self.bit(AddressingMode::Absolute),
                // BMI
                0x30 => self.bmi(AddressingMode::Relative),
                // BNE
                0xD0 => self.bne(AddressingMode::Relative),
                // BPL
                0x10 => self.bpl(AddressingMode::Relative),
                // BRK
                // 0x00 => self.brk(),
                0x00 => break,
                // BVC
                0x50 => self.bvc(AddressingMode::Relative),
                // BVS
                0x70 => self.bvs(AddressingMode::Relative),
                // CLC
                0x18 => self.clc(),
                // CLD
                0xD8 => self.cld(),
                // CLI
                0x58 => self.cli(),
                // CLV
                0xB8 => self.clv(),
                // CMP
                0xC9 => self.cmp(AddressingMode::Immediate),
                0xC5 => self.cmp(AddressingMode::ZeroPage),
                0xD5 => self.cmp(AddressingMode::ZeroPageX),
                0xCD => self.cmp(AddressingMode::Absolute),
                0xDD => self.cmp(AddressingMode::AbsoluteX),
                0xD9 => self.cmp(AddressingMode::AbsoluteY),
                0xC1 => self.cmp(AddressingMode::IndirectX),
                0xD1 => self.cmp(AddressingMode::IndirectY),
                // CPX
                0xE0 => self.cpx(AddressingMode::Immediate),
                0xE4 => self.cpx(AddressingMode::ZeroPage),
                0xEC => self.cpx(AddressingMode::Absolute),
                // CPY
                0xC0 => self.cpy(AddressingMode::Immediate),
                0xC4 => self.cpy(AddressingMode::ZeroPage),
                0xCC => self.cpy(AddressingMode::Absolute),
                // DEC
                0xC6 => self.dec(AddressingMode::ZeroPage),
                0xD6 => self.dec(AddressingMode::ZeroPageX),
                0xCE => self.dec(AddressingMode::Absolute),
                0xDE => self.dec(AddressingMode::AbsoluteX),
                // DEX
                0xCA => self.dex(),
                // DEY
                0x88 => self.dey(),
                // EOR
                0x49 => self.eor(AddressingMode::Immediate),
                0x45 => self.eor(AddressingMode::ZeroPage),
                0x55 => self.eor(AddressingMode::ZeroPageX),
                0x4D => self.eor(AddressingMode::Absolute),
                0x5D => self.eor(AddressingMode::AbsoluteX),
                0x59 => self.eor(AddressingMode::AbsoluteY),
                0x41 => self.eor(AddressingMode::IndirectX),
                0x51 => self.eor(AddressingMode::IndirectY),
                // INC
                0xE6 => self.inc(AddressingMode::ZeroPage),
                0xF6 => self.inc(AddressingMode::ZeroPageX),
                0xEE => self.inc(AddressingMode::Absolute),
                0xFE => self.inc(AddressingMode::AbsoluteX),
                // INX
                0xE8 => self.inx(),
                // INY
                0xC8 => self.iny(),
                // JMP
                0x4C => self.jmp(AddressingMode::Absolute),
                0x6C => self.jmp(AddressingMode::Indirect),
                // JSR
                0x20 => self.jsr(),
                // LDA
                0xA9 => self.lda(AddressingMode::Immediate),
                0xA5 => self.lda(AddressingMode::ZeroPage),
                0xB5 => self.lda(AddressingMode::ZeroPageX),
                0xAD => self.lda(AddressingMode::Absolute),
                0xBD => self.lda(AddressingMode::AbsoluteX),
                0xB9 => self.lda(AddressingMode::AbsoluteY),
                0xA1 => self.lda(AddressingMode::IndirectX),
                0xB1 => self.lda(AddressingMode::IndirectY),
                // LDX
                0xA2 => self.ldx(AddressingMode::Immediate),
                0xA6 => self.ldx(AddressingMode::ZeroPage),
                0xB6 => self.ldx(AddressingMode::ZeroPageY),
                0xAE => self.ldx(AddressingMode::Absolute),
                0xBE => self.ldx(AddressingMode::AbsoluteY),
                // LDY
                0xA0 => self.ldy(AddressingMode::Immediate),
                0xA4 => self.ldy(AddressingMode::ZeroPage),
                0xB4 => self.ldy(AddressingMode::ZeroPageX),
                0xAC => self.ldy(AddressingMode::Absolute),
                0xBC => self.ldy(AddressingMode::AbsoluteX),
                // LSR
                0x4A => self.lsr(None),
                0x46 => self.lsr(Some(AddressingMode::ZeroPage)),
                0x56 => self.lsr(Some(AddressingMode::ZeroPageX)),
                0x4E => self.lsr(Some(AddressingMode::Absolute)),
                0x5E => self.lsr(Some(AddressingMode::AbsoluteX)),
                // NOP
                0xEA => self.nop(),
                // ORA
                0x09 => self.ora(AddressingMode::Immediate),
                0x05 => self.ora(AddressingMode::ZeroPage),
                0x15 => self.ora(AddressingMode::ZeroPageX),
                0x0D => self.ora(AddressingMode::Absolute),
                0x1D => self.ora(AddressingMode::AbsoluteX),
                0x19 => self.ora(AddressingMode::AbsoluteY),
                0x01 => self.ora(AddressingMode::IndirectX),
                0x11 => self.ora(AddressingMode::IndirectY),
                // PHA
                0x48 => self.pha(),
                // PHP
                0x08 => self.php(),
                // PLA
                0x68 => self.pla(),
                // PLP
                0x28 => self.plp(),
                // ROL
                0x2A => self.rol(None),
                0x26 => self.rol(Some(AddressingMode::ZeroPage)),
                0x36 => self.rol(Some(AddressingMode::ZeroPageX)),
                0x2E => self.rol(Some(AddressingMode::Absolute)),
                0x3E => self.rol(Some(AddressingMode::AbsoluteX)),
                // ROR
                0x6A => self.ror(None),
                0x66 => self.ror(Some(AddressingMode::ZeroPage)),
                0x76 => self.ror(Some(AddressingMode::ZeroPageX)),
                0x6E => self.ror(Some(AddressingMode::Absolute)),
                0x7E => self.ror(Some(AddressingMode::AbsoluteX)),
                // RTI
                0x40 => self.rti(),
                // RTS
                0x60 => self.rts(),
                // SBC
                0xE9 => self.sbc(AddressingMode::Immediate),
                0xE5 => self.sbc(AddressingMode::ZeroPage),
                0xF5 => self.sbc(AddressingMode::ZeroPageX),
                0xED => self.sbc(AddressingMode::Absolute),
                0xFD => self.sbc(AddressingMode::AbsoluteX),
                0xF9 => self.sbc(AddressingMode::AbsoluteY),
                0xE1 => self.sbc(AddressingMode::IndirectX),
                0xF1 => self.sbc(AddressingMode::IndirectY),
                // SEC
                0x38 => self.sec(),
                // SED
                0xF8 => self.sed(),
                // SEI
                0x78 => self.sei(),
                // STA
                0x85 => self.sta(AddressingMode::ZeroPage),
                0x95 => self.sta(AddressingMode::ZeroPageX),
                0x8D => self.sta(AddressingMode::Absolute),
                0x9D => self.sta(AddressingMode::AbsoluteX),
                0x99 => self.sta(AddressingMode::AbsoluteY),
                0x81 => self.sta(AddressingMode::IndirectX),
                0x91 => self.sta(AddressingMode::IndirectY),
                // STX
                0x86 => self.stx(AddressingMode::ZeroPage),
                0x96 => self.stx(AddressingMode::ZeroPageY),
                0x8E => self.stx(AddressingMode::Absolute),
                // STY
                0x84 => self.sty(AddressingMode::ZeroPage),
                0x94 => self.sty(AddressingMode::ZeroPageX),
                0x8C => self.sty(AddressingMode::Absolute),
                // TAX
                0xAA => self.tax(),
                // TAY
                0xA8 => self.tay(),
                // TSX
                0xBA => self.tsx(),
                // TXA
                0x8A => self.txa(),
                // TXS
                0x9A => self.txs(),
                // TYA
                0x98 => self.tya(),
                _ => todo!("opcode {:02X}", opcode),
            }
        }
    }

    /// Add with carry
    fn adc(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.add_with_carry_and_set_flags(operand);
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

        let result = operand << 1;
        let carry = operand & 0b1000_0000 > 0;

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
        self.push_u16(self.register_pc.wrapping_add(1));
        self.push_status(status, true);
        todo!("interrupt")
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

    /// Exclusive OR
    fn eor(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.register_a ^= operand;
        self.set_zero_and_negative_flags();
    }

    /// Increment memory
    fn inc(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        let result = operand.wrapping_add(1);

        self.memory.store(operand_address, result);
        self.set_zero_and_negative_flags_by(result);
    }

    /// Increment X register
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.set_zero_and_negative_flags_by(self.register_x);
    }

    /// Increment Y register
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.set_zero_and_negative_flags_by(self.register_y);
    }

    /// Jump
    fn jmp(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load_u16(operand_address);

        self.register_pc = operand;
    }

    /// Jump to subroutine
    fn jsr(&mut self) {
        let operand = self.take_u16();
        self.push_u16(self.register_pc.wrapping_sub(1));
        self.register_pc = operand;
    }

    /// Load accumulator
    fn lda(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.register_a = operand;
        self.set_zero_and_negative_flags();
    }

    /// Load X register
    fn ldx(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.register_x = operand;
        self.set_zero_and_negative_flags_by(self.register_x);
    }

    /// Load Y register
    fn ldy(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.register_y = operand;
        self.set_zero_and_negative_flags_by(self.register_y);
    }

    /// Logical shift right
    fn lsr(&mut self, addressing: Option<AddressingMode>) {
        let operand_address = addressing.map(|a| self.take_operand_address(a));
        let operand = match operand_address {
            Some(address) => self.memory.load(address),
            None => self.register_a,
        };

        let result = operand >> 1;
        let carry = operand & 0b0000_0001 > 0;

        match operand_address {
            Some(address) => self.memory.store(address, result),
            None => self.register_a = result,
        }
        self.register_p.set(CpuStatus::CARRY, carry);
        self.set_zero_and_negative_flags_by(result);
    }

    /// No operation
    fn nop(&mut self) {}

    /// Logical inclusive OR
    fn ora(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.register_a |= operand;
        self.set_zero_and_negative_flags();
    }

    /// Push accumulator
    fn pha(&mut self) {
        self.push_u8(self.register_a);
    }

    /// Push processor status
    fn php(&mut self) {
        self.push_status(self.register_p, false);
    }

    /// Pull accumulator
    fn pla(&mut self) {
        self.register_a = self.pull_u8();
        self.set_zero_and_negative_flags();
    }

    /// Pull processor status
    fn plp(&mut self) {
        self.register_p = CpuStatus::from_bits_truncate(self.pull_u8());
    }

    /// Rotate left
    fn rol(&mut self, addressing: Option<AddressingMode>) {
        let operand_address = addressing.map(|a| self.take_operand_address(a));
        let operand = match operand_address {
            Some(address) => self.memory.load(address),
            None => self.register_a,
        };

        let c = self.register_p.contains(CpuStatus::CARRY) as u8;
        let result = (operand << 1) | c;
        let carry = operand & 0b1000_0000 > 0;

        match operand_address {
            Some(address) => self.memory.store(address, result),
            None => self.register_a = result,
        }
        self.register_p.set(CpuStatus::CARRY, carry);
        self.set_zero_and_negative_flags_by(result);
    }

    /// Rotate right
    fn ror(&mut self, addressing: Option<AddressingMode>) {
        let operand_address = addressing.map(|a| self.take_operand_address(a));
        let operand = match operand_address {
            Some(address) => self.memory.load(address),
            None => self.register_a,
        };

        let c = (self.register_p.contains(CpuStatus::CARRY) as u8) << 7;
        let result = (operand >> 1) | c;
        let carry = operand & 0b0000_0001 > 0;

        match operand_address {
            Some(address) => self.memory.store(address, result),
            None => self.register_a = result,
        }
        self.register_p.set(CpuStatus::CARRY, carry);
        self.set_zero_and_negative_flags_by(result);
    }

    /// Return from interrupt
    fn rti(&mut self) {
        self.register_p = CpuStatus::from_bits_truncate(self.pull_u8());
        self.register_pc = self.pull_u16();
    }

    /// Return from subroutine
    fn rts(&mut self) {
        self.register_pc = self.pull_u16().wrapping_add(1);
    }

    /// Subtract with carry
    fn sbc(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        let operand = self.memory.load(operand_address);

        self.add_with_carry_and_set_flags(!operand);
    }

    /// Set carry flag
    fn sec(&mut self) {
        self.register_p.set(CpuStatus::CARRY, true);
    }

    /// Set decimal mode
    fn sed(&mut self) {
        self.register_p.set(CpuStatus::DECIMAL, true);
    }

    /// Set interrupt disable
    fn sei(&mut self) {
        self.register_p.set(CpuStatus::INTERRUPT_DISABLE, true);
    }

    /// Store accumulator
    fn sta(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.memory.store(operand_address, self.register_a);
    }

    /// Store X register
    fn stx(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.memory.store(operand_address, self.register_x);
    }

    /// Store Y register
    fn sty(&mut self, addressing: AddressingMode) {
        let operand_address = self.take_operand_address(addressing);
        self.memory.store(operand_address, self.register_y);
    }

    /// Transfer accumulator to X
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.set_zero_and_negative_flags_by(self.register_x);
    }

    /// Transfer accumulator to Y
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.set_zero_and_negative_flags_by(self.register_y);
    }

    /// Transfer stack pointer to X
    fn tsx(&mut self) {
        self.register_x = self.register_s;
        self.set_zero_and_negative_flags_by(self.register_x);
    }

    /// Transfer X to accumulator
    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.set_zero_and_negative_flags();
    }

    /// Transfer X to stack pointer
    fn txs(&mut self) {
        self.register_s = self.register_x;
    }

    /// Transfer Y to accumulator
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.set_zero_and_negative_flags();
    }

    /// Add the given value and the carry flag to the accumulator and set the flags accordingly.
    fn add_with_carry_and_set_flags(&mut self, operand: u8) {
        let (result, carry) = self.register_a.overflowing_add(operand);
        // bit 7 of the two inputs and the result must be the same if there is no overflow
        let overflow = (self.register_a ^ result) & (operand ^ result) & 0b1000_0000 > 0;

        self.register_a = result;
        self.register_p.set(CpuStatus::CARRY, carry);
        self.register_p.set(CpuStatus::OVERFLOW, overflow);
        self.set_zero_and_negative_flags();
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

    /// Pull a value from the stack.
    fn pull_u8(&mut self) -> u8 {
        self.register_s += 1;
        let address = STACK_BOTTOM.wrapping_add(self.register_s as u16);
        self.memory.load(address)
    }

    /// Pull a value from the stack.
    fn pull_u16(&mut self) -> u16 {
        self.register_s += 2;
        let address = STACK_BOTTOM
            .wrapping_add(self.register_s as u16)
            .wrapping_sub(1);
        self.memory.load_u16(address)
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
