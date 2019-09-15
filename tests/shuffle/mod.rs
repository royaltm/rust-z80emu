use z80emu::{Io, Memory};

#[derive(Clone, Debug, Default)]
pub struct TestShuffle {
    pub mem: Vec<u8>
}

impl Io for TestShuffle {
    type Timestamp = i32;
    fn read_io(&mut self, _port: u16, _ts: Self::Timestamp) -> u8 { u8::max_value() }
    fn write_io(&mut self, _port: u16, _data: u8, _ts: Self::Timestamp) -> bool { false }
    fn is_irq(&mut self, _ts: Self::Timestamp) -> bool { false }
}

impl Memory for TestShuffle {
    type Timestamp = i32;
    fn read_mem(&self, addr: u16, _ts: Self::Timestamp) -> u8 {
        self.read_debug(addr)
    }
    fn read_mem16(&self, addr: u16, _ts: Self::Timestamp) -> u16 {
        let mut bytes = [0;2];
        bytes.copy_from_slice(&self.mem[addr as usize..=addr as usize + 1]);
        u16::from_le_bytes(bytes)
    }
    fn read_opcode(&mut self, pc: u16, _ir: u16, _ts: Self::Timestamp) -> u8 {
        self.read_debug(pc)
    }
    /// This is used by the Cpu for writing to the memory.
    fn write_mem(&mut self, addr: u16, value: u8, _ts: Self::Timestamp) {
        self.mem[addr as usize] = value;
    }
    /// Used by the Cpu debugger to get conditional command argument (DJNZ/JR when not jumping). No timestamp.
    fn read_debug(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }
}
