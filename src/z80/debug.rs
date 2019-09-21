use super::{Cpu, Z80};

impl core::fmt::Debug for Z80 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result { // TODO: alternate and hex
        write!(f, "Z80A {{ pc: {:?}, sp: {:?}, \
            af: {:?}, bc: {:?}, de: {:?}, hl: {:?}, \
            af': {:?}, bc': {:?}, de': {:?}, hl': {:?}, \
            ix: {:?}, iy: {:?}, ir: {:?}, r: {:02X}, im: {}, \
            mp: {:?}, iff1: {}, iff2: {}, halt: {}, ei:{}, prefix: {:?} \
            f: {:?} }}",
             self.pc, self.sp, 
             self.af, self.regs.bc, self.regs.de, self.regs.hl,
             self.af_alt, self.regs_alt.bc, self.regs_alt.de, self.regs_alt.hl,
             self.index.ix, self.index.iy, self.ir, self.r.0 & 0x7F, self.im as u8,
             self.memptr, self.iff1 as u8, self.iff2 as u8, self.halt as u8,
             self.last_ei, self.prefix, self.get_flags())
    }
}
