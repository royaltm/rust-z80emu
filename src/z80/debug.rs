use core::num::Wrapping;
use crate::cpu::{Cpu, Prefix, CpuDebug, CpuDebugCode, CpuDebugArgs};
use super::flavours::Flavour;
use super::Z80;

impl<Q: Flavour> core::fmt::Debug for Z80<Q> {
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

impl CpuDebug {
    pub(super) fn debug_instruction<F>(mnemonic: &'static str, args: CpuDebugArgs, prefix: Option<Prefix>,
                                                               bytes: &[u8], pc: Wrapping<u16>, debugger: F)
    where F: FnOnce(CpuDebug)
    {
        let mut code = CpuDebugCode::new();
        if let Some(pfx) = prefix {
            code.push(pfx as u8);
        }
        code.extend(bytes.iter().cloned());
        let pc = (pc - Wrapping(code.len() as u16)).0;
        debugger(CpuDebug {
            code,
            mnemonic,
            pc,
            prefix,
            args
        });
    }
}
