use core::num::Wrapping;
use crate::cpu::{Prefix, CpuDebug, CpuDebugCode, CpuDebugArgs};
use super::flavours::Flavour;
use super::Z80;

impl<Q: Flavour> core::fmt::Debug for Z80<Q> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Z80")
            .field("pc", &self.pc.get16())
            .field("sp", &self.sp.get16())
            .field("af", &self.af.get16())
            .field("bc", &self.regs.bc.get16())
            .field("de", &self.regs.de.get16())
            .field("hl", &self.regs.hl.get16())
            .field("af'", &self.af_alt.get16())
            .field("bc'", &self.regs_alt.bc.get16())
            .field("de'", &self.regs_alt.de.get16())
            .field("hl'", &self.regs_alt.hl.get16())
            .field("ix", &self.index.ix.get16())
            .field("iy", &self.index.iy.get16())
            .field("ir", &self.ir.get16())
            .field("r", &self.r.0)
            .field("im", &self.im)
            .field("iff1", &self.iff1)
            .field("iff2", &self.iff2)
            .field("halt", &self.halt)
            .field("ei", &self.last_ei)
            .field("mp", &self.memptr.get16())
            .field("prefix", &self.prefix)
            .finish()
    }
}

impl CpuDebug {
    pub(super) fn debug_instruction<F>(
            mnemonic: &'static str,
            args: CpuDebugArgs,
            prefix: Option<Prefix>,
            bytes: &[u8],
            pc: Wrapping<u16>,
            debugger: F
        )
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
