//! Private methods with implementation of some of the more complicated instructions of Z80.
use super::*;
use crate::{NMI_RESTART, Rot};

/// Converts Rot enum into one of the appriopriate ops function.
impl From<Rot> for fn(u8, &mut CpuFlags) -> u8 {
    #[inline]
    fn from(rot: Rot) -> Self {
        match rot {
            Rot::RLC  => ops::rlc,
            Rot::RRC  => ops::rrc,
            Rot::RL   => ops::rl,
            Rot::RR   => ops::rr,
            Rot::SLA  => ops::sla,
            Rot::SRA  => ops::sra,
            Rot::SLL  => ops::sll,
            Rot::SRL  => ops::srl
        }
    }
}

impl Z80 {
    #[inline]
    pub(super) fn get_ir(&mut self) -> u16 {
        let ir = self.ir.get16();
        ir & 0xFF80 | self.r.0 as u16 & 0x007F
    }

    #[inline]
    pub(super) fn ex_de_hl(&mut self) {
        swap(&mut self.regs.de, &mut self.regs.hl);
    }

    #[inline]
    pub(super) fn ex_af_af_with_flags(&mut self, flags: CpuFlags) -> CpuFlags {
        let a = self.af.get8hi();
        self.af = self.af_alt;
        self.af_alt.set(a, flags.bits());
        self.get_flags()
    }

    #[inline]
    pub(super) fn load_reg(&mut self, dst: Reg8, src: Reg8, prefix: Prefix) {
        self.set_reg(dst, prefix, self.get_reg(src, prefix))
    }

    /// Force IRQ
    pub(super) fn irq_no_check<M, T, F>(&mut self, control: &mut M, mut tsc: T, debug: Option<F>) -> Result<T, T>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock,
          F: FnOnce(CpuDebug)
    {
        self.disable_interrupts();
        self.inc_r();
        let mut pc = self.pc.get16();
        if self.halt {
            self.halt = false;
            pc = pc.wrapping_add(1);
            self.pc.set16(pc);
        }
        let bus_ts = tsc.add_irq(pc);
        let vector = control.irq_data(pc, bus_ts);
        match self.im {
            InterruptMode::Mode0 => {
                let res = self.execute_instruction::<M,T,F>(control, tsc, debug, vector);
                if self.halt {
                    // HALT decreases PC we don't want that here
                    self.pc.set16(self.pc.get16().wrapping_add(1));
                }
                res
            }
            InterruptMode::Mode1 => {
                self.execute_instruction::<M,T,F>(control, tsc, debug, opconsts::RST_38H_OPCODE)
            }
            InterruptMode::Mode2 => {
                let ir = self.get_ir();
                tsc.add_no_mreq(ir, 1); // ir:1
                self.push16(pc, control, &mut tsc); // sp-1:3, sp-2:3
                let vaddr = ir & 0xFF00 | vector as u16;
                self.pc.set16(vaddr);
                self.execute_instruction::<M,T,F>(control, tsc, debug, opconsts::JP_OPCODE) // pc+1:3,pc+2:3
            }
        }
    }

    /// Force NMI
    pub(super) fn nmi_no_check<M, T>(&mut self, control: &mut M, mut tsc: T) -> T
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    {
        self.last_ei = false;
        self.iff1 = false;
        self.inc_r();
        let mut pc = self.pc.get16();
        if self.halt {
            self.halt = false;
            pc = pc.wrapping_add(1);
        }
        tsc.add_m1(pc); // pc:4
        tsc.add_no_mreq(self.get_ir(), 1); // ir:1
        self.push16(pc, control, &mut tsc); // sp-1:3, sp-2:3
        self.pc.set16(NMI_RESTART);
        tsc
    }

    #[inline(always)]
    pub(super) fn pop16<M, T>(&mut self, control: &mut M, tsc: &mut T) -> u16
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // sp:3,sp+1:3
        let sp = self.sp.get16();
        let val = control.read_mem16(sp, tsc.add_mreq(sp));
        tsc.add_mreq(sp.wrapping_add(1));
        self.sp.set16(sp.wrapping_add(2));
        val
    }

    #[inline(always)]
    pub(super) fn push16<M, T>(&mut self, val: u16, control: &mut M, tsc: &mut T)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // sp-1:3,sp-2:3
        let [vlo, vhi] = val.to_le_bytes();
        self.push2(vhi, vlo, control, tsc);
    }

    #[inline(always)]
    pub(super) fn push2<M, T>(&mut self, vhi: u8, vlo: u8, control: &mut M, tsc: &mut T)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // sp-1:3,sp-2:3
        let sp = self.sp.get16().wrapping_sub(1);
        control.write_mem(sp, vhi, tsc.add_mreq(sp));
        let sp = sp.wrapping_sub(1);
        control.write_mem(sp, vlo, tsc.add_mreq(sp));
        self.sp.set16(sp);
    }

    #[inline]
    pub(super) unsafe fn reg8_ptr(&mut self, tgt: Reg8, prefix: Prefix) -> *mut u8 {
        match tgt {
            Reg8::B => self.regs.bc.ptr8hi(),
            Reg8::C => self.regs.bc.ptr8lo(),
            Reg8::D => self.regs.de.ptr8hi(),
            Reg8::E => self.regs.de.ptr8lo(),
            Reg8::H => match prefix {
                Prefix::None => self.regs.hl.ptr8hi(),
                Prefix::Xdd => self.index.ix.ptr8hi(),
                Prefix::Yfd => self.index.iy.ptr8hi(),
            }
            Reg8::L => match prefix {
                Prefix::None => self.regs.hl.ptr8lo(),
                Prefix::Xdd => self.index.ix.ptr8lo(),
                Prefix::Yfd => self.index.iy.ptr8lo(),
            }
            Reg8::A => self.af.ptr8hi()
        }
    }

    #[inline]
    pub(super) fn apply_reg8<F>(&mut self, tgt: Reg8, prefix: Prefix, op: F)
    where F: FnOnce(u8) -> u8
    {
        unsafe {
            let ptr = self.reg8_ptr(tgt, prefix);
            *ptr = op(*ptr);
        }
    }

    #[inline]
    pub(super) fn reg16_ref(&self, src: Reg16) -> &RegisterPair {
        match src {
            Reg16::BC => &self.regs.bc,
            Reg16::DE => &self.regs.de,
            Reg16::HL => &self.regs.hl,
            Reg16::SP => &self.sp
        }        
    }

    #[inline]
    pub(super) fn get_reg_prefix16(&self, src: Reg16, prefix: Prefix) -> u16 {
        match src {
            Reg16::BC => self.regs.bc.get16(),
            Reg16::DE => self.regs.de.get16(),
            Reg16::HL => self.get_index16(prefix),
            Reg16::SP => self.sp.get16()
        }        
    }

    #[inline]
    pub(super) fn index16_ref(&self, prefix: Prefix) -> &RegisterPair {
        match prefix {
            Prefix::Xdd => &self.index.ix,
            Prefix::Yfd => &self.index.iy,
            Prefix::None => &self.regs.hl,
        }        
    }

    #[inline]
    pub(super) fn reg16_mut(&mut self, src: Reg16) -> &mut RegisterPair {
        match src {
            Reg16::BC => &mut self.regs.bc,
            Reg16::DE => &mut self.regs.de,
            Reg16::HL => &mut self.regs.hl,
            Reg16::SP => &mut self.sp
        }        
    }

    #[inline]
    pub(super) fn index16_mut(&mut self, prefix: Prefix) -> &mut RegisterPair {
        match prefix {
            Prefix::Xdd => &mut self.index.ix,
            Prefix::Yfd => &mut self.index.iy,
            Prefix::None => &mut self.regs.hl,
        }        
    }

    pub(super) fn block_transfer<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // hl:3, de:3, de:1 x 2 [de:1 x 5]
        let hl = self.regs.hl.get16();
        let de = self.regs.de.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl));
        control.write_mem(de, val, tsc.add_mreq(de));
        tsc.add_no_mreq(de, 2);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        self.regs.de.set16(de.wrapping_add(delta as u16));
        let is_over = self.regs.bc.dec16_is_zero();
        ops::ldx(self.af.get8hi(), val, is_over, flags);
        if let Some(pc) = pc {
            if !is_over {
                tsc.add_no_mreq(de, 5);
                // MEMPTR = PC + 1
                self.memptr.set16((pc - Wrapping(1)).0);
                return Some(pc - Wrapping(2))
            }
        }
        None
    }

    pub(super) fn block_search<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // hl:3, hl:1 x 5, [hl:1 x 5]
        let hl = self.regs.hl.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl));
        tsc.add_no_mreq(hl, 5);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        let is_over = ops::cpx( self.af.get8hi(),
                                val,
                                self.regs.bc.dec16_is_zero(),
                                flags);
        if let Some(pc) = pc {
            if !is_over {
                tsc.add_no_mreq(hl, 5);
                // MEMPTR = PC + 1
                self.memptr.set16((pc - Wrapping(1)).0);
                return Some(pc - Wrapping(2))
            }
        }
        // MEMPTR = MEMPTR +/- 1
        self.memptr.add16(delta as u16);
        None
    }

    pub(super) fn block_in<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ir:1, IO, hl:3, [hl:1 x 5]
        tsc.add_no_mreq(self.get_ir(), 1);
        let bc = self.regs.bc.get16();
        let val = control.read_io(bc, tsc.add_io(bc));
        let hl = self.regs.hl.get16();
        control.write_mem(hl, val, tsc.add_mreq(hl));
        // MEMPTR = BC_before_decrementing_B +/- 1
        self.memptr.set16(bc.wrapping_add(delta as u16));
        let b = ((bc >> 8) as u8).wrapping_sub(1);
        ops::iox(val, b, (bc as u8).wrapping_add(delta as u8), flags);
        self.regs.bc.set8hi(b);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        if let Some(pc) = pc {
            if b != 0 {
                tsc.add_no_mreq(hl, 5);
                return Some(pc - Wrapping(2))
            }
        }
        None
    }

    pub(super) fn block_out<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> (bool, Option<Wrapping<u16>>)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ir:1, hl:3, IO, [bc:1 x 5]
        tsc.add_no_mreq(self.get_ir(), 1);
        let hl = self.regs.hl.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl));
        let (b, c) = self.regs.bc.get();
        let hl1 = hl.wrapping_add(delta as u16);
        let b = b.wrapping_sub(1);
        ops::iox(val, b, hl1 as u8, flags);
        let bc = (b as u16) << 8 | c as u16;
        // MEMPTR = BC_after_decrementing_B +/- 1
        self.memptr.set16(bc.wrapping_add(delta as u16));
        let should_break = control.write_io(bc, val, tsc.add_io(bc));
        self.regs.bc.set8hi(b);
        self.regs.hl.set16(hl1);
        if let Some(pc) = pc {
            if b != 0 {
                tsc.add_no_mreq(bc, 5);
                return (should_break, Some(pc - Wrapping(2)))
            }
        }
        (should_break, None)
    }

    #[inline]
    pub(super) fn op8(&mut self, op: Ops8, val: u8, flags: &mut CpuFlags) {
        match op {
            Ops8::ADD => self.af.op8hi(|a| ops::add(a, val, flags)),
            Ops8::ADC => self.af.op8hi(|a| ops::adc(a, val, flags)),
            Ops8::SUB => self.af.op8hi(|a| ops::sub(a, val, flags)),
            Ops8::SBC => self.af.op8hi(|a| ops::sbc(a, val, flags)),
            Ops8::AND => self.af.op8hi(|a| ops::and(a, val, flags)),
            Ops8::XOR => self.af.op8hi(|a| ops::xor(a, val, flags)),
            Ops8::OR  => self.af.op8hi(|a| ops::or( a, val, flags)),
            Ops8::CP  => ops::cp(self.af.get8hi(), val, flags),
        }
    }
}
