/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! Private methods with implementation of some of the more complicated instructions of Z80.
use core::num::NonZeroU8;
use super::*;
use crate::NMI_RESTART;

type R8ParseResult = core::result::Result<Reg8,()>;

/// Constants for internal cycles.
pub (super) mod cycles {
    use core::num::NonZeroU8;
    macro_rules! def_consts_non_zero_u8 {
        ($($n:ident: $i:literal;)*) => { $(pub const $n: NonZeroU8 = unsafe { NonZeroU8::new_unchecked($i) };)* };
    }
    def_consts_non_zero_u8!{
        NO_MREQ_X1: 1;
        NO_MREQ_X2: 2;
        NO_MREQ_X4: 4;
        NO_MREQ_X5: 5;
        NO_MREQ_X7: 7;
    }
}
use cycles::*;

/// Determines the direction for the block instruction group.
#[derive(Clone, Copy, Debug)]
#[repr(i8)]
pub(super) enum BlockDelta {
    Increase = 1,
    Decrease = -1
}

pub(super) trait WordBytes {
    fn msb(self) -> u8;
    fn lsb(self) -> u8;
}

impl WordBytes for u16 {
    fn msb(self) -> u8 {
        (self >> 8) as u8
    }

    fn lsb(self) -> u8 {
        self as u8
    }
}

impl Rot {
    /// Calls one of the appriopriate rotate op function.
    fn op(self, v: u8, flags: &mut CpuFlags) -> u8 {
        match self {
            Rot::RLC  => ops::rlc(v, flags),
            Rot::RRC  => ops::rrc(v, flags),
            Rot::RL   => ops::rl(v, flags),
            Rot::RR   => ops::rr(v, flags),
            Rot::SLA  => ops::sla(v, flags),
            Rot::SRA  => ops::sra(v, flags),
            Rot::SLL  => ops::sll(v, flags),
            Rot::SRL  => ops::srl(v, flags)
        }
    }
}

impl<Q: Flavour> Z80<Q> {
    /// Reads 1 byte from memory via PC register (pc). Increases pc afterwards.
    /// Used for the M1 cycle (op-code fetch) so a proper number of M1 cycles is being added.
    /// Increases the memory refresh (R) counter.
    #[inline(always)]
    pub(super) fn fetch_next_opcode<M, T>(
        &mut self, control: &mut M, tsc: &mut T, pc: Wrapping<u16>
    ) -> (Wrapping<u16>, u8)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // pc:4, pc+=1
        self.inc_r();
        let code = control.read_opcode(pc.0, self.get_ir(), tsc.add_m1(pc.0));
        (pc + Wrapping(1), code)
    }

    #[inline]
    pub(super) fn fetch_next_imm8<M, T, const MREQ_ADD: u8>(
        &mut self, control: &mut M, tsc: &mut T, pc: Wrapping<u16>
    ) -> (Wrapping<u16>, u8)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // pc:3, pc+=1
        let code = control.read_mem(pc.0, tsc.add_mreq(pc.0));
        if let Some(add_ts_more) = NonZeroU8::new(MREQ_ADD) {
            // pc:1 x m
            // tsc.add_no_mreq::<MREQ_ADD>(pc.0);
            tsc.add_no_mreq(pc.0, add_ts_more);
        }
        (pc + Wrapping(1), code)
    }

    #[inline]
    pub(super) fn fetch_next_imm16<M, T, const MREQ_ADD: u8>(
        &mut self, control: &mut M, tsc: &mut T, pc: Wrapping<u16>
    ) -> (Wrapping<u16>, u16)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // pc:3, pc+1:3, pc+=2
        let nn: u16 = control.read_mem16(pc.0, tsc.add_mreq(pc.0));
        let pc1 = pc.0.wrapping_add(1);
        tsc.add_mreq(pc1);
        if let Some(add_ts_more) = NonZeroU8::new(MREQ_ADD) {
            // pc:1 x m
            // tsc.add_no_mreq::<MREQ_ADD>(pc1);
            tsc.add_no_mreq(pc1, add_ts_more);
        }
        (pc + Wrapping(2), nn)
    }

    #[inline(always)]
    pub(super) fn ex_de_hl(&mut self) {
        swap(&mut self.regs.de, &mut self.regs.hl);
    }

    #[inline(always)]
    pub(super) fn ex_af_af_with_flags(&mut self, flags: CpuFlags) -> CpuFlags {
        let a = self.af.get8hi();
        self.af = self.af_alt;
        self.af_alt.set(a, flags.bits());
        self.get_flags()
    }

    #[inline(always)]
    pub(super) fn load_reg(&mut self, dst: Reg8, src: Reg8, prefix: Option<Prefix>) {
        self.set_reg(dst, prefix, self.get_reg(src, prefix))
    }

    /// Force IRQ
    #[inline(never)]
    pub(super) fn irq_no_check<M, T, F>(&mut self, control: &mut M, tsc: &mut T, debug: Option<F>) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
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
        let (vector, wait_states) = control.irq_data(pc, bus_ts);
        if let Some(ws) = wait_states {
            tsc.add_wait_states(pc, ws);
        }
        match self.im {
            InterruptMode::Mode0 => {
                self.execute_instruction::<M,T,F>(control, tsc, debug, vector)
            }
            InterruptMode::Mode1 => {
                self.execute_instruction::<M,T,F>(control, tsc, debug, opconsts::RST_38H_OPCODE)
            }
            InterruptMode::Mode2 => {
                let ir = self.get_ir();
                // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(ir); // ir:1
                tsc.add_no_mreq(ir, NO_MREQ_X1); // ir:1
                self.push16(pc, control, tsc); // sp-1:3, sp-2:3
                let vaddr = ir & 0xFF00 | vector as u16;
                self.pc.set16(vaddr);
                self.execute_instruction::<M,T,F>(control, tsc, debug, opconsts::JP_OPCODE) // pc+1:3,pc+2:3
            }
        }
    }

    /// Force NMI
    #[inline(never)]
    pub(super) fn nmi_no_check<M, T>(&mut self, control: &mut M, tsc: &mut T)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
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
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(self.get_ir()); // ir:1
        tsc.add_no_mreq(self.get_ir(), NO_MREQ_X1); // ir:1
        self.push16(pc, control, tsc); // sp-1:3, sp-2:3
        self.flavour.begin_instruction();
        self.pc.set16(NMI_RESTART);
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

    pub(super) fn push_ss<M, T>(&mut self, control: &mut M, tsc: &mut T, ss: StkReg16, flags: CpuFlags)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ir:1, sp-1:3, sp-2:3
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(self.get_ir());
        tsc.add_no_mreq(self.get_ir(), NO_MREQ_X1);
        let (vhi, vlo) = match ss {
            StkReg16::BC => self.regs.bc.get(),
            StkReg16::DE => self.regs.de.get(),
            StkReg16::HL => self.regs.hl.get(),
            StkReg16::AF => (self.af.get8hi(), flags.bits())
        };
        // push2!(vhi, vlo);
        self.push2(vhi, vlo, control, tsc);
    }

    pub(super) fn pop_ss<M, T>(&mut self, control: &mut M, tsc: &mut T, ss: StkReg16, flags: &mut CpuFlags)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // sp:3,sp+1:3
        // let val: u16 = pop16!();
        let val = self.pop16(control, tsc);
        match ss {
            StkReg16::BC => self.regs.bc.set16(val),
            StkReg16::DE => self.regs.de.set16(val),
            StkReg16::HL => self.regs.hl.set16(val),
            StkReg16::AF => {
                *flags = CpuFlags::from_bits_retain(val as u8);
                self.af.set16(val);
            }
        }
    }

    #[inline(always)]
    pub(super) unsafe fn reg8_ptr(&mut self, tgt: Reg8, prefix: Option<Prefix>) -> *mut u8 {
        match tgt {
            Reg8::B => self.regs.bc.ptr8hi(),
            Reg8::C => self.regs.bc.ptr8lo(),
            Reg8::D => self.regs.de.ptr8hi(),
            Reg8::E => self.regs.de.ptr8lo(),
            Reg8::H => match prefix {
                None => self.regs.hl.ptr8hi(),
                Some(Prefix::Xdd) => self.index.ix.ptr8hi(),
                Some(Prefix::Yfd) => self.index.iy.ptr8hi(),
            }
            Reg8::L => match prefix {
                None => self.regs.hl.ptr8lo(),
                Some(Prefix::Xdd) => self.index.ix.ptr8lo(),
                Some(Prefix::Yfd) => self.index.iy.ptr8lo(),
            }
            Reg8::A => self.af.ptr8hi()
        }
    }

    #[inline(always)]
    pub(super) fn apply_reg8<F>(&mut self, tgt: Reg8, prefix: Option<Prefix>, op: F)
    where F: FnOnce(u8) -> u8
    {
        unsafe {
            let ptr = self.reg8_ptr(tgt, prefix);
            *ptr = op(*ptr);
        }
    }

    #[inline(always)]
    pub(super) fn reg16_ref(&self, src: Reg16) -> &RegisterPair {
        match src {
            Reg16::BC => &self.regs.bc,
            Reg16::DE => &self.regs.de,
            Reg16::HL => &self.regs.hl,
            Reg16::SP => &self.sp
        }        
    }

    #[inline(always)]
    pub(super) fn stkreg16_ref(&self, src: StkReg16) -> &RegisterPair {
        match src {
            StkReg16::BC => &self.regs.bc,
            StkReg16::DE => &self.regs.de,
            StkReg16::HL => &self.regs.hl,
            StkReg16::AF => &self.af
        }
    }

    #[inline(always)]
    pub(super) fn stkreg16_alt_ref(&self, src: StkReg16) -> &RegisterPair {
        match src {
            StkReg16::BC => &self.regs_alt.bc,
            StkReg16::DE => &self.regs_alt.de,
            StkReg16::HL => &self.regs_alt.hl,
            StkReg16::AF => &self.af_alt
        }
    }

    #[inline(always)]
    pub(super) fn get_prefix_reg16(&self, src: Reg16, prefix: Prefix) -> u16 {
        match src {
            Reg16::BC => self.regs.bc.get16(),
            Reg16::DE => self.regs.de.get16(),
            Reg16::HL => self.index16_ref(prefix).get16(),
            Reg16::SP => self.sp.get16()
        }        
    }

    #[inline(always)]
    pub(super) fn index16_ref(&self, prefix: Prefix) -> &RegisterPair {
        match prefix {
            Prefix::Xdd => &self.index.ix,
            Prefix::Yfd => &self.index.iy,
        }        
    }

    #[inline(always)]
    pub(super) fn reg16_mut(&mut self, src: Reg16) -> &mut RegisterPair {
        match src {
            Reg16::BC => &mut self.regs.bc,
            Reg16::DE => &mut self.regs.de,
            Reg16::HL => &mut self.regs.hl,
            Reg16::SP => &mut self.sp
        }        
    }

    #[inline(always)]
    pub(super) fn stkreg16_mut(&mut self, src: StkReg16) -> &mut RegisterPair {
        match src {
            StkReg16::BC => &mut self.regs.bc,
            StkReg16::DE => &mut self.regs.de,
            StkReg16::HL => &mut self.regs.hl,
            StkReg16::AF => &mut self.af
        }        
    }

    #[inline(always)]
    pub(super) fn index16_mut(&mut self, prefix: Prefix) -> &mut RegisterPair {
        match prefix {
            Prefix::Xdd => &mut self.index.ix,
            Prefix::Yfd => &mut self.index.iy,
        }        
    }

    #[inline(never)]
    pub(super) fn block_transfer<M, T>(
            &mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags,
            delta: BlockDelta, pc: Option<Wrapping<u16>>
        ) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // hl:3, de:3, de:1 x 2 [de:1 x 5]
        let hl = self.regs.hl.get16();
        let de = self.regs.de.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl));
        control.write_mem(de, val, tsc.add_mreq(de));
        // tsc.add_no_mreq::<{NO_MREQ_X2.get()}>(de);
        tsc.add_no_mreq(de, NO_MREQ_X2);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        self.regs.de.set16(de.wrapping_add(delta as u16));
        let is_over = self.regs.bc.dec16_is_zero();
        ops::ldx(self.af.get8hi(), val, is_over, flags);
        self.flavour.flags_modified();
        if let Some(pc) = pc {
            if !is_over {
                // tsc.add_no_mreq::<{NO_MREQ_X5.get()}>(de);
                tsc.add_no_mreq(de, NO_MREQ_X5);
                // MEMPTR = PC + 1
                self.memptr.set16((pc - Wrapping(1)).0);
                return Some(pc - Wrapping(2))
            }
        }
        None
    }

    #[inline(never)]
    pub(super) fn block_search<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // hl:3, hl:1 x 5, [hl:1 x 5]
        let hl = self.regs.hl.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl));
        // tsc.add_no_mreq::<{NO_MREQ_X5.get()}>(hl);
        tsc.add_no_mreq(hl, NO_MREQ_X5);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        let is_over = ops::cpx( self.af.get8hi(),
                                val,
                                self.regs.bc.dec16_is_zero(),
                                flags);
        self.flavour.flags_modified();
        if let Some(pc) = pc {
            if !is_over {
                // tsc.add_no_mreq::<{NO_MREQ_X5.get()}>(hl);
                tsc.add_no_mreq(hl, NO_MREQ_X5);
                // MEMPTR = PC + 1
                self.memptr.set16((pc - Wrapping(1)).0);
                return Some(pc - Wrapping(2))
            }
        }
        // MEMPTR = MEMPTR +/- 1
        self.memptr.add16(delta as u16);
        None
    }

    #[inline(never)]
    pub(super) fn block_in<M, T>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, delta: BlockDelta, pc: Option<Wrapping<u16>>) -> Option<Wrapping<u16>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ir:1, IO, hl:3, [hl:1 x 5]
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(self.get_ir());
        tsc.add_no_mreq(self.get_ir(), NO_MREQ_X1);
        let bc = self.regs.bc.get16();
        let (data, wait_states) = control.read_io(bc, tsc.add_io(bc));
        if let Some(ws) = wait_states {
            tsc.add_wait_states(bc, ws);
        }
        let hl = self.regs.hl.get16();
        control.write_mem(hl, data, tsc.add_mreq(hl));
        // MEMPTR = BC_before_decrementing_B +/- 1
        self.memptr.set16(bc.wrapping_add(delta as u16));
        let b = ((bc >> 8) as u8).wrapping_sub(1);
        ops::iox(data, b, (bc as u8).wrapping_add(delta as u8), flags);
        self.flavour.flags_modified();
        self.regs.bc.set8hi(b);
        self.regs.hl.set16(hl.wrapping_add(delta as u16));
        if let Some(pc) = pc {
            if b != 0 {
                // tsc.add_no_mreq::<{NO_MREQ_X5.get()}>(hl);
                tsc.add_no_mreq(hl, NO_MREQ_X5);
                return Some(pc - Wrapping(2))
            }
        }
        None
    }

    #[inline(never)]
    pub(super) fn block_out<M, T>(
            &mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags,
            delta: BlockDelta, pc: Option<Wrapping<u16>>
        ) -> (Option<M::WrIoBreak>, Option<Wrapping<u16>>)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ir:1, hl:3, IO, [bc:1 x 5]
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(self.get_ir());
        tsc.add_no_mreq(self.get_ir(), NO_MREQ_X1);
        let hl = self.regs.hl.get16();
        let data = control.read_mem(hl, tsc.add_mreq(hl));
        let (b, c) = self.regs.bc.get();
        let hl1 = hl.wrapping_add(delta as u16);
        let b = b.wrapping_sub(1);
        ops::iox(data, b, hl1 as u8, flags);
        self.flavour.flags_modified();
        let bc = (b as u16) << 8 | c as u16;
        // MEMPTR = BC_after_decrementing_B +/- 1
        self.memptr.set16(bc.wrapping_add(delta as u16));
        let (should_break, wait_states) = control.write_io(bc, data, tsc.add_io(bc));
        if let Some(ws) = wait_states {
            tsc.add_wait_states(bc, ws);
        }
        self.regs.bc.set8hi(b);
        self.regs.hl.set16(hl1);
        if let Some(pc) = pc {
            if b != 0 {
                // tsc.add_no_mreq::<{NO_MREQ_X5.get()}>(bc);
                tsc.add_no_mreq(bc, NO_MREQ_X5);
                return (should_break, Some(pc - Wrapping(2)))
            }
        }
        (should_break, None)
    }

    #[inline(always)]
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
        self.flavour.flags_modified();
    }

    #[inline(always)]
    pub(super) fn op8_with_arg<M, T>(
        &mut self, control: &mut M, tsc: &mut T, op: Ops8, arg: R8ParseResult, flags: &mut CpuFlags
    )
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    {
        let val: u8 = match arg {
            Ok(src) => self.get_reg(src, None),
            // hl:3
            Err(_) => self.read_mem8_hl::<_, _, 0>(control, tsc)
        };
        self.op8(op, val, flags);
    }

    #[inline(always)]
    pub(super) fn ld_a_from_mem8<M, T>(&mut self, control: &mut M, tsc: &mut T, addr: u16)
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // nn:3
        // LD A, (nn) MEMPTR = addr + 1
        self.memptr.set16(addr.wrapping_add(1));
        let n = control.read_mem(addr, tsc.add_mreq(addr));
        self.af.set8hi(n);
    }

    #[inline(always)]
    pub(super) fn ld_mem8_from_a<M, T>(&mut self, control: &mut M, tsc: &mut T, addr: u16)
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // nn:3
        let a = self.af.get8hi();
        // MEMPTR_low = (nn + 1) & #FF,  MEMPTR_hi = A
        self.memptr = Q::memptr_mix(a, addr as u8).into();
        control.write_mem(addr, a, tsc.add_mreq(addr));
    }

    #[inline]
    pub(super) fn ex_sp_nn<M, T>(&mut self, control: &mut M, tsc: &mut T, (vhi, vlo): (u8, u8)) -> u16
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    {
        // sp:3,sp+1:3,sp+1:1,sp+1(write):3,sp(write):3,sp(write):1 x 2
        let sp = self.sp.get16();
        let val = control.read_mem16(sp, tsc.add_mreq(sp));
        let sp_1 = sp.wrapping_add(1);
        tsc.add_mreq(sp_1);
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(sp_1);
        tsc.add_no_mreq(sp_1, NO_MREQ_X1);
        // let (vhi, vlo): (u8, u8) = $val2;
        control.write_mem(sp_1, vhi, tsc.add_mreq(sp_1));
        control.write_mem(sp, vlo, tsc.add_mreq(sp));
        // tsc.add_no_mreq::<{NO_MREQ_X2.get()}>(sp);
        tsc.add_no_mreq(sp, NO_MREQ_X2);
        // MEMPTR = rp value after the operation
        self.memptr.set16(val);
        val
    }

    #[inline]
    pub(super) fn instr_rxd<M, T, F>(&mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags, rxd: F)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(u8, u8, &mut CpuFlags) -> (u8, u8)
    { // hl:3, hl:1 x 4, hl(write):3
        let hl: u16 = self.regs.hl.get16();
        // RLD/RRD  MEMPTR = HL + 1
        self.memptr.set16(hl.wrapping_add(1));
        let val = control.read_mem(hl, tsc.add_mreq(hl));
        // tsc.add_no_mreq::<{NO_MREQ_X4.get()}>(hl);
        tsc.add_no_mreq(hl, NO_MREQ_X4);
        self.af.op8hi(|a| {
            let (a, res) = rxd(a, val, flags);
            control.write_mem(hl, res, tsc.add_mreq(hl));
            a
        });
        self.flavour.flags_modified();
    }

    pub(super) fn op16_reg16<T, F>(
        &mut self, tsc: &mut T, flags: &mut CpuFlags, op16: F, reg16: RegisterPair, nn: u16
    ) -> RegisterPair
    where T: Clock,
          F: FnOnce(u16, u16, &mut CpuFlags) -> (u8, u8)
    { // ir:1 x 7
        // tsc.add_no_mreq::<{NO_MREQ_X7.get()}>(self.get_ir());
        tsc.add_no_mreq(self.get_ir(), NO_MREQ_X7);
        let v: u16 = reg16.get16();
        // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
        self.memptr.set16(v.wrapping_add(1));
        let reg = op16(v, nn, flags).into();
        self.flavour.flags_modified();
        reg
    }

    #[inline(always)]
    pub(super) fn write_mem16_addr16<M, T>(
        &mut self, control: &mut M, tsc: &mut T, addr: u16, (vhi, vlo): (u8, u8)
    )
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
    { // nn:3, nn+1:3
        control.write_mem(addr, vlo, tsc.add_mreq(addr));
        let addr_1 = addr.wrapping_add(1);
        // LD (addr), rp; MEMPTR = addr + 1
        self.memptr.set16(addr_1);
        control.write_mem(addr_1, vhi, tsc.add_mreq(addr_1));
    }

    #[inline(always)]
    pub(super) fn read_mem16_addr16<M, T>(&mut self, control: &mut M, tsc: &mut T, addr: u16) -> u16
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock
    { // nn:3, nn+1:3
        // LD rp,(addr) MEMPTR = addr + 1
        let val = control.read_mem16(addr, tsc.add_mreq(addr));
        let addr1 = addr.wrapping_add(1);
        self.memptr.set16(addr1);
        tsc.add_mreq(addr1);
        val
    }

    /// Used by various 8-bit operations on the memory via one of the address registers: HL, IX or IY.
    pub(super) fn r_op_w_mem8<M, T, F>(&mut self, control: &mut M, tsc: &mut T, op8: F)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(u8) -> u8
    { // hl:3, hl:1, hl(write):3
        let hl: u16 = self.regs.hl.get16();
        let val: u8 = op8(control.read_mem(hl, tsc.add_mreq(hl)));
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(hl, );
        tsc.add_no_mreq(hl, NO_MREQ_X1);
        control.write_mem(hl, val, tsc.add_mreq(hl));
    }

    #[inline]
    pub(super) fn instr_inc_dec8<M, T, F>(
        &mut self, control: &mut M, tsc: &mut T, arg: R8ParseResult, flags: &mut CpuFlags, opfn: F
    )
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(u8, &mut CpuFlags) -> u8
    {
        match arg {
            Ok(src) => self.apply_reg8(src, None, |v| opfn(v, flags)),
            // hl:3,hl:1,hl(write):3
            Err(_) => self.r_op_w_mem8(control, tsc, |v| opfn(v, flags))
        }
        self.flavour.flags_modified();
    }

    #[inline(never)]
    pub(super) fn instr_inc_dec_x_mem8<M, T, F>(
        &mut self, control: &mut M, tsc: &mut T, prefix: Prefix, index8: u8, flags: &mut CpuFlags, opfn: F
    )
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(u8, &mut CpuFlags) -> u8
    {
        // ii+n:3, ii+n:1, ii+n(write):3
        // MEMPTR = INDEX+d
        let ii_d = indexed_address!(self.get_index16(prefix), index8);
        let val: u8 = opfn(control.read_mem(ii_d, tsc.add_mreq(ii_d)), flags);
        self.flavour.flags_modified();
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(ii_d);
        tsc.add_no_mreq(ii_d, NO_MREQ_X1);
        // Any instruction with (INDEX+d): MEMPTR = INDEX+d
        self.memptr.set16(ii_d);
        control.write_mem(ii_d, val, tsc.add_mreq(ii_d));
    }

    #[inline(never)]
    pub(super) fn instr_ld_a_from_ir<M, T, const I: bool>(
        &mut self, control: &mut M, tsc: &mut T, flags: &mut CpuFlags
    )
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    {
        let ir = self.get_ir();
        // ir:1
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(ir);
        tsc.add_no_mreq(ir, NO_MREQ_X1);
        let val = if I {
            (ir >> 8) as u8
        }
        else {
            ir as u8
        };
        let iff2 = if self.iff2 {
            !(Q::ACCEPTING_INT_RESETS_IFF2_EARLY && self.iff1 && control.is_irq(tsc.as_timestamp()))
        }
        else {
            false
        };
        ops::ld_a_ir(val, iff2, flags);
        self.flavour.flags_modified();
        self.af.set8hi(val);
    }

    #[inline(never)]
    pub(super) fn instr_ld_ir_from_a<T, const I: bool>(&mut self, tsc: &mut T)
    where T: Clock
    { // ir: 1
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(self.get_ir());
        tsc.add_no_mreq(self.get_ir(), NO_MREQ_X1);
        let a = self.af.get8hi();
        if I {
            self.set_i(a);
        }
        else {
            self.set_r(a);
        }
    }

    #[inline(always)]
    pub(super) fn instr_ld_a_from_rp<M, T, const BC: bool>(&mut self, control: &mut M, tsc: &mut T)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ss:3
        let rp16: u16 = if BC { self.regs.bc } else { self.regs.de }.get16();
        // MEMPTR = rp + 1
        self.memptr.set16(rp16.wrapping_add(1));
        let n = control.read_mem(rp16, tsc.add_mreq(rp16));
        self.af.set8hi(n);
    }

    #[inline(always)]
    pub(super) fn instr_ld_rp_from_a<M, T, const BC: bool>(&mut self, control: &mut M, tsc: &mut T)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ss:3
        let a = self.af.get8hi();
        let rp16: u16 = if BC { self.regs.bc } else { self.regs.de }.get16();
        // MEMPTR_low = (rp + 1) & #FF,  MEMPTR_hi = A
        self.memptr = Q::memptr_mix(a, rp16 as u8).into();
        // let (hi, lo): (u8, u8) = Q::memptr_mix(a, rp16 as u8);
        // self.memptr.set(hi, lo);
        control.write_mem(rp16, a, tsc.add_mreq(rp16));
    }

    #[inline(always)]
    pub(super) fn write_mem8_hl<M, T>(&mut self, control: &mut M, tsc: &mut T, val: u8)
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ss: 3
        let hl = self.regs.hl.get16();
        control.write_mem(hl, val, tsc.add_mreq(hl));
    }

    #[inline(always)]
    pub(super) fn write_mem8_ii_d<M, T>(
            &mut self, control: &mut M, tsc: &mut T, prefix: Prefix, index8: u8, val: u8
        )
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ii+d: 3
        let ii_d = indexed_address!(self.get_index16(prefix), index8);
        // Any instruction with (INDEX+d): MEMPTR = INDEX+d
        self.memptr.set16(ii_d);
        control.write_mem(ii_d, val, tsc.add_mreq(ii_d));
    }

    #[inline(always)]
    pub(super) fn read_mem8_hl<M, T, const MREQ_ADD: u8>(&mut self, control: &mut M, tsc: &mut T) -> u8
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // hl: 3
        let hl: u16 = self.regs.hl.get16();
        let val = control.read_mem(hl, tsc.add_mreq(hl));
        if let Some(add_ts_more) = NonZeroU8::new(MREQ_ADD) {
            // hl:1 x m
            // tsc.add_no_mreq::<MREQ_ADD>(hl);
            tsc.add_no_mreq(hl, add_ts_more);
        }
        val
    }

    #[inline(always)]
    pub(super) fn read_mem8_ii_d<M, T>(
            &mut self, control: &mut M, tsc: &mut T, prefix: Prefix, index8: u8
        ) -> u8
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ii+d: 3
        let ii_d = indexed_address!(self.get_index16(prefix), index8);
        // Any instruction with (INDEX+d): MEMPTR = INDEX+d
        self.memptr.set16(ii_d);
        control.read_mem(ii_d, tsc.add_mreq(ii_d))
    }

    #[inline(never)]
    pub(super) fn io_a_inp_an<M, T>(&mut self, control: &mut M, tsc: &mut T, n: u8)
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // IO
        let port: u16 = (self.af.get8hi() as u16) << 8 | n as u16;
        // MEMPTR = (A_before_operation << 8) + port + 1
        self.memptr.set16(port.wrapping_add(1));
        let (data, wait_states) = control.read_io(port, tsc.add_io(port));
        if let Some(ws) = wait_states {
            tsc.add_wait_states(port, ws);
        }
        self.af.set8hi(data);
    }

    #[inline(never)]
    pub(super) fn io_a_out_an<M, T>(&mut self, control: &mut M, tsc: &mut T, n: u8) -> Option<M::WrIoBreak>
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // IO
        let a = self.af.get8hi();
        // MEMPTR_low = (port + 1) & #FF,  MEMPTR_hi = A
        self.memptr = Q::memptr_mix(a, n).into();
        let port = u16::from_le_bytes([n, a]);
        let (should_break, wait_states) = control.write_io(port, a, tsc.add_io(port));
        if let Some(ws) = wait_states {
            tsc.add_wait_states(port, ws);
        }
        should_break
    }

    #[inline(never)]
    pub(super) fn io_r_inp_bc<M, T>(
            &mut self, control: &mut M, tsc: &mut T, arg: R8ParseResult, flags: &mut CpuFlags
        )
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // IO
        let bc = self.regs.bc.get16();
        let (data, wait_states) = control.read_io(bc, tsc.add_io(bc));
        if let Some(ws) = wait_states {
            tsc.add_wait_states(bc, ws);
        }
        ops::io(data, flags);
        self.flavour.flags_modified();
        // IN r,(C)     MEMPTR = BC + 1
        self.memptr.set16(bc.wrapping_add(1));
        if let Ok(dst) = arg { // IN r,(C)
            self.set_reg(dst, None, data);
        }
    }

    #[inline(never)]
    pub(super) fn io_r_out_bc<M, T>(
            &mut self, control: &mut M, tsc: &mut T, arg: R8ParseResult
        ) -> Option<M::WrIoBreak>
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // IO
        let bc = self.regs.bc.get16();
        // OUT (C),r    MEMPTR = BC + 1
        self.memptr.set16(bc.wrapping_add(1));
        let val = match arg {
            Ok(src) => {
                self.get_reg(src, None)
            }
            Err(_) => {
                Q::CONSTANT_OUT_DATA
            }
        };
        let (should_break, wait_states) = control.write_io(bc, val, tsc.add_io(bc));
        if let Some(ws) = wait_states {
            tsc.add_wait_states(bc, ws);
        }
        should_break
    }

    #[inline]
    pub(super) fn bitops<M, T>(
            &mut self, control: &mut M, tsc: &mut T, bitops: BitOps, flags: &mut CpuFlags
        )
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    {
        match bitops {
            BitOps::Rot(rot, arg) => {
                let rot_fn = |v| rot.op(v, flags);
                match arg {
                    Ok(reg) => self.apply_reg8(reg, None, rot_fn),
                    Err(_) => { // hl:3, hl:1, hl(write):3
                        self.r_op_w_mem8(control, tsc, rot_fn)
                        // r_op_w_mem8!(rot_fn [hl]);
                    }
                };
                self.flavour.flags_modified();
                // flags_op!();
            }
            BitOps::Bit(b, arg) => {
                match arg {
                    Ok(reg) => ops::bit(b, self.get_reg(reg, None), flags),
                    Err(_) => { // hl:3, hl:1
                        // let val = read_mem8_reg16!(<- [hl]; no_mreq: NO_MREQ_X1);
                        let val = self.read_mem8_hl::<_, _, { NO_MREQ_X1.get() }>(control, tsc);
                        ops::bit_mp(b, val, self.memptr.get8hi(), flags);
                    }
                };
                self.flavour.flags_modified();
                // flags_op!();
            }
            BitOps::Res(b, arg) => {
                let res_fn = |v| ops::res(b, v);
                match arg {
                    Ok(reg) => self.apply_reg8(reg, None, res_fn),
                    Err(_) => { // hl:3, hl:1, hl(write):3
                        self.r_op_w_mem8(control, tsc, res_fn)
                        // r_op_w_mem8!(setres_fn b, [hl]);
                    }
                }
            }
            BitOps::Set(b, arg) => {
                let set_fn = |v| ops::set(b, v);
                match arg {
                    Ok(reg) => self.apply_reg8(reg, None, set_fn),
                    Err(_) => { // hl:3, hl:1, hl(write):3
                        self.r_op_w_mem8(control, tsc, set_fn)
                        // r_op_w_mem8!(setres_fn b, [hl]);
                    }
                }
            }
        };
    }

    #[inline(never)]
    pub(super) fn bitops_qq<M, T>(
            &mut self, control: &mut M, tsc: &mut T,
            bitops: BitOps, flags: &mut CpuFlags,
            prefix: Prefix, index8: u8
        )
        where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>, T: Clock
    { // ii+n:3, ii+n:1, [ ii+n(write):3 ]
        let ii_d = indexed_address!(self.get_index16(prefix), index8);
        // Any instruction with (INDEX+d): MEMPTR = INDEX+d
        self.memptr.set16(ii_d);
        let val = control.read_mem(ii_d, tsc.add_mreq(ii_d));
        // tsc.add_no_mreq::<{NO_MREQ_X1.get()}>(ii_d);
        tsc.add_no_mreq(ii_d, NO_MREQ_X1);
        let (result, arg) = match bitops {
            BitOps::Rot(rot, arg) => {
                let rotres = rot.op(val, flags);
                self.flavour.flags_modified();
                (rotres, arg)
            }
            BitOps::Bit(b, _) => {
                ops::bit_mp(b, val, (ii_d >> 8) as u8, flags);
                self.flavour.flags_modified();
                return;
            }
            BitOps::Res(b, arg) => {
                (ops::res(b, val), arg)
            }
            BitOps::Set(b, arg) => {
                (ops::set(b, val), arg)
            }
        };
        // ii+n(write):3
        control.write_mem(ii_d, result, tsc.add_mreq(ii_d));
        if let Ok(reg) = arg {
            self.set_reg(reg, None, result);
        }
    }
}
