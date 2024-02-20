/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
#![macro_use]
//! This module contains implementation of an executor and a debugger for all of the Z80 mnemonics.
//!
//! A macro match_instructions in opcodes module dispatches the byte codes into mnemonics.
//!
//! To find a particular implementation just search for a mnemonic in capital letters or @ops
//! for arithmetic operations or @rot for bitwise shift and rotation.
//!
//! For some instructions the byte code dispatch is continued here, like recognizing the target, source
//! or the kind of an arithmetic or bitwise operation.
//!
//! The main logic for all of the instructions can be found here, but some repeatable operations, like
//! memory r/w are delegated further to another macros.
//!
//! All flag modifying operations are delegated to the functions in z80::ops module.
macro_rules! define_instructions_scoped {
    ($flags:ident, $pc:ident, $cpu:ident, $control:ident, $tsc:ident) => {
//#################################################################################//
//################################# MNEMONICS #####################################//
//#################################################################################//
macro_rules! run_mnemonic {
    (     NOP *                     @@@ $code0:expr, $code1:expr) => {
        cpu_debug!([$code0, $code1] "NOP*")
    };
    (     DAA                       @@@ $code:expr) => {
        instr_acc_op!(daa; [$code])
    };
    (     CPL                       @@@ $code:expr) => {
        instr_acc_op!(cpl; [$code])
    };
    (     NEG                       @@@ $code0:expr, $code1:expr) => {
        instr_acc_op!(neg; [$code0, $code1])
    };
    (     CCF                       @@@ $code:expr) => {
        {
            cpu_debug!([$code] CCF );
            let q = $cpu.flavour.get_q($cpu.af.get8hi(), $flags);
            ops::ccf(q, &mut $flags);
            flags_op!();
        }
    };
    (     SCF                       @@@ $code:expr) => {
        {
            cpu_debug!([$code] SCF );
            let q = $cpu.flavour.get_q($cpu.af.get8hi(), $flags);
            ops::scf(q, &mut $flags);
            flags_op!();
        }
    };
    (     NOP                       @@@ $code:expr) => {
        cpu_debug!([$code] NOP )
    };
    (     DI                        @@@ $code:expr) => {
        {
            cpu_debug!([$code] DI );
            $cpu.disable_interrupts();
        }
    };
    (     EI break $main:tt         @@@ $code:expr) => {
        {
            cpu_debug!([$code] EI );
            $cpu.enable_interrupts();
            break $main LoopExitReason::EnableInt;
        }
    };
    (     IM $mode:tt               @@@ $code0:expr, $code1:expr) => {
        {
            cpu_debug!([$code0, $code1] IM  m:$mode);
            $cpu.set_im(interrupt_mode!($mode));
        }
    };
    (     LD r,r | LD (HL),r | LD r,(HL) | HALT break $main:tt  @@@ $code:expr) => {
        {
            match Reg8::tuple_from_b5_3_and_b2_0($code) {
                (Ok(dst), Ok(src)) => {
                    cpu_debug!([$code] LD r:dst, r:src);
                    $cpu.load_reg(dst, src, None);
                }
                (Ok(dst), Err(_)) => {
                    cpu_debug!([$code] LD r:dst, addr:HL);
                    // ss:3
                    let n: u8 = read_mem8_reg16!(<- [hl]);
                    $cpu.set_reg(dst, None, n);
                }
                (Err(_), Ok(src)) => {
                    cpu_debug!([$code] LD addr:HL, r:src);
                    // ss:3
                    write_mem8_reg16!([hl] <- $cpu.get_reg(src, None));
                }
                _ => { // HALT
                    cpu_debug!([$code] HALT);
                    $pc -= Wrapping(1);
                    $cpu.halt();
                    break $main LoopExitReason::Halt;
                }
            }
        }
    };
    (     LD r,n | LD (HL),n        @@@ $code:expr) => {
        {   // pc+1:3
            let n: u8 = fetch_next_imm8!();
            match Reg8::from_b5_3($code) {
                Ok(dst) => {
                    cpu_debug!([$code, n] LD  r:dst, n:n);
                    $cpu.set_reg(dst, None, n);
                }
                Err(_) => { // pc+1:3, hl:3
                    cpu_debug!([$code, n] LD addr:HL, n:n);
                    write_mem8_reg16!([hl] <- n);
                }
            }
        }
    };
    (     LD A,(BC)                 @@@ $code:expr) => {
        instr_ld_rp! { A <- [bc]; [$code] }
    };
    (     LD (BC),A                 @@@ $code:expr) => {
        instr_ld_rp! { [bc] <- A; [$code] }
    };
    (     LD A,(DE)                 @@@ $code:expr) => {
        instr_ld_rp! { A <- [de]; [$code] }
    };
    (     LD (DE),A                 @@@ $code:expr) => {
        instr_ld_rp! { [de] <- A; [$code] }
    };
    (     LD A,(nn)                 @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            let nn: u16 = fetch_next_imm16!();
            cpu_debug!([$code, nn.lsb(), nn.msb()] LD r:A, adnn:nn);
            // nn:3
            // LD A, (nn) MEMPTR = addr + 1
            $cpu.ld_a_from_mem8($control, $tsc, nn);
            // $cpu.memptr.set16(nn.wrapping_add(1));
            // let n = $control.read_mem(nn, $tsc.add_mreq(nn));
            // $cpu.af.set8hi(n);
        }        
    };
    (     LD (nn),A                 @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            let nn: u16 = fetch_next_imm16!();
            cpu_debug!([$code, nn.lsb(), nn.msb()] LD adnn:nn, r:A);
            // nn: 3
            // MEMPTR_low = (nn + 1) & #FF,  MEMPTR_hi = A
            $cpu.ld_mem8_from_a($control, $tsc, nn);
            // let a: u8 = $cpu.af.get8hi();
            // let (hi, lo) = Q::memptr_mix(a, nn as u8);
            // $cpu.memptr.set(hi, lo);
            // $control.write_mem(nn, a, $tsc.add_mreq(nn));
        }
    };
    (     LD A,I                    @@@ $code0:expr, $code1:expr) => {
        instr_ld_ir! { A <- I; [$code0, $code1] }
    };
    (     LD I,A                    @@@ $code0:expr, $code1:expr) => {
        instr_ld_ir! { I <- A; [$code0, $code1] }
    };
    (     LD A,R                    @@@ $code0:expr, $code1:expr) => {
        instr_ld_ir! { A <- R; [$code0, $code1] }
    };
    (     LD R,A                    @@@ $code0:expr, $code1:expr) => {
        instr_ld_ir! { R <- A; [$code0, $code1] }
    };
    (     LD dd,nn                  @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            let nn: u16 = fetch_next_imm16!();
            let dst = Reg16::from($code);
            cpu_debug!([$code, nn.lsb(), nn.msb()] LD rr:dst, nn:nn);
            $cpu.reg16_mut(dst).set16(nn);
        }
    };
    (     LD HL,(nn)                @@@ $code:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr.lsb(), addr.msb()] LD rr:HL, adnn:addr);
            // LD rp,(addr) MEMPTR = addr + 1
            let nn: u16 = read_mem16_addr16!(<- [addr] memptr=addr+1);
            $cpu.regs.hl.set16(nn);
        }
    };
    (     LD (nn),HL                @@@ $code:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr.lsb(), addr.msb()] LD adnn:addr, rr:HL);
            // LD (addr),rp MEMPTR = addr + 1
            write_mem16_addr16!([addr] <- $cpu.regs.hl.get(); memptr=addr+1);
        }
    };
    (     LD dd,(nn)                @@@ $code0:expr, $code1:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            let reg = Reg16::from($code1);
            cpu_debug!([$code0, $code1, addr.lsb(), addr.msb()] LD rr:reg, adnn:addr);
            // LD rp,(addr) MEMPTR = addr + 1
            let nn: u16 = read_mem16_addr16!(<- [addr] memptr=addr+1);
            $cpu.reg16_mut(reg).set16(nn);
        }
    };
    (     LD (nn),dd                @@@ $code0:expr, $code1:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            let reg = Reg16::from($code1);
            cpu_debug!([$code0, $code1, addr.lsb(), addr.msb()] LD adnn:addr, rr:reg);
            // LD (addr),rp MEMPTR = addr + 1
            write_mem16_addr16!([addr] <- $cpu.reg16_ref(reg).get(); memptr=addr+1);
        }
    };
    (     LD SP,HL                  @@@ $code:expr) => {
        { // ir:1 x 2
            cpu_debug!([$code] LD rr:SP, rr:HL);
            // $tsc.add_no_mreq::<{NO_MREQ_X2.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X2);
            $cpu.sp.set16($cpu.regs.hl.get16());
        }
    };
    (     PUSH ss                   @@@ $code:expr) => {
        { // ir:1, sp-1:3, sp-2:3
            // $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X1);
            let ss = StkReg16::from($code);
            cpu_debug!([$code] PUSH ss:ss);
            $cpu.push_ss($control, $tsc, ss, $flags);
            // let (vhi, vlo) = match ss {
            //     StkReg16::BC => $cpu.regs.bc.get(),
            //     StkReg16::DE => $cpu.regs.de.get(),
            //     StkReg16::HL => $cpu.regs.hl.get(),
            //     StkReg16::AF => ($cpu.af.get8hi(), $flags.bits())
            // };
            // push2!(vhi, vlo);
        }
    };
    (     POP ss                    @@@ $code:expr) => {
        { // sp:3,sp+1:3
            let ss = StkReg16::from($code);
            cpu_debug!([$code] POP ss:ss);
            $cpu.pop_ss($control, $tsc, ss, &mut $flags);
            // let val: u16 = pop16!();
            // match ss {
            //     StkReg16::BC => $cpu.regs.bc.set16(val),
            //     StkReg16::DE => $cpu.regs.de.set16(val),
            //     StkReg16::HL => $cpu.regs.hl.set16(val),
            //     StkReg16::AF => {
            //         $flags = CpuFlags::from_bits_retain(val as u8);
            //         $cpu.af.set16(val);
            //     }
            // }
        }
    };
    (     EX DE,HL                  @@@ $code:expr) => {
        {
            cpu_debug!([$code] EX rr:DE, rr:HL);
            $cpu.ex_de_hl();
        }
    };
    (     EX AF,AF^                 @@@ $code:expr) => {
        {
            cpu_debug!([$code] EX  ss:AF, ss:AF);
            $flags = $cpu.ex_af_af_with_flags($flags);
        }
    };
    (     EXX                       @@@ $code:expr) => {
        {
            cpu_debug!([$code] EXX );
            $cpu.exx();
        }
    };
    (     EX (SP),HL                @@@ $code:expr) => {
        { // sp:3, sp+1:3, sp+1:1, sp+1(write):3, sp(write):3, sp(write):1 x 2
            cpu_debug!([$code] EX addr:SP, rr:HL);
            // MEMPTR = rp value after the operation
            let val: u16 = ex_sp_nn!($cpu.regs.hl.get());
            $cpu.regs.hl.set16(val);
        }
    };
    (     LDI                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2
            cpu_debug!([$code0, $code1] LDI );
            $cpu.block_transfer::<M, T>($control, $tsc, &mut $flags, BlockDelta::Increase, None);
            // flags_op!();
        }
    };
    (     LDIR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2 [de:1 x 5]
            cpu_debug!([$code0, $code1] LDIR );
            if let Some(pc) = $cpu.block_transfer::<M, T>($control, $tsc, &mut $flags,
                                                            BlockDelta::Increase, Some($pc)) {
                $pc = pc;
            }
            // flags_op!();
        }
    };
    (     LDD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2
            cpu_debug!([$code0, $code1] LDD );
            $cpu.block_transfer::<M, T>($control, $tsc, &mut $flags, BlockDelta::Decrease, None);
            // flags_op!();
        }
    };
    (     LDDR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2 [de:1 x 5]
            cpu_debug!([$code0, $code1] LDDR );
            if let Some(pc) = $cpu.block_transfer::<M, T>($control, $tsc, &mut $flags,
                                                            BlockDelta::Decrease, Some($pc)) {
                $pc = pc;
            }
            // flags_op!();
        }
    };
    (     CPI                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5
            cpu_debug!([$code0, $code1] CPI );
            $cpu.block_search::<M, T>($control, $tsc, &mut $flags, BlockDelta::Increase, None);
            // flags_op!();
        }
    };
    (     CPIR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5, [hl:1 x 5]
            cpu_debug!([$code0, $code1] CPIR );
            if let Some(pc) = $cpu.block_search::<M, T>($control, $tsc, &mut $flags,
                                                        BlockDelta::Increase, Some($pc)) {
                $pc = pc;
            }
            // flags_op!();
        }
    };
    (     CPD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5
            cpu_debug!([$code0, $code1] CPD );
            $cpu.block_search::<M, T>($control, $tsc, &mut $flags, BlockDelta::Decrease, None);
            // flags_op!();
        }
    };
    (     CPDR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5, [hl:1 x 5]
            cpu_debug!([$code0, $code1] CPDR );
            if let Some(pc) = $cpu.block_search::<M, T>($control, $tsc, &mut $flags,
                                                        BlockDelta::Decrease, Some($pc)) {
                $pc = pc;
            }
            // flags_op!();
        }
    };
    (     @ops A,r | @ops A,(HL)    @@@ $code:expr) => {
        {
            let arg = Reg8::from_b2_0($code);
            let op = Ops8::from($code);
            cpu_debug!([$code] op8(op) r_addr:arg);
            $cpu.op8_with_arg($control, $tsc, op, arg, &mut $flags);
            // let val: u8 = match arg {
            //     Ok(src) => $cpu.get_reg(src, None),
            //     // hl:3
            //     Err(_) => read_mem8_reg16!(<- [hl])
            // };
            // $cpu.op8(op, val, &mut $flags);
            // flags_op!();
        }
    };
    (     @ops A,n                  @@@ $code:expr) => {
        { // pc+1:3
            let n: u8 = fetch_next_imm8!();
            let op = Ops8::from($code);
            cpu_debug!([$code, n] op8(op) n:n);
            $cpu.op8(op, n, &mut $flags);
            // flags_op!();
        }
    };
    (     INC r | INC (HL)          @@@ $code:expr) => {
        instr_inc_dec8!( inc r|[hl]; [$code])
    };
    (     DEC r | DEC (HL)          @@@ $code:expr) => {
        instr_inc_dec8!( dec r|[hl]; [$code])
    };
    (     ADD HL,dd                 @@@ $code:expr) => {
        { // ir:1 x 7
            let src = Reg16::from($code);
            cpu_debug!([$code] ADD rr:HL, rr:src);
            // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
            op16_reg16!(add16: $cpu.regs.hl, $cpu.reg16_ref(src).get16());
        }
    };
    (     ADC HL,dd                 @@@ $code0:expr, $code1:expr) => {
        { // ir:1 x 7
            let src = Reg16::from($code1);
            cpu_debug!([$code0, $code1] ADC rr:HL, rr:src);
            // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
            op16_reg16!(adc16: $cpu.regs.hl, $cpu.reg16_ref(src).get16());
        }
    };
    (     SBC HL,dd                 @@@ $code0:expr, $code1:expr) => {
        { // ir:1 x 7
            let src = Reg16::from($code1);
            cpu_debug!([$code0, $code1] SBC rr:HL, rr:src);
            // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
            op16_reg16!(sbc16: $cpu.regs.hl, $cpu.reg16_ref(src).get16());
        }
    };
    (     INC dd                    @@@ $code:expr) => {
        { // ir:1 x 2
            // $tsc.add_no_mreq::<{NO_MREQ_X2.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X2);
            let tgt = Reg16::from($code);
            cpu_debug!([$code] INC rr:tgt);
            $cpu.reg16_mut(tgt).inc16();
        }
    };
    (     DEC dd                    @@@ $code:expr) => {
        { // ir:1 x 2
            // $tsc.add_no_mreq::<{NO_MREQ_X2.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X2);
            let tgt = Reg16::from($code);
            cpu_debug!([$code] DEC rr:tgt);
            $cpu.reg16_mut(tgt).dec16();
        }
    };
    (     RLCA                      @@@ $code:expr) => {
        instr_acc_op!(rlca; [$code])
    };
    (     RRCA                      @@@ $code:expr) => {
        instr_acc_op!(rrca; [$code])
    };
    (     RLA                       @@@ $code:expr) => {
        instr_acc_op!(rla; [$code])
    };
    (     RRA                       @@@ $code:expr) => {
        instr_acc_op!(rra; [$code])
    };
    (     @rot|BIT|RES|SET r|(HL)   @@@ $code0:expr) => {
        { // pc+1:4 [hl:3, hl:1, hl(write):3]
            let code1 = fetch_next_opcode!();
            let bops = BitOps::from(code1);
            cpu_debug!([$code0, code1] bitops(bops));
            // match bops {
            //     BitOps::Rot(rot, arg) => cpu_debug!([$code0, code1] str(rot) r_addr:arg),
            //     BitOps::Bit(b, arg) => cpu_debug!([$code0, code1] BIT  b:b, r_addr:arg),
            //     BitOps::Res(b, arg) => cpu_debug!([$code0, code1] RES  b:b, r_addr:arg),
            //     BitOps::Set(b, arg) => cpu_debug!([$code0, code1] SET  b:b, r_addr:arg)
            // }
            $cpu.bitops($control, $tsc, bops, &mut $flags);
            // loop {
            //     let (setres_fn, b, arg) : (fn (u32, u8) -> u8, _, _) = match BitOps::from(code1) {
            //         BitOps::Rot(rot, arg) => {
            //             cpu_debug!([$code0, code1] str(rot) r_addr:arg);
            //             let rot_fn: fn(u8, &mut CpuFlags) -> u8 = rot.into();
            //             match arg {
            //                 Ok(reg) => $cpu.apply_reg8(reg, None, |v| rot_fn(v, &mut $flags)),
            //                 Err(_) => { // hl:3, hl:1, hl(write):3
            //                     r_op_w_mem8!(rot_fn [hl]);
            //                 }
            //             };
            //             flags_op!();
            //             break;
            //         }
            //         BitOps::Bit(b, arg) => {
            //             cpu_debug!([$code0, code1] BIT  b:b, r_addr:arg);
            //             match arg {
            //                 Ok(reg) => ops::bit(b, $cpu.get_reg(reg, None), &mut $flags),
            //                 Err(_) => { // hl:3, hl:1
            //                     let val = read_mem8_reg16!(<- [hl]; no_mreq: NO_MREQ_X1);
            //                     ops::bit_mp(b, val, $cpu.memptr.get8hi(), &mut $flags);
            //                 }
            //             };
            //             flags_op!();
            //             break;
            //         }
            //         BitOps::Res(b, arg) => {
            //             cpu_debug!([$code0, code1] RES  b:b, r_addr:arg);
            //             (ops::res, b, arg)
            //         }
            //         BitOps::Set(b, arg) => {
            //             cpu_debug!([$code0, code1] SET  b:b, r_addr:arg);
            //             (ops::set, b, arg)
            //         }
            //     };
            //     match arg {
            //         Ok(reg) => $cpu.apply_reg8(reg, None, |v| setres_fn(b, v)),
            //         Err(_) => { // hl:3, hl:1, hl(write):3
            //             r_op_w_mem8!(setres_fn b, [hl]);
            //         }
            //     };
            //     break;
            // }
        }
    };
    (     RLD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 4, hl(write):3
            cpu_debug!([$code0, $code1] RLD );
            instr_rxd!(ops::rld);
        }
    };
    (     RRD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 4, hl(write):3
            cpu_debug!([$code0, $code1] RRD );
            instr_rxd!(ops::rrd);
        }
    };
    (     JP nn                     @@@ $code:expr) => {
        { // pc+1:3, pc+2:3
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr.lsb(), addr.msb()] JP nn:addr);
            // JP   MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     JP cc,nn                  @@@ $code:expr) => {
        { // pc+1:3, pc+2:3
            let addr: u16 = fetch_next_imm16!();
            let cc = Condition::from($code);
            cpu_debug!([$code, addr.lsb(), addr.msb()] JP cc:cc, nn:addr);
            // JP(except JP rp)/CALL addr (even in case of conditional call/jp, independantly on condition satisfied or not)
            // MEMPTR = addr
            $cpu.memptr.set16(addr);
            if cc.is_satisfied($flags) {
                $pc = Wrapping(addr);
            }
        }
    };
    (     JR e                      @@@ $code:expr) => {
        { // pc+1:3, pc+1:1 x 5
            let e: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X5);
            let addr = relative_jump_address!(e);
            cpu_debug!([$code, e] JR nn_0:addr);
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr.0);
            $pc = addr;
        }
    };
    (     JR C,e | JR NC, e | JR Z,e | JR NZ,e  @@@ $code:expr) => {
        { // pc+1:3, [pc+1:1 x 5]
            let cc = Condition::from_jr_subset($code);
            if cc.is_satisfied($flags) {
                let e: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X5);
                let addr = relative_jump_address!(e);
                cpu_debug!([$code, e] JR cc:cc, nn_0:addr);
                // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
                $cpu.memptr.set16(addr.0);
                $pc = addr;
            }
            else {
                $tsc.add_mreq($pc.0);
                let e = $control.read_debug($pc.0);
                $pc += Wrapping(1);
                cpu_debug!([$code, e] JR cc:cc, rel:e);
            }
        }
    };
    (     JP (HL)                   @@@ $code:expr) => {
        {
            cpu_debug!([$code] JP addr:HL);
            $pc = Wrapping($cpu.regs.hl.get16());
        }
    };
    (     DJNZ e                    @@@ $code:expr) => {
        { // ir:1,pc+1:3,[pc+1:1 x 5]
            // $tsc.add_no_mreq::<{NO_MREQ_X1.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X1);
            // let memptr = &mut $cpu.memptr;
            // $cpu.regs.bc.op8hi(|b| {
            let b = $cpu.regs.bc.get8hi().wrapping_sub(1);
            if b != 0 {
                let e: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X5);
                let addr = relative_jump_address!(e);
                cpu_debug!([$code, e] DJNZ nn_0:addr);
                // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
                // memptr.set16(addr.0);
                $cpu.memptr.set16(addr.0);
                $pc = addr;
            }
            else {
                $tsc.add_mreq($pc.0);
                let e = $control.read_debug($pc.0);
                $pc += Wrapping(1);
                cpu_debug!([$code, e] DJNZ rel:e);
            }
            $cpu.regs.bc.set8hi(b);
        }
    };
    (     CALL nn                   @@@ $code:expr) => {
        { // pc+1:3, pc+2:3, pc+2:1, sp-1:3, sp-2:3
            let addr: u16 = fetch_next_imm16!(no_mreq: NO_MREQ_X1);
            cpu_debug!([$code, addr.lsb(), addr.msb()] CALL nn:addr);
            push16!($pc.0);
            // CALL addr MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     CALL cc, nn               @@@ $code:expr) => {
        { // pc+1:3, pc+2:3, [pc+2:1,sp-1:3,sp-2:3]
            let addr: u16 = fetch_next_imm16!();
            let cc = Condition::from($code);
            cpu_debug!([$code, addr.lsb(), addr.msb()] CALL cc:cc, nn:addr);
            // CALL addr (even in case of conditional call/jp, independantly on condition satisfied or not)
            // MEMPTR = addr
            $cpu.memptr.set16(addr);
            if cc.is_satisfied($flags) {
                // $tsc.add_no_mreq::<{NO_MREQ_X1.get()}>($pc.0.wrapping_sub(1));
                $tsc.add_no_mreq($pc.0.wrapping_sub(1), NO_MREQ_X1);
                push16!($pc.0);
                $pc = Wrapping(addr);
            }
        }
    };
    (     RET                       @@@ $code:expr) => {
        { // sp:3, sp+1:3
            cpu_debug!([$code] RET );
            let addr: u16 = pop16!();
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     RET cc                    @@@ $code:expr) => {
        { // ir:1, [sp:3,sp+1:3]
            // $tsc.add_no_mreq::<{NO_MREQ_X1.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X1);
            let cc = Condition::from($code);
            cpu_debug!([$code] RET cc:cc);
            if cc.is_satisfied($flags) {
                let addr: u16 = pop16!();
                // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
                $cpu.memptr.set16(addr);
                $pc = Wrapping(addr);
            }
        }
    };
    (     RETN | RETI | break $main:tt  @@@ $code0:expr, $code1:expr) => {
        { // sp:3, sp+1:3
            $cpu.restore_iff1(); // Both RETI and RETN do that.
            let should_break = if $code1 == opconsts::RETI_OPCODE_T2.1 {
                cpu_debug!([$code0, $code1] RETI );
                $control.reti($pc.0, $tsc.as_timestamp())
            }
            else {
                cpu_debug!([$code0, $code1] RETN );
                None
            };
            let addr: u16 = pop16!();
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
            if let Some(cause) = should_break {
                break $main LoopExitReason::Reti(cause);
            }
        }
    };
    (     RST p                     @@@ $code:expr) => {
        { // ir:1, sp-1:3, sp-2:3
            // $tsc.add_no_mreq::<{NO_MREQ_X1.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X1);
            let addr = parse_restart_address($code);
            cpu_debug!([$code] RST p:addr);
            push16!($pc.0);
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     IN A,(n)                  @@@ $code:expr) => {
        { // pc+1:3, IO
            let n: u8 = fetch_next_imm8!();
            cpu_debug!([$code, n] IN r:A, port:n);
            $cpu.io_a_inp_an($control, $tsc, n);
            // let port: u16 = ($cpu.af.get8hi() as u16) << 8 | n as u16;
            // // MEMPTR = (A_before_operation << 8) + port + 1
            // $cpu.memptr.set16(port.wrapping_add(1));
            // let (data, wait_states) = $control.read_io(port, $tsc.add_io(port));
            // if let Some(ws) = wait_states {
            //     $tsc.add_wait_states(port, ws);
            // }
            // $cpu.af.set8hi(data);
        }
    };
    (     IN r,(C)                  @@@ $code0:expr, $code1:expr) => {
        { // IO
            let arg = Reg8::from_b5_3($code1);
            cpu_debug!([$code0, $code1] IN maybe_r:arg, port:C);
            $cpu.io_r_inp_bc($control, $tsc, arg, &mut $flags);
            // let bc = $cpu.regs.bc.get16();
            // let (data, wait_states) = $control.read_io(bc, $tsc.add_io(bc));
            // if let Some(ws) = wait_states {
            //     $tsc.add_wait_states(bc, ws);
            // }
            // ops::io(data, &mut $flags);
            // // IN r,(C)     MEMPTR = BC + 1
            // $cpu.memptr.set16(bc.wrapping_add(1));
            // match arg {
            //     Ok(dst) => { // IN r,(C)
            //         cpu_debug!([$code0, $code1] IN r:dst, port:C);
            //         $cpu.set_reg(dst, None, data);
            //     }
            //     Err(_) => { // IN F, (c)
            //         cpu_debug!([$code0, $code1] IN port:C);
            //     }
            // }
            // flags_op!();
        }
    };
    (     INI                       @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3
            cpu_debug!([$code0, $code1] INI );
            $cpu.block_in::<M, T>($control, $tsc, &mut $flags, BlockDelta::Increase, None);
            // flags_op!();
        }
    };
    (     INIR                      @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3, [hl:1 x 5]
            cpu_debug!([$code0, $code1] INIR );
            if let Some(pc) = $cpu.block_in::<M, T>($control, $tsc, &mut $flags,
                                                        BlockDelta::Increase, Some($pc)) {
                $pc = pc;
            }
            // flags_op!();
        }
    };
    (     IND                       @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3
            cpu_debug!([$code0, $code1] IND );
            $cpu.block_in::<M, T>($control, $tsc, &mut $flags, BlockDelta::Decrease, None);
            // flags_op!();
        }
    };
    (     INDR                      @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3, [hl:1 x 5]
            cpu_debug!([$code0, $code1] INDR );
            if let Some(pc) = $cpu.block_in::<M, T>($control, $tsc, &mut $flags,
                                                        BlockDelta::Decrease, Some($pc)) {
                $pc = pc;
            }
            // flags_op!();
        }
    };
    (     OUT (n),A | break $main:tt  @@@ $code:expr) => {
        { // pc+1:3, IO
            let n: u8 = fetch_next_imm8!();
            cpu_debug!([$code, n] OUT port:n, r:A);
            // let a = $cpu.af.get8hi();
            // let (hi, lo) = Q::memptr_mix(a, n); // MEMPTR_low = (port + 1) & #FF,  MEMPTR_hi = A
            // $cpu.memptr.set(hi, lo);
            // let port = u16::from_le_bytes([n, a]);
            // let (should_break, wait_states) = $control.write_io(port, a, $tsc.add_io(port));
            // if let Some(ws) = wait_states {
            //     $tsc.add_wait_states(port, ws);
            // }
            if let Some(cause) = $cpu.io_a_out_an($control, $tsc, n) {
                break $main LoopExitReason::WriteIo(cause);
            }
        }
    };
    (     OUT (C),r | break $main:tt  @@@ $code0:expr, $code1:expr) => {
        { // IO
            let arg = Reg8::from_b5_3($code1);
            cpu_debug!([$code0, $code1] OUT port:C, maybe_r:arg);
            // let bc = $cpu.regs.bc.get16();
            // // OUT (C),r    MEMPTR = BC + 1
            // $cpu.memptr.set16(bc.wrapping_add(1));
            // let val = match Reg8::from_b5_3($code1) {
            //     Ok(src) => {
            //         cpu_debug!([$code0, $code1] OUT port:C, r:src);
            //         $cpu.get_reg(src, None)
            //     }
            //     Err(_) => {
            //         cpu_debug!([$code0, $code1] OUT port:C);
            //         Q::CONSTANT_OUT_DATA
            //     }
            // };
            // let (should_break, wait_states) = $control.write_io(bc, val, $tsc.add_io(bc));
            // if let Some(ws) = wait_states {
            //     $tsc.add_wait_states(bc, ws);
            // }
            if let Some(cause) = $cpu.io_r_out_bc($control, $tsc, arg) {
                break $main LoopExitReason::WriteIo(cause);
            }
        }
    };
    (     OUTI | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO
            cpu_debug!([$code0, $code1] OUTI );
            let (should_break, _) = $cpu.block_out::<M, T>($control, $tsc, &mut $flags, BlockDelta::Increase, None);
            // flags_op!();
            if let Some(cause) = should_break {
                break $main LoopExitReason::WriteIo(cause);
            }
        }
    };
    (     OTIR | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO, [bc:1 x 5]
            cpu_debug!([$code0, $code1] OTIR );
            let (should_break, maybe_pc) = $cpu.block_out::<M, T>($control, $tsc, &mut $flags,
                                                                    BlockDelta::Increase, Some($pc));
            // flags_op!();
            if let Some(pc) = maybe_pc { $pc = pc; }
            if let Some(cause) = should_break {
                break $main LoopExitReason::WriteIo(cause);
            }
        }
    };
    (     OUTD | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO
            cpu_debug!([$code0, $code1] OUTD );
            let (should_break, _) = $cpu.block_out::<M, T>($control, $tsc, &mut $flags, BlockDelta::Decrease, None);
            // flags_op!();
            if let Some(cause) = should_break {
                break $main LoopExitReason::WriteIo(cause);
            }
        }
    };
    (     OTDR | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO, [bc:1 x 5]
            cpu_debug!([$code0, $code1] OTDR );
            let (should_break, maybe_pc) = $cpu.block_out::<M, T>($control, $tsc, &mut $flags,
                                                                    BlockDelta::Decrease, Some($pc));
            // flags_op!();
            if let Some(pc) = maybe_pc { $pc = pc; }
            if let Some(cause) = should_break {
                break $main LoopExitReason::WriteIo(cause);
            }
        }
    };
    ($statement:stmt;               @@@ $_:expr) => {
        {
            $statement
        }
    };
}
//#################################################################################//
//#################################### HELPERS ####################################//
//#################################################################################//
macro_rules! instr_ld_rp {
    (@const bc) => { true };
    (@const de) => { false };
    // LD A, (rp); rp: BC or DE
    (A <- [$reg16:ident]; [$code:expr]) => {
        { // ss:3
            cpu_debug!([$code] LD r:A, addr:$reg16);
            // MEMPTR = rp + 1
            $cpu.instr_ld_a_from_rp::<_, _, {instr_ld_rp!(@const $reg16)}>($control, $tsc);
            // let n: u8 = read_mem8_reg16!(<- [$reg16] memptr=$reg16.wrapping_add(1));
            // $cpu.af.set8hi(n);
        }
    };
    // LD (rp), A; rp: BC or DE
    ([$reg16:ident] <- A; [$code:expr]) => {
        { // ss: 3
            cpu_debug!([$code] LD addr:$reg16, r:A);
            // MEMPTR_low = (rp + 1) & #FF,  MEMPTR_hi = A
            $cpu.instr_ld_rp_from_a::<_, _, {instr_ld_rp!(@const $reg16)}>($control, $tsc);
            // let a: u8 = $cpu.af.get8hi();
            // write_mem8_reg16!([$reg16] <- a; memptr=Q::memptr_mix(a, $reg16 as u8));
        }
    };
}

macro_rules! instr_ld_ir {
    (@const I) => { true };
    (@const R) => { false };
    // LD A,I; LD A,R
    (A <- $reg:tt; [$code0:expr, $code1:expr]) => {
        { // ir:1
            cpu_debug!([$code0, $code1] LD r:A, r:$reg);
            $cpu.instr_ld_a_from_ir::<_, _, {instr_ld_ir!(@const $reg)}>($control, $tsc, &mut $flags);
            // let ir = $cpu.get_ir();
            // $tsc.add_no_mreq(ir, NO_MREQ_X1);
            // let val = instr_ld_ir!(@extract $reg <- ir);
            // let iff2 = if $cpu.iff2 {
            //     !(Q::ACCEPTING_INT_RESETS_IFF2_EARLY && $cpu.iff1 && $control.is_irq($tsc.as_timestamp()))
            // }
            // else {
            //     false
            // };
            // ops::ld_a_ir(val, iff2, &mut $flags);
            // flags_op!();
            // $cpu.af.set8hi(val);
        }
    };
    // (@extract I <- $ir:ident) => { ($ir >> 8) as u8 };
    // (@extract R <- $ir:ident) => { $ir as u8 };
    // LD I,A; LD R,A
    ($reg:tt <- A; [$code0:expr, $code1:expr]) => {
        { // ir:1
            cpu_debug!([$code0, $code1] LD r:$reg, r:A);
            $cpu.instr_ld_ir_from_a::<_, {instr_ld_ir!(@const $reg)}>($tsc);
    //         $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X1);
    //         let a = $cpu.af.get8hi();
    //         instr_ld_ir!(@store $reg <- a);
        }
    };
    // (@store I <- $a:ident) => { $cpu.ir.set8hi($a); };
    // (@store R <- $a:ident) => { $cpu.set_r($a); };
}

// Unary accumulator operators
macro_rules! instr_acc_op {
    ($op:ident; [$code:expr]) => {
        {
            cpu_debug!([$code] str(acc_op_str!($op)));
            $cpu.af.op8hi(|a| ops::$op(a, &mut $flags));
            flags_op!();
        }
    };
    ($op:ident; [$code0:expr, $code1:expr]) => {
        {
            cpu_debug!([$code0, $code1] str(acc_op_str!($op)));
            $cpu.af.op8hi(|a| ops::$op(a, &mut $flags));
            flags_op!();
        }
    };
}

// 8-bit INC/DEC
macro_rules! instr_inc_dec8 {
    ($op:ident r|[hl]; [$code:expr]) => {
        {
            let arg = Reg8::from_b5_3($code);
            cpu_debug!([$code] str(incdec_str!($op)) r_addr:arg);
            $cpu.instr_inc_dec8($control, $tsc, arg, &mut $flags, ops::$op);
            // let opfn = ops::$op;
            // match arg {
            //     Ok(src) => $cpu.apply_reg8(src, None, |v| opfn(v, &mut $flags)),
            //     // hl:3,hl:1,hl(write):3
            //     Err(_) => r_op_w_mem8!(opfn [hl])
            // };
            // flags_op!();
        }
    };
}
//#################################################################################//
//################################ HELPERS END ####################################//
//#################################################################################//
    };
//#################################################################################//
//################################ WITH PREFIX ####################################//
//#################################################################################//
    ($flags:ident, $pc:ident, $cpu:ident, $control:ident, $tsc:ident, $prefix:ident) => {
//#################################################################################//
//################################# MNEMONICS #####################################//
//#################################################################################//
macro_rules! run_mnemonic {
    (     LD q,q | LD (ii+d),r | LD r,(ii+d)  @@@ $code:expr) => {
        {
            match Reg8::tuple_from_b5_3_and_b2_0($code) {
                (Ok(dst @ Reg8::H), Ok(src))|
                (Ok(dst @ Reg8::L), Ok(src))|
                (Ok(dst), Ok(src @ Reg8::H))|
                (Ok(dst), Ok(src @ Reg8::L)) => {
                    cpu_debug!([$code] LD q:dst, q:src);
                    $cpu.load_reg(dst, src, Some($prefix));
                }
                (Ok(dst), Err(_)) => {
                    // pc+2:3, pc+2:1 x 5
                    let d: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X5);
                    cpu_debug!([$code, d] LD r:dst, ii:d);
                    // ii+d:3
                    // MEMPTR = INDEX+d
                    let val = read_mem8_reg16!(<- [$prefix+d] memptr=ii+d);
                    $cpu.set_reg(dst, None, val);
                }
                (Err(_), Ok(src)) => {
                    // pc+2:3, pc+2:1 x 5
                    let d: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X5);
                    cpu_debug!([$code, d] LD ii:d, r:src);
                    // ii+d:3
                    // MEMPTR = INDEX+d
                    write_mem8_reg16!([$prefix+d] <- $cpu.get_reg(src, None); memptr=ii+d);
                }
                _ => debug_unreachable_unchecked!()
            }
        }
    };
    (     LD q,n                    @@@ $code:expr) => {
        {   // pc+1:3
            match Reg8::from_b5_3($code) {
                Ok(dst @ Reg8::H) | Ok(dst @ Reg8::L) => {
                    let n: u8 = fetch_next_imm8!();
                    cpu_debug!([$code, n] LD q:dst, n:n);
                    $cpu.set_reg(dst, Some($prefix), n);
                }
                _ => debug_unreachable_unchecked!()
            }
        }
    };
    (     LD (ii+d),n               @@@ $code:expr) => {
        {   // pc+2:3
            let d: u8 = fetch_next_imm8!();
            // pc+3:3,pc+3:1 x 2
            let n: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X2);
            cpu_debug!([$code, d, n] LD ii:d, n:n);
            // ii+d:3
            // MEMPTR = INDEX+d
            write_mem8_reg16!([$prefix+d] <- n; memptr=ii+d);
        }
    };
    (     LD ii,nn                  @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            let nn: u16 = fetch_next_imm16!();
            cpu_debug!([$code, nn.lsb(), nn.msb()] LD qq:ii, nn:nn);
            $cpu.set_index16($prefix, nn);
        }
    };
    (     LD ii,(nn)                @@@ $code:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr.lsb(), addr.msb()] LD qq:ii, adnn:addr);
            // LD rp,(addr) MEMPTR = addr + 1
            let nn: u16 = read_mem16_addr16!(<- [addr] memptr=addr+1);
            $cpu.set_index16($prefix, nn);
        }
    };
    (     LD (nn),ii                @@@ $code:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr.lsb(), addr.msb()] LD adnn:addr, qq:ii);
            // LD (addr),rp MEMPTR = addr + 1
            write_mem16_addr16!([addr] <- $cpu.get_index2($prefix); memptr=addr+1);
        }
    };
    (     LD SP,ii                  @@@ $code:expr) => {
        { // ir:1 x 2
            // $tsc.add_no_mreq::<{NO_MREQ_X2.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X2);
            cpu_debug!([$code] LD rr:SP, qq:ii);
            $cpu.sp.set16($cpu.get_index16($prefix));
        }
    };
    (     EX (SP),ii                @@@ $code:expr) => {
        { // sp:3, sp+1:3, sp+1:1, sp+1(write):3, sp(write):3, sp(write):1 x 2
            cpu_debug!([$code] EX addr:SP, qq:ii);
            // MEMPTR = rp value after the operation
            let val: u16 = ex_sp_nn!($cpu.get_index2($prefix));
            $cpu.set_index16($prefix, val);
        }
    };
    (     PUSH ii                   @@@ $code:expr) => {
        { // ir:1,sp-1:3,sp-2:3
            // $tsc.add_no_mreq::<{NO_MREQ_X1.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X1);
            cpu_debug!([$code] PUSH qq:ii);
            let (vhi, vlo) = $cpu.get_index2($prefix);
            push2!(vhi, vlo);
        }
    };
    (     POP ii                    @@@ $code:expr) => {
        { // sp:3,sp+1:3
            cpu_debug!([$code] POP qq:ii);
            let val: u16 = pop16!();
            $cpu.set_index16($prefix, val);
        }
    };
    (     INC ii                    @@@ $code:expr) => {
        { // ir:1 x 2
            cpu_debug!([$code] INC qq:ii);
            // $tsc.add_no_mreq::<{NO_MREQ_X2.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X2);
            $cpu.index16_mut($prefix).inc16();
        }
    };
    (     DEC ii                    @@@ $code:expr) => {
        { // ir:1 x 2
            cpu_debug!([$code] DEC qq:ii);
            // $tsc.add_no_mreq::<{NO_MREQ_X2.get()}>($cpu.get_ir());
            $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X2);
            $cpu.index16_mut($prefix).dec16();
        }
    };
    (     @ops A,q | @ops A,(ii+d)  @@@ $code:expr) => {
        {
            let op = Ops8::from($code);
            let val: u8 = match Reg8::from_b2_0($code) {
                Ok(src @ Reg8::H) | Ok(src @ Reg8::L) => {
                    cpu_debug!([$code] op8(op) q:src);
                    $cpu.get_reg(src, Some($prefix))
                }
                Err(_) => {
                    // pc+2:3, pc+2:1 x 5, ii+n:3
                    let d: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X5);
                    cpu_debug!([$code, d] op8(op) ii:d);
                    // ii+d:3
                    // MEMPTR = INDEX+d
                    read_mem8_reg16!(<- [$prefix+d] memptr=ii+d)
                }
                _ => debug_unreachable_unchecked!()
            };
            $cpu.op8(op, val, &mut $flags);
            // flags_op!();
        }
    };
    (     INC q | INC (ii+d)        @@@ $code:expr) => {
        {
            instr_inc_dec8!( inc q|[ii+d]; [$code]);
        }
    };
    (     DEC q | DEC (ii+d)        @@@ $code:expr) => {
        {
            instr_inc_dec8!( dec q|[ii+d]; [$code]);
        }
    };
    (     ADD ii,dd                 @@@ $code:expr) => {
        { // ir:1 x 7
            let src = Reg16::from($code);
            cpu_debug!([$code] ADD qq:ii, qq:src);
            // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
            let nn = $cpu.get_prefix_reg16(src, $prefix);
            match $prefix {
                Prefix::Xdd => op16_reg16!(add16: $cpu.index.ix, nn),
                Prefix::Yfd => op16_reg16!(add16: $cpu.index.iy, nn),
            }
        }
    };
    (     @rot|BIT|RES|SET (ii+d)   @@@ $code0:expr) => {
        { // pc+2:3, pc+3:3, pc+3:1 x 2, ii+n:3, ii+n:1, [ ii+n(write):3 ]
            let d: u8 = fetch_next_imm8!();
            let code1: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X2);
            let bops = BitOps::from(code1);
            cpu_debug!([$code0, d, code1] bitops(bops) ii:d);
            // match bops {
            //     BitOps::Rot(rot, arg) => cpu_debug!([$code0, d, code1] str(rot) ii:d, maybe_r:arg),
            //     BitOps::Bit(b,   _) => cpu_debug!([$code0, d, code1] BIT b:b, ii:d),
            //     BitOps::Res(b, arg) => cpu_debug!([$code0, d, code1] RES b:b, ii:d, maybe_r:arg),
            //     BitOps::Set(b, arg) => cpu_debug!([$code0, d, code1] SET b:b, ii:d, maybe_r:arg),
            // }
            $cpu.bitops_qq($control, $tsc, bops, &mut $flags, $prefix, d);
            // let ii_d = indexed_address!($cpu.get_index16($prefix), d);
            // // Any instruction with (INDEX+d): MEMPTR = INDEX+d
            // $cpu.memptr.set16(ii_d);
            // let val = $control.read_mem(ii_d, $tsc.add_mreq(ii_d));
            // $tsc.add_no_mreq(ii_d, NO_MREQ_X1);
            // loop {
            //     let (result, arg) = match BitOps::from(code1) {
            //         BitOps::Rot(rot, arg) => {
            //             let rot_fn: fn(u8, &mut CpuFlags) -> u8 = rot.into();
            //             cpu_debug!([$code0, d, code1] str(rot) ii:d, maybe_r:arg);
            //             let rotres = rot_fn(val, &mut $flags);
            //             flags_op!();
            //             (rotres, arg)
            //         }
            //         BitOps::Bit(b, _) => {
            //             ops::bit_mp(b, val, (ii_d >> 8) as u8, &mut $flags);
            //             cpu_debug!([$code0, d, code1] BIT b:b, ii:d);
            //             flags_op!();
            //             break;
            //         }
            //         BitOps::Res(b, arg) => {
            //             cpu_debug!([$code0, d, code1] RES b:b, ii:d, maybe_r:arg);
            //             (ops::res(b, val), arg)
            //         }
            //         BitOps::Set(b, arg) => {
            //             cpu_debug!([$code0, d, code1] SET b:b, ii:d, maybe_r:arg);
            //             (ops::set(b, val), arg)
            //         }
            //     };
            //     $control.write_mem(ii_d, result, $tsc.add_mreq(ii_d));
            //     if let Ok(reg) = arg {
            //         $cpu.set_reg(reg, None, result);
            //     }
            //     break;
            // }
        }
    };
    (     JP (ii)                   @@@ $code:expr) => {
        {
            cpu_debug!([$code] JP addr:ii);
            $pc = Wrapping($cpu.get_index16($prefix));
        }
    };
    ($statement:stmt;               @@@ $_:expr) => {
        {
            $statement
        }
    };
}
//#################################################################################//
//#################################### HELPERS ####################################//
//#################################################################################//
// 8-bit INC/DEC
macro_rules! instr_inc_dec8 {
    ($op:ident q|[ii+d]; [$code:expr]) => {
        {
            match Reg8::from_b5_3($code) {
                Ok(src @ Reg8::H) | Ok(src @ Reg8::L) => {
                    cpu_debug!([$code] str(incdec_str!($op)) q:src);
                    $cpu.apply_reg8(src, Some($prefix), |v| ops::$op(v, &mut $flags));
                    flags_op!();
                }
                Err(_) => {
                    // pc+2:3, pc+2:1 x 5
                    let d: u8 = fetch_next_imm8!(no_mreq: NO_MREQ_X5);
                    cpu_debug!([$code, d] str(incdec_str!($op)) ii:d);
                    // ii+n:3, ii+n:1, ii+n(write):3
                    // MEMPTR = INDEX+d
                    $cpu.instr_inc_dec_x_mem8($control, $tsc, $prefix, d, &mut $flags, ops::$op);
                    // let ii_d = indexed_address!($cpu.get_index16($prefix), d);
                    // let val: u8 = ops::$op($control.read_mem(ii_d, $tsc.add_mreq(ii_d)), &mut $flags);
                    // $tsc.add_no_mreq(ii_d, NO_MREQ_X1);
                    // // Any instruction with (INDEX+d): MEMPTR = INDEX+d
                    // $cpu.memptr.set16(ii_d);
                    // $control.write_mem(ii_d, val, $tsc.add_mreq(ii_d));
                    // flags_op!();
                }
                _ => debug_unreachable_unchecked!()
            };
        }
    };
}
//#################################################################################//
//################################ HELPERS END ####################################//
//#################################################################################//
    };
}
