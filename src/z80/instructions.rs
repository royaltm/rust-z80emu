#![macro_use]
//! This module contains implementation of an executor and a debugger (all in one) for all of the Z80 mnemonics.
//!
//! A macro match_instructions in opcodes module dispatches the byte codes into mnemonics.
//!
//! If you look for a particular implementation just search for a mnemonic in capital letters or @ops
//! for arithmetic operations or @rot for bitwise shift and rotation.
//!
//! For some instructions the byte code dispatch is continued here, like recognizing the target, source
//! or the kind of an arithmetic or bitwise operation.
//!
//! The main logic for all of the instructions can be found here, but some repeatable operations, like
//! memory r/w are delegated further to another macros.
//!
//! All flag modifying operations are delegated to the functions in ops module.
macro_rules! define_instructions_scoped {
    ($deb:expr; $prefix:ident, $flags:ident, $pc:ident, $cpu:ident, $control:ident, $tsc:ident) => {
//#################################################################################//
//################################# MNEMONICS #####################################//
//#################################################################################//
macro_rules! run_mnemonic {
    (@extended | break $main:tt             @@@ $code:expr) => {
        {
            // pc+1:4
            debug_assert_eq!($code, 0xEDu8);
            let code = fetch_next_opcode!($cpu, $control, $pc, $tsc);
            match_instruction!(ED [code]; break $main)
        }
    };
    (@prefix = $new:ident                   @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            $prefix = Prefix::$new;
            debug_assert_eq!(Ok($prefix), Prefix::try_from($code));
        }
    };
    (@prefix = $new:ident; break $label:tt  @@@ $code:expr) => {
        {
            debug_assert_ne!($prefix, Prefix::None);
            $prefix = Prefix::$new;
            debug_assert_eq!(Ok($prefix), Prefix::try_from($code));
            break $label;
        }
    };
    (@setnoprefix continue $label:tt        @@@ $_:expr) => {
        { // NOTE: Spectaculator waits 12 ts on DD/FD NONI, a bug perhaps? Fuse tests expect 8 though.
            debug_assert_ne!($prefix, Prefix::None);
            $prefix = Prefix::None;
            continue $label;
        }
    };
    (     NOP *                     @@@ $code0:expr, $code1:expr) => {
        cpu_debug!([$code0, $code1] "NOP*");
    };
    (     DAA                       @@@ $code:expr) => {
        instr_acc_op!(daa; [$code]);
    };
    (     CPL                       @@@ $code:expr) => {
        instr_acc_op!(cpl; [$code]);
    };
    (     NEG                       @@@ $code0:expr, $code1:expr) => {
        instr_acc_op!(neg; [$code0, $code1]);
    };
    (     CCF                       @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] CCF );
            let q = $cpu.af.get8hi() | $flags.bits(); // TODO: Zilog "Q" register
            ops::ccf(q, &mut $flags);
        }
    };
    (     SCF                       @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] SCF );
            let q = $cpu.af.get8hi() | $flags.bits(); // TODO: Zilog "Q" register
            ops::scf(q, &mut $flags);
        }
    };
    (     NOP                       @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] NOP );
        }
    };
    (     DI                        @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] DI );
            $cpu.disable_interrupts();
        }
    };
    (     EI break $main:tt         @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] EI );
            $cpu.enable_interrupts();
            break $main LoopExitReason::EnableInt;
        }
    };
    (     IM 0                      @@@ $code0:expr, $code1:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] IM  n:0);
            $cpu.set_im(InterruptMode::Mode0);
        }
    };
    (     IM 1                      @@@ $code0:expr, $code1:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] IM  n:1);
            $cpu.set_im(InterruptMode::Mode1);
        }
    };
    (     IM 2                      @@@ $code0:expr, $code1:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] IM  n:2);
            $cpu.set_im(InterruptMode::Mode2);
        }
    };
    (     LD r,r | LD (HL),r | LD r,(HL) | HALT break $main:tt  @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            match Reg8::tuple_from_b5_3_and_b2_0($code) {
                (Ok(dst), Ok(src)) => {
                    cpu_debug!([$code] LD r:dst, r:src);
                    $cpu.load_reg(dst, src, Prefix::None);
                }
                (Ok(dst), Err(_)) => {
                    cpu_debug!([$code] LD r:dst, addr:HL);
                    // ss:3
                    $cpu.set_reg(dst, Prefix::None, read_mem8_reg16!(<- [hl]));
                }
                (Err(_), Ok(src)) => {
                    cpu_debug!([$code] LD addr:HL, r:src);
                    // ss:3
                    write_mem8_reg16!([hl] <- $cpu.get_reg(src, Prefix::None));
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
    (     LD q,q | LD (ii+d),r | LD r,(ii+d) | continue $label:tt  @@@ $code:expr) => {
        {
            debug_assert_ne!($prefix, Prefix::None);
            match Reg8::tuple_from_b5_3_and_b2_0($code) {
                (Ok(dst @ Reg8::H), Ok(src))|
                (Ok(dst @ Reg8::L), Ok(src))|
                (Ok(dst), Ok(src @ Reg8::H))|
                (Ok(dst), Ok(src @ Reg8::L)) => {
                    cpu_debug!([$code] LD q:dst, q:src);
                    $cpu.load_reg(dst, src, $prefix);
                }
                (Ok(dst), Err(_)) => {
                    // pc+2:3, pc+2:1 x 5
                    let d: u8 = fetch_next_imm8!(no_mreq: 1x 5);
                    cpu_debug!([$code, d] LD r:dst, ii:d);
                    // ii+d:3
                    // MEMPTR = INDEX+d
                    let val = read_mem8_reg16!(<- [ii+d] memptr=ii+d);
                    $cpu.set_reg(dst, Prefix::None, val);
                }
                (Err(_), Ok(src)) => {
                    // pc+2:3, pc+2:1 x 5
                    let d: u8 = fetch_next_imm8!(no_mreq: 1x 5);
                    cpu_debug!([$code, d] LD ii:d, r:src);
                    // ii+d:3
                    // MEMPTR = INDEX+d
                    write_mem8_reg16!([ii+d] <- $cpu.get_reg(src, Prefix::None); memptr=ii+d);
                }
                _ => {
                    $prefix = Prefix::None;
                    continue $label;
                }
            }
        }
    };
    (     LD r,n | LD (HL),n        @@@ $code:expr) => {
        {   // pc+1:3
            debug_assert_eq!($prefix, Prefix::None);
            let n: u8 = fetch_next_imm8!();
            match Reg8::from_b5_3($code) {
                Ok(dst) => {
                    cpu_debug!([$code, n] LD  r:dst, n:n);
                    $cpu.set_reg(dst, Prefix::None, n);
                }
                Err(_) => { // pc+1:3, hl:3
                    cpu_debug!([$code, n] LD addr:HL, n:n);
                    write_mem8_reg16!([hl] <- n);
                }
            }
        }
    };
    (     LD q,n                    @@@ $code:expr) => {
        {   // pc+1:3
            debug_assert_ne!($prefix, Prefix::None);
            match Reg8::from_b5_3($code) {
                Ok(dst @ Reg8::H) | Ok(dst @ Reg8::L) => {
                    let n: u8 = fetch_next_imm8!();
                    cpu_debug!([$code, n] LD q:dst, n:n);
                    $cpu.set_reg(dst, $prefix, n);
                }
                _ => debug_unreachable_unchecked!()
            }
        }
    };
    (     LD (ii+d),n               @@@ $code:expr) => {
        {   // pc+1:3
            debug_assert_ne!($prefix, Prefix::None);
            // pc+2:3
            let d: u8 = fetch_next_imm8!();
            // pc+3:3,pc+3:1 x 2
            let n: u8 = fetch_next_imm8!(no_mreq: 1x 2);
            cpu_debug!([$code, d, n] LD ii:d, n:n);
            // ii+d:3
            // MEMPTR = INDEX+d
            write_mem8_reg16!([ii+d] <- n; memptr=ii+d);
        }
    };
    (     LD A,(BC)                 @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_rp! { A <- [bc]; [$code] }
        }
    };
    (     LD (BC),A                 @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_rp! { [bc] <- A; [$code] }
        }
    };
    (     LD A,(DE)                 @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_rp! { A <- [de]; [$code] }
        }
    };
    (     LD (DE),A                 @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_rp! { [de] <- A; [$code] }
        }
    };
    (     LD A,(nn)                 @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            debug_assert_eq!($prefix, Prefix::None);
            let nn: u16 = fetch_next_imm16!();
            cpu_debug!([$code, nn as u8, (nn >> 8) as u8] LD r:A, adnn:nn);
            // nn:3
            // LD A, (nn) MEMPTR = addr + 1
            $cpu.memptr.set16(nn.wrapping_add(1));
            let n = $control.read_mem(nn, $tsc.add_mreq(nn));
            $cpu.af.set8hi(n);
        }        
    };
    (     LD (nn),A                 @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            debug_assert_eq!($prefix, Prefix::None);
            let nn: u16 = fetch_next_imm16!();
            cpu_debug!([$code, nn as u8, (nn >> 8) as u8] LD adnn:nn, r:A);
            let a: u8 = $cpu.af.get8hi();
            // nn: 3
            // MEMPTR_low = (nn + 1) & #FF,  MEMPTR_hi = A
            $cpu.memptr.set16(nn.wrapping_add(1) & 0xFF | (a as u16) << 8);
            $control.write_mem(nn, a, $tsc.add_mreq(nn));
        }
    };
    (     LD A,I                    @@@ $code0:expr, $code1:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_ir! { A <- I; [$code0, $code1] }
        }
    };
    (     LD I,A                    @@@ $code0:expr, $code1:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_ir! { I <- A; [$code0, $code1] }
        }
    };
    (     LD A,R                    @@@ $code0:expr, $code1:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_ir! { A <- R; [$code0, $code1] }
        }
    };
    (     LD R,A                    @@@ $code0:expr, $code1:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_ld_ir! { R <- A; [$code0, $code1] }
        }
    };
    (     LD dd,nn                  @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            debug_assert_eq!($prefix, Prefix::None);
            let nn: u16 = fetch_next_imm16!();
            let dst = Reg16::from($code);
            cpu_debug!([$code, nn as u8, (nn >> 8) as u8] LD rr:dst, nn:nn);
            $cpu.set_reg16(dst, nn);
        }
    };
    (     LD ii,nn                  @@@ $code:expr) => {
        { // pc+1:3,pc+2:3
            debug_assert_ne!($prefix, Prefix::None);
            let nn: u16 = fetch_next_imm16!();
            cpu_debug!([$code, nn as u8, (nn >> 8) as u8] LD qq:ii, nn:nn);
            $cpu.set_index16($prefix, nn);
        }
    };
    (     LD ii,(nn)                @@@ $code:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr as u8, (addr >> 8) as u8] LD qq:ii, adnn:addr);
            // LD rp,(addr) MEMPTR = addr + 1
            let nn: u16 = read_mem16_addr16!(<- [addr] memptr=addr+1);
            $cpu.set_index16($prefix, nn);
        }
    };
    (     LD (nn),ii                @@@ $code:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr as u8, (addr >> 8) as u8] LD adnn:addr, qq:ii);
            let (vhi, vlo) = $cpu.get_index2($prefix);
            // LD (addr),rp MEMPTR = addr + 1
            write_mem16_addr16!([addr] <- (vhi, vlo); memptr=addr+1);
        }
    };
    (     LD dd,(nn)                @@@ $code0:expr, $code1:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            debug_assert_eq!($prefix, Prefix::None);
            let addr: u16 = fetch_next_imm16!();
            let reg = Reg16::from($code1);
            cpu_debug!([$code0, $code1, addr as u8, (addr >> 8) as u8] LD rr:reg, adnn:addr);
            // LD rp,(addr) MEMPTR = addr + 1
            let nn: u16 = read_mem16_addr16!(<- [addr] memptr=addr+1);
            $cpu.set_reg16(reg, nn);
        }
    };
    (     LD (nn),dd                @@@ $code0:expr, $code1:expr) => {
        { // pc+1:3,pc+2:3,nn:3,nn+1:3
            debug_assert_eq!($prefix, Prefix::None);
            let addr: u16 = fetch_next_imm16!();
            let reg = Reg16::from($code1);
            cpu_debug!([$code0, $code1, addr as u8, (addr >> 8) as u8] LD adnn:addr, rr:reg);
            let (vhi, vlo) = $cpu.get_reg2(reg);
            // LD (addr),rp MEMPTR = addr + 1
            write_mem16_addr16!([addr] <- (vhi, vlo); memptr=addr+1);
        }
    };
    (     LD SP,ii                  @@@ $code:expr) => {
        { // ir:1 x 2
            $tsc.add_no_mreq($cpu.get_ir(), 2);
            cpu_debug!([$code] LD rr:SP, qq:ii);
            $cpu.sp.set16($cpu.get_index16($prefix));
        }
    };
    (     PUSH ss                   @@@ $code:expr) => {
        { // ir:1, sp-1:3, sp-2:3
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 1);
            let ss = StkReg16::from($code);
            cpu_debug!([$code] PUSH ss:ss);
            let (vhi, vlo) = match ss {
                StkReg16::BC => $cpu.regs.bc.get(),
                StkReg16::DE => $cpu.regs.de.get(),
                StkReg16::HL => $cpu.regs.hl.get(),
                StkReg16::AF => ($cpu.af.get8hi(), $flags.bits())
            };
            push2!(vhi, vlo);
        }
    };
    (     POP ss                    @@@ $code:expr) => {
        { // sp:3,sp+1:3
            debug_assert_eq!($prefix, Prefix::None);
            let ss = StkReg16::from($code);
            cpu_debug!([$code] POP ss:ss);
            let val: u16 = pop16!();
            match ss {
                StkReg16::BC => $cpu.regs.bc.set16(val),
                StkReg16::DE => $cpu.regs.de.set16(val),
                StkReg16::HL => $cpu.regs.hl.set16(val),
                StkReg16::AF => {
                    $flags = CpuFlags::from_bits_truncate(val as u8);
                    $cpu.af.set16(val);
                }
            }
        }
    };
    (     PUSH ii                   @@@ $code:expr) => {
        { // ir:1,sp-1:3,sp-2:3
            debug_assert_ne!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 1);
            cpu_debug!([$code] PUSH qq:ii);
            let (vhi, vlo) = $cpu.get_index2($prefix);
            push2!(vhi, vlo);
        }
    };
    (     POP ii                    @@@ $code:expr) => {
        { // sp:3,sp+1:3
            debug_assert_ne!($prefix, Prefix::None);
            cpu_debug!([$code] POP qq:ii);
            let val: u16 = pop16!();
            $cpu.set_index16($prefix, val);
        }
    };
    (     EX DE,HL                  @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] EX rr:DE, rr:HL);
            $cpu.ex_de_hl();
        }
    };
    (     EX AF,AF^                 @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] EX  ss:AF, ss:AF);
            $flags = $cpu.ex_af_af_with_flags($flags);
        }
    };
    (     EXX                       @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] EXX );
            $cpu.exx();
        }
    };
    (     EX (SP),ii                @@@ $code:expr) => {
        { // sp:3, sp+1:3, sp+1:1, sp+1(write):3, sp(write):3, sp(write):1 x 2
            cpu_debug!([$code] EX addr:SP, qq:ii);
            let (vhi, vlo) = $cpu.get_index2($prefix);
            let val = ex_sp_nn!(vhi, vlo);
            // MEMPTR = rp value after the operation
            $cpu.memptr.set16(val);
            $cpu.set_index16($prefix, val);
        }
    };
    (     LDI                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] LDI );
            $cpu.block_transfer::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Increase, None);
        }
    };
    (     LDIR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2 [de:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] LDIR );
            if let Some(pc) = $cpu.block_transfer::<M, T>($control, &mut $tsc, &mut $flags,
                                                            BlockDelta::Increase, Some($pc)) {
                $pc = pc;
            }
        }
    };
    (     LDD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] LDD );
            $cpu.block_transfer::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Decrease, None);
        }
    };
    (     LDDR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, de:3, de:1 x 2 [de:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] LDDR );
            if let Some(pc) = $cpu.block_transfer::<M, T>($control, &mut $tsc, &mut $flags,
                                                            BlockDelta::Decrease, Some($pc)) {
                $pc = pc;
            }
        }
    };
    (     CPI                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] CPI );
            $cpu.block_search::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Increase, None);
        }
    };
    (     CPIR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5, [hl:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] CPIR );
            if let Some(pc) = $cpu.block_search::<M, T>($control, &mut $tsc, &mut $flags,
                                                        BlockDelta::Increase, Some($pc)) {
                $pc = pc;
            }
        }
    };
    (     CPD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] CPD );
            $cpu.block_search::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Decrease, None);
        }
    };
    (     CPDR                      @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 5, [hl:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] CPDR );
            if let Some(pc) = $cpu.block_search::<M, T>($control, &mut $tsc, &mut $flags,
                                                        BlockDelta::Decrease, Some($pc)) {
                $pc = pc;
            }
        }
    };
    (     @ops A,r | @ops A,(HL)    @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            let arg = Reg8::from_b2_0($code);
            let op = Ops8::from($code);
            let val: u8 = match arg {
                Ok(src) => {
                    $cpu.get_reg(src, Prefix::None)
                }
                // hl:3
                Err(_) => {
                    read_mem8_reg16!(<- [hl])
                }
            };
            cpu_debug!([$code] op8(op) r_addr:arg);
            $cpu.op8(op, val, &mut $flags);
            
        }
    };
    (     @ops A,q | @ops A,(ii+d) | continue $label:tt @@@ $code:expr) => {
        {
            debug_assert_ne!($prefix, Prefix::None);
            let op = Ops8::from($code);
            let val: u8 = match Reg8::from_b2_0($code) {
                Ok(src @ Reg8::H) | Ok(src @ Reg8::L) => {
                    cpu_debug!([$code] op8(op) q:src);
                    $cpu.get_reg(src, $prefix)
                }
                Err(_) => {
                    // pc+2:3, pc+2:1 x 5, ii+n:3
                    let d: u8 = fetch_next_imm8!(no_mreq: 1x 5);
                    cpu_debug!([$code, d] op8(op) ii:d);
                    // ii+d:3
                    // MEMPTR = INDEX+d
                    read_mem8_reg16!(<- [ii+d] memptr=ii+d)
                }
                _ => {
                    $prefix = Prefix::None;
                    continue $label;
                }
            };
            $cpu.op8(op, val, &mut $flags);
        }
    };
    (     @ops A,n                  @@@ $code:expr) => {
        { // pc+1:3
            debug_assert_eq!($prefix, Prefix::None);
            let n: u8 = fetch_next_imm8!();
            let op = Ops8::from($code);
            $cpu.op8(op, n, &mut $flags);
            cpu_debug!([$code, n] op8(op) n:n);
        }
    };
    (     INC r | INC (HL)          @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_inc_dec8!( inc r|[hl]; [$code]);
        }
    };
    (     DEC r | DEC (HL)          @@@ $code:expr) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            instr_inc_dec8!( dec r|[hl]; [$code]);
        }
    };
    (     INC q | INC (ii+d)        @@@ $code:expr) => {
        {
            debug_assert_ne!($prefix, Prefix::None);
            instr_inc_dec8!( inc q|[ii+d]; [$code]);
        }
    };
    (     DEC q | DEC (ii+d)        @@@ $code:expr) => {
        {
            debug_assert_ne!($prefix, Prefix::None);
            instr_inc_dec8!( dec q|[ii+d]; [$code]);
        }
    };
    (     ADD ii,dd                 @@@ $code:expr) => {
        { // ir:1 x 7
            $tsc.add_no_mreq($cpu.get_ir(), 7);
            let src = Reg16::from($code);
            cpu_debug!([$code] ADD qq:ii, qq:src);
            let nn = $cpu.get_reg_prefix16(src, $prefix);
            let memptr = &mut $cpu.memptr;
            match $prefix {
                Prefix::Xdd  => &mut $cpu.index.ix,
                Prefix::Yfd  => &mut $cpu.index.iy,
                Prefix::None => &mut $cpu.regs.hl,
            }.op16(|v| {
                // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
                memptr.set16(v.wrapping_add(1));
                ops::add16(v, nn, &mut $flags)
            });
        }
    };
    (     ADC HL,dd                 @@@ $code0:expr, $code1:expr) => {
        { // ir:1 x 7
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 7);
            let src = Reg16::from($code1);
            cpu_debug!([$code0, $code1] ADC rr:HL, rr:src);
            let nn = $cpu.get_reg16(src);
            let memptr = &mut $cpu.memptr;
            $cpu.regs.hl.op16(|v| {
                // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
                memptr.set16(v.wrapping_add(1));
                ops::adc16(v, nn, &mut $flags)
            });
        }
    };
    (     SBC HL,dd                 @@@ $code0:expr, $code1:expr) => {
        { // ir:1 x 7
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 7);
            let src = Reg16::from($code1);
            cpu_debug!([$code0, $code1] SBC rr:HL, rr:src);
            let nn = $cpu.get_reg16(src);
            let memptr = &mut $cpu.memptr;
            $cpu.regs.hl.op16(|v| {
                // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
                memptr.set16(v.wrapping_add(1));
                ops::sbc16(v, nn, &mut $flags)
            });
        }
    };
    (     INC dd                    @@@ $code:expr) => {
        { // ir:1 x 2
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 2);
            let tgt = Reg16::from($code);
            cpu_debug!([$code] INC rr:tgt);
            $cpu.reg16_mut(tgt).inc16();
        }
    };
    (     DEC dd                    @@@ $code:expr) => {
        { // ir:1 x 2
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 2);
            let tgt = Reg16::from($code);
            cpu_debug!([$code] DEC rr:tgt);
            $cpu.reg16_mut(tgt).dec16();
        }
    };
    (     INC ii                    @@@ $code:expr) => {
        { // ir:1 x 2
            debug_assert_ne!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 2);
            cpu_debug!([$code] INC qq:ii);
            $cpu.index16_mut($prefix).inc16();
        }
    };
    (     DEC ii                    @@@ $code:expr) => {
        { // ir:1 x 2
            debug_assert_ne!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 2);
            cpu_debug!([$code] DEC qq:ii);
            $cpu.index16_mut($prefix).dec16();
        }
    };
    (     RLCA                      @@@ $code:expr) => {
        instr_acc_op!(rlca; [$code]);
    };
    (     RRCA                      @@@ $code:expr) => {
        instr_acc_op!(rrca; [$code]);
    };
    (     RLA                       @@@ $code:expr) => {
        instr_acc_op!(rla; [$code]);
    };
    (     RRA                       @@@ $code:expr) => {
        instr_acc_op!(rra; [$code]);
    };
    (     @rot|BIT|RES|SET r|(HL)   @@@ $code0:expr) => {
        { // pc+1:4
            debug_assert_eq!($prefix, Prefix::None);
            let code1 = fetch_next_opcode!($cpu, $control, $pc, $tsc);
            loop {
                let (setres_fn, b, arg) : (fn (u32, u8) -> u8, _, _) = match BitOps::from(code1) {
                    BitOps::Rot(rot, arg) => {
                        let rot_fn: fn(u8, &mut CpuFlags) -> u8 = rot.into();
                        match arg {
                            Ok(reg) => $cpu.apply_reg8(reg, Prefix::None, |v| rot_fn(v, &mut $flags)),
                            Err(_) => { // hl:3, hl:1, hl(write):3
                                r_op_w_mem8!(rot_fn [hl]);
                            }
                        };
                        cpu_debug!([$code0, code1] str(rot) r_addr:arg);
                        break;
                    }
                    BitOps::Bit(b, arg) => {
                        match arg {
                            Ok(reg) => ops::bit(b, $cpu.get_reg(reg, Prefix::None), &mut $flags),
                            Err(_) => { // hl:3, hl:1
                                let val = read_mem8_reg16!(<- [hl]; no_mreq: 1x 1);
                                ops::bit_mp(b, val, $cpu.memptr.get8hi(), &mut $flags);
                            }
                        };
                        cpu_debug!([$code0, code1] BIT  b:b, r_addr:arg);
                        break;
                    }
                    BitOps::Res(b, arg) => {
                        cpu_debug!([$code0, code1] RES  b:b, r_addr:arg);
                        (ops::res, b, arg)
                    }
                    BitOps::Set(b, arg) => {
                        cpu_debug!([$code0, code1] SET  b:b, r_addr:arg);
                        (ops::set, b, arg)
                    }
                };
                match arg {
                    Ok(reg) => $cpu.apply_reg8(reg, Prefix::None, |v| setres_fn(b, v)),
                    Err(_) => { // hl:3, hl:1, hl(write):3
                        r_op_w_mem8!(setres_fn b, [hl]);
                    }
                };
                break;
            }
        }
    };
    (     @rot|BIT|RES|SET (ii+d)   @@@ $code0:expr) => {
        { // pc+2:3, pc+3:3, pc+3:1 x 2, ii+n:3, ii+n:1, [ ii+n(write):3 ]
            debug_assert_ne!($prefix, Prefix::None);
            let d: u8 = fetch_next_imm8!();
            let code1: u8 = fetch_next_imm8!(no_mreq: 1x 2);
            let ii_d = indexed_address!($cpu.get_index16($prefix), d);
            // Any instruction with (INDEX+d): MEMPTR = INDEX+d
            $cpu.memptr.set16(ii_d);
            let val = $control.read_mem(ii_d, $tsc.add_mreq(ii_d));
            $tsc.add_no_mreq(ii_d, 1);
            loop {
                let (result, op, bit, arg) = match BitOps::from(code1) {
                    BitOps::Rot(rot, arg) => {
                        let rot_fn: fn(u8, &mut CpuFlags) -> u8 = rot.into();
                        (rot_fn(val, &mut $flags), rot.into(), None, arg)
                    }
                    BitOps::Bit(b, _) => {
                        ops::bit_mp(b, val, (ii_d >> 8) as u8, &mut $flags);
                        cpu_debug!([$code0, d, code1] BIT b:b, ii:d);
                        break;
                    }
                    BitOps::Res(b, arg) => {
                        (ops::res(b, val), "RES", Some(b), arg)
                    }
                    BitOps::Set(b, arg) => {
                        (ops::set(b, val), "SET", Some(b), arg)
                    }
                };
                $control.write_mem(ii_d, result, $tsc.add_mreq(ii_d));
                match arg {
                    Ok(reg) => $cpu.set_reg(reg, Prefix::None, result),
                    Err(_) => {}
                }
                cpu_debug!([$code0, d, code1] str(op) maybe_b:bit, ii:d, maybe_r:arg);
                break;
            }
        }
    };
    (     RLD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 4, hl(write):3
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] RLD );
            instr_rxd!(ops::rld);
        }
    };
    (     RRD                       @@@ $code0:expr, $code1:expr) => {
        { // hl:3, hl:1 x 4, hl(write):3
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] RRD );
            instr_rxd!(ops::rrd);
        }
    };
    (     JP nn                     @@@ $code:expr) => {
        { // pc+1:3, pc+2:3
            debug_assert_eq!($prefix, Prefix::None);
            let addr: u16 = fetch_next_imm16!();
            cpu_debug!([$code, addr as u8, (addr >> 8) as u8] JP nn:addr);
            // JP   MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     JP cc,nn                  @@@ $code:expr) => {
        { // pc+1:3, pc+2:3
            debug_assert_eq!($prefix, Prefix::None);
            let addr: u16 = fetch_next_imm16!();
            let cc = Condition::from($code);
            cpu_debug!([$code, addr as u8, (addr >> 8) as u8] JP cc:cc, nn:addr);
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
            debug_assert_eq!($prefix, Prefix::None);
            let e: u8 = fetch_next_imm8!(no_mreq: 1x 5);
            let addr = relative_jump_address!(e);
            cpu_debug!([$code, e] JR nn_0:addr);
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr.0);
            $pc = addr;
        }
    };
    (     JR C,e | JR NC, e | JR Z,e | JR NZ,e  @@@ $code:expr) => {
        { // pc+1:3, [pc+1:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            let cc = Condition::from_jr_subset($code);
            if cc.is_satisfied($flags) {
                let e: u8 = fetch_next_imm8!(no_mreq: 1x 5);
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
    (     JP (ii)                   @@@ $code:expr) => {
        {
            cpu_debug!([$code] JP addr:ii);
            $pc = Wrapping($cpu.get_index16($prefix));
        }
    };
    (     DJNZ e                    @@@ $code:expr) => {
        { // ir:1,pc+1:3,[pc+1:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 1);
            let memptr = &mut $cpu.memptr;
            $cpu.regs.bc.op8hi(|b| {
                let b = b.wrapping_sub(1);
                if b != 0 {
                    let e: u8 = fetch_next_imm8!(no_mreq: 1x 5);
                    let addr = relative_jump_address!(e);
                    cpu_debug!([$code, e] DJNZ nn_0:addr);
                    // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
                    memptr.set16(addr.0);
                    $pc = addr;
                }
                else {
                    $tsc.add_mreq($pc.0);
                    let e = $control.read_debug($pc.0);
                    $pc += Wrapping(1);
                    cpu_debug!([$code, e] DJNZ rel:e);
                }
                b
            });
        }
    };
    (     CALL nn                   @@@ $code:expr) => {
        { // pc+1:3, pc+2:3, pc+2:1, sp-1:3, sp-2:3
            debug_assert_eq!($prefix, Prefix::None);
            let addr: u16 = fetch_next_imm16!(no_mreq: 1x 1);
            cpu_debug!([$code, addr as u8, (addr >> 8) as u8] CALL nn:addr);
            push16!($pc.0);
            // CALL addr MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     CALL cc, nn               @@@ $code:expr) => {
        { // pc+1:3, pc+2:3, [pc+2:1,sp-1:3,sp-2:3]
            debug_assert_eq!($prefix, Prefix::None);
            let addr: u16 = fetch_next_imm16!();
            let cc = Condition::from($code);
            cpu_debug!([$code, addr as u8, (addr >> 8) as u8] CALL cc:cc, nn:addr);
            // CALL addr (even in case of conditional call/jp, independantly on condition satisfied or not)
            // MEMPTR = addr
            $cpu.memptr.set16(addr);
            if cc.is_satisfied($flags) {
                $tsc.add_no_mreq($pc.0.wrapping_sub(1), 1);
                push16!($pc.0);
                $pc = Wrapping(addr);
            }
        }
    };
    (     RET                       @@@ $code:expr) => {
        { // sp:3, sp+1:3
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] RET );
            let addr: u16 = pop16!();
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     RET cc                    @@@ $code:expr) => {
        { // ir:1, [sp:3,sp+1:3]
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 1);
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
            debug_assert_eq!($prefix, Prefix::None);
            $cpu.restore_iff1(); // Both RETI and RETN do that.
            let should_break = if $code1 == opconsts::RETI_OPCODE_T2.1 {
                cpu_debug!([$code0, $code1] RETI );
                $control.reti($pc.0, $tsc.as_timestamp())
            }
            else {
                cpu_debug!([$code0, $code1] RETN );
                false
            };
            let addr: u16 = pop16!();
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
            if should_break {
                break $main LoopExitReason::RetInt;
            }
        }
    };
    (     RST p                     @@@ $code:expr) => {
        { // ir:1, sp-1:3, sp-2:3
            debug_assert_eq!($prefix, Prefix::None);
            $tsc.add_no_mreq($cpu.get_ir(), 1);
            let addr = parse_restart_address($code);
            cpu_debug!([$code] RST adnn:addr);
            push16!($pc.0);
            // JR/DJNZ/RET/RETI/RST (jumping to addr) MEMPTR = addr
            $cpu.memptr.set16(addr);
            $pc = Wrapping(addr);
        }
    };
    (     IN A,(n)                  @@@ $code:expr) => {
        { // pc+1:3, IO
            debug_assert_eq!($prefix, Prefix::None);
            let n: u8 = fetch_next_imm8!();
            cpu_debug!([$code, n] IN r:A, port:n);
            let port: u16 = ($cpu.af.get8hi() as u16) << 8 | n as u16;
            // MEMPTR = (A_before_operation << 8) + port + 1
            $cpu.memptr.set16(port.wrapping_add(1));
            $cpu.af.set8hi($control.read_io(port, $tsc.add_io(port)));
        }
    };
    (     IN r,(C)                  @@@ $code0:expr, $code1:expr) => {
        { // IO
            debug_assert_eq!($prefix, Prefix::None);
            let bc = $cpu.regs.bc.get16();
            let val = $control.read_io(bc, $tsc.add_io(bc));
            ops::io(val, &mut $flags);
            // IN r,(C)     MEMPTR = BC + 1
            $cpu.memptr.set16(bc.wrapping_add(1));
            match Reg8::from_b5_3($code1) {
                Ok(dst) => { // IN r,(C)
                    cpu_debug!([$code0, $code1] IN r:dst, port:C);
                    $cpu.set_reg(dst, Prefix::None, val);
                }
                Err(_) => { // IN F, (c)
                    cpu_debug!([$code0, $code1] IN port:C);
                }
            }
        }
    };
    (     INI                       @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] INI );
            $cpu.block_in::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Increase, None);
        }
    };
    (     INIR                      @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3, [hl:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] INIR );
            if let Some(pc) = $cpu.block_in::<M, T>($control, &mut $tsc, &mut $flags,
                                                        BlockDelta::Increase, Some($pc)) {
                $pc = pc;
            }

        }
    };
    (     IND                       @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] IND );
            $cpu.block_in::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Decrease, None);
        }
    };
    (     INDR                      @@@ $code0:expr, $code1:expr) => {
        { // ir:1, IO, hl:3, [hl:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] INDR );
            if let Some(pc) = $cpu.block_in::<M, T>($control, &mut $tsc, &mut $flags,
                                                        BlockDelta::Decrease, Some($pc)) {
                $pc = pc;
            }
        }
    };
    (     OUT (n),A | break $main:tt  @@@ $code:expr) => {
        { // pc+1:3, IO
            debug_assert_eq!($prefix, Prefix::None);
            let n: u8 = fetch_next_imm8!();
            cpu_debug!([$code, n] OUT port:n, r:A);
            let a = $cpu.af.get8hi();
            let addr: u16 = (a as u16) << 8;
            $cpu.memptr.set16(addr | n.wrapping_add(1) as u16); // MEMPTR_low = (port + 1) & #FF,  MEMPTR_hi = A
            let port = addr|n as u16;
            if $control.write_io(port, a, $tsc.add_io(port)) {
                break $main LoopExitReason::WriteIo;
            }
        }
    };
    (     OUT (C),r | break $main:tt  @@@ $code0:expr, $code1:expr) => {
        { // IO
            debug_assert_eq!($prefix, Prefix::None);
            let bc = $cpu.regs.bc.get16();
            // OUT (C),r    MEMPTR = BC + 1
            $cpu.memptr.set16(bc.wrapping_add(1));
            let val = match Reg8::from_b5_3($code1) {
                Ok(src) => {
                    cpu_debug!([$code0, $code1] OUT port:C, r:src);
                    $cpu.get_reg(src, Prefix::None)
                }
                Err(_) => {// TODO: CMOS does OUT (C), 255
                    cpu_debug!([$code0, $code1] OUT port:C);
                    0
                }
            };
            if $control.write_io(bc, val, $tsc.add_io(bc)) {
                break $main LoopExitReason::WriteIo;
            }
        }
    };
    (     OUTI | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] OUTI );
            let (should_break, _) = $cpu.block_out::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Increase, None);
            if should_break {
                break $main LoopExitReason::WriteIo;
            }
        }
    };
    (     OTIR | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO, [bc:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] OTIR );
            let (should_break, maybe_pc) = $cpu.block_out::<M, T>($control, &mut $tsc, &mut $flags,
                                                                    BlockDelta::Increase, Some($pc));
            if let Some(pc) = maybe_pc { $pc = pc; }
            if should_break { break $main LoopExitReason::WriteIo; }
        }
    };
    (     OUTD | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] OUTD );
            let (should_break, _) = $cpu.block_out::<M, T>($control, &mut $tsc, &mut $flags, BlockDelta::Decrease, None);
            if should_break {
                break $main LoopExitReason::WriteIo;
            }
        }
    };
    (     OTDR | break $main:tt     @@@ $code0:expr, $code1:expr) => {
        { // ir:1, hl:3, IO, [bc:1 x 5]
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] OTDR );
            let (should_break, maybe_pc) = $cpu.block_out::<M, T>($control, &mut $tsc, &mut $flags,
                                                                    BlockDelta::Decrease, Some($pc));
            if let Some(pc) = maybe_pc { $pc = pc; }
            if should_break { break $main LoopExitReason::WriteIo; }
        }
    };
//#################################################################################//
//################################# MNEMONICS END #################################//
//#################################################################################//
}
//#################################################################################//
//#################################### HELPERS ####################################//
//#################################################################################//

macro_rules! instr_ld_rp {
    // LD A, (rp); rp: BC or DE
    (A <- [$reg16:ident]; [$code:expr]) => {
        {
            cpu_debug!([$code] LD r:A, addr:$reg16);
            // ss:3
            // MEMPTR = rp + 1
            let n: u8 = read_mem8_reg16!(<- [$reg16] memptr=$reg16.wrapping_add(1));
            $cpu.af.set8hi(n);
        }
    };
    // LD (rp), A; rp: BC or DE
    ([$reg16:ident] <- A; [$code:expr]) => {
        {
            cpu_debug!([$code] LD addr:$reg16, r:A);
            let a: u8 = $cpu.af.get8hi();
            // ss: 3
            // MEMPTR_low = (rp + 1) & #FF,  MEMPTR_hi = A
            write_mem8_reg16!([$reg16] <- a; memptr=(a, $reg16.wrapping_add(1) as u8));
        }
    };
}

macro_rules! instr_ld_ir {
    // LD A,I; LD A,R
    (A <- $reg:tt; [$code0:expr, $code1:expr]) => {
        {
            cpu_debug!([$code0, $code1] LD r:A, r:$reg);
            let ir = $cpu.get_ir();
            // ir:1
            $tsc.add_no_mreq(ir, 1);
            let val = instr_ld_ir!(@extract $reg <- ir);
            let iff2 = if $cpu.iff2 {
                !$control.is_irq($tsc.as_timestamp()) // TODO: flavours
            }
            else {
                false
            };
            ops::ld_a_ir(val, iff2, &mut $flags);
            $cpu.af.set8hi(val);
        }
    };
    (@extract I <- $ir:ident) => { ($ir >> 8) as u8 };
    (@extract R <- $ir:ident) => { $ir as u8 };
    // LD I,A; LD R,A
    ($reg:tt <- A; [$code0:expr, $code1:expr]) => {
        {
            cpu_debug!([$code0, $code1] LD r:$reg, r:A);
            // ir:1
            $tsc.add_no_mreq($cpu.get_ir(), 1);
            let a = $cpu.af.get8hi();
            instr_ld_ir!(@store $reg <- a);
        }
    };
    (@store I <- $a:ident) => { $cpu.ir.set8hi($a); };
    (@store R <- $a:ident) => { $cpu.set_r($a); };
}

macro_rules! acc_op_str {
    (daa)  => { "DAA" };
    (cpl)  => { "CPL" };
    (neg)  => { "NEG" };
    (rlca) => { "RLCA" };
    (rrca) => { "RRCA" };
    (rla)  => { "RLA" };
    (rra)  => { "RRA" };
}

// Unary accumulator operators
macro_rules! instr_acc_op {
    ($op:ident; [$code:expr]) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code] str(acc_op_str!($op)));
            $cpu.af.op8hi(|a| ops::$op(a, &mut $flags));
        }
    };
    ($op:ident; [$code0:expr, $code1:expr]) => {
        {
            debug_assert_eq!($prefix, Prefix::None);
            cpu_debug!([$code0, $code1] str(acc_op_str!($op)));
            $cpu.af.op8hi(|a| ops::$op(a, &mut $flags));
        }
    };
}

macro_rules! incdec_str {
    (inc) => { "INC" };
    (dec) => { "DEC" };
}

// 8-bit INC/DEC
macro_rules! instr_inc_dec8 {
    ($op:ident r|[hl]; [$code:expr]) => {
        {
            let arg = Reg8::from_b5_3($code);
            cpu_debug!([$code] str(incdec_str!($op)) r_addr:arg);
            let opfn = ops::$op;
            match arg {
                Ok(src) => $cpu.apply_reg8(src, Prefix::None, |v| opfn(v, &mut $flags)),
                // hl:3,hl:1,hl(write):3
                Err(_) => r_op_w_mem8!(opfn [hl])
            };
        }
    };
    ($op:ident q|[ii+d]; [$code:expr]) => {
        {
            let arg = Reg8::from_b5_3($code);
            let opfn = ops::$op;
            match arg {
                Ok(src @ Reg8::H) | Ok(src @ Reg8::L) => {
                    cpu_debug!([$code] str(incdec_str!($op)) q:src);
                    $cpu.apply_reg8(src, $prefix, |v| opfn(v, &mut $flags));
                }
                Err(_) => {
                    // pc+2:3, pc+2:1 x 5
                    let d: u8 = fetch_next_imm8!(no_mreq: 1x 5);
                    cpu_debug!([$code, d] str(incdec_str!($op)) ii:d);
                    // ii+n:3, ii+n:1, ii+n(write):3
                    // MEMPTR = INDEX+d
                    r_op_w_mem8!(opfn [ii+d] memptr=ii+d);
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
