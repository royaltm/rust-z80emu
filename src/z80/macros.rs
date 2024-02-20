/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
//! This module contains various macros used by instructions module.
#![macro_use]
// macro_rules! replace_expr {
//     ($_t:tt $sub:expr) => {$sub};
// }

// macro_rules! count_exprs {
//     ($($x:expr),*) => {0usize $(+ replace_expr!($x 1usize))*};
// }

/// Reads 1 byte from memory via PC register ($pc). Increases $pc afterwards.
/// Used for the M1 cycle (op-code fetch) so a proper number of M1 cycles is being added.
/// Increases the memory refresh (R) counter.
macro_rules! fetch_next_opcode_ext {
    ($cpu:ident, $control:ident, $pc:ident, $tsc:ident) => {
        { // pc:4, pc+=1
            let (pc_1, code) = $cpu.fetch_next_opcode($control, $tsc, $pc);
            $pc = pc_1;
            code
            // $cpu.inc_r();
            // let code = $control.read_opcode($pc.0, $cpu.get_ir(), $tsc.add_m1($pc.0));
            // $pc += Wrapping(1);
            // code
        }
    };
}

/// Calculates a wrapping 16-bit $ii + $d where $ii is a 16 bit unsigned address and
/// $d can be a signed or unsigned integer representing a twos compliment 8-bit offset.
macro_rules! indexed_address {
    ($ii:expr, $d:expr) => {
        $ii.wrapping_add($d as i8 as i16 as u16)
    };
}

macro_rules! define_cpu_debug_scoped {
    ([$dol:tt] $deb:expr; $maybe_prefix:ident, $pc:ident) => {
        /// A sugar for creating CpuDebug and calling a debugger function if it was given as $deb.
        macro_rules! cpu_debug {
            (@deb $debugger:ident, [$dol($code:expr),+] str($mnemonic:expr) $dol($id:ident:$e:tt),*) => {
                CpuDebug::debug_instruction(
                    $mnemonic.into(),
                    cpu_debug!(@args $dol($id:$e),*),
                    $maybe_prefix,
                    &[$dol($code),+],
                    $pc,
                    $debugger)
            };
            (@deb $debugger:ident, [$dol($code:expr),+] $mnemonic:ident $dol($id:ident:$e:tt),*) => {
                cpu_debug!(@deb $debugger, [$dol($code),+] str(stringify!($mnemonic)) $dol($id:$e),*)
            };
            ([$dol($code:expr),+] str($mnemonic:expr) $dol($id:ident:$e:tt),*) => {
                if let Some(debugger) = $deb {
                    cpu_debug!(@deb debugger, [$dol($code),+] str($mnemonic) $dol($id:$e),*)
                }
            };
            ([$dol($code:expr),+] $mnemonic:literal $dol($id:ident:$e:tt),*) => {
                cpu_debug!([$dol($code),+] str($mnemonic) $dol($id:$e),*)
            };
            ([$dol($code:expr),+] $mnemonic:ident $dol($id:ident:$e:tt),*) => {
                cpu_debug!([$dol($code),+] str(stringify!($mnemonic)) $dol($id:$e),*)
            };
            ([$dol($code:expr),+] op8($op:expr) $id:ident:$e:tt) => {
                if let Some(debugger) = $deb {
                    match $op {
                        Ops8::ADD|
                        Ops8::ADC|
                        Ops8::SBC => cpu_debug!(@deb debugger, [$dol($code),+] str($op) r:A, $id:$e),
                        _ => cpu_debug!(@deb debugger, [$dol($code),+] str($op) $id:$e)
                    }
                }
            };
            ([$dol($code:expr),+] bitops($bops:expr)) => {
                if let Some(debugger) = $deb {
                    match $bops {
                        BitOps::Rot(rot, arg) => cpu_debug!(@deb debugger, [$dol($code),+] str(rot) r_addr:arg),
                        BitOps::Bit(b, arg) => cpu_debug!(@deb debugger, [$dol($code),+] BIT  b:b, r_addr:arg),
                        BitOps::Res(b, arg) => cpu_debug!(@deb debugger, [$dol($code),+] RES  b:b, r_addr:arg),
                        BitOps::Set(b, arg) => cpu_debug!(@deb debugger, [$dol($code),+] SET  b:b, r_addr:arg)
                    }
                }
            };
            ([$dol($code:expr),+] bitops($bops:expr) ii:$d:expr) => {
                if let Some(debugger) = $deb {
                    match $bops {
                        BitOps::Rot(rot, arg) => cpu_debug!(@deb debugger, [$dol($code),+] str(rot) ii:$d, maybe_r:arg),
                        BitOps::Bit(b,   _) => cpu_debug!(@deb debugger, [$dol($code),+] BIT  b:b, ii:$d),
                        BitOps::Res(b, arg) => cpu_debug!(@deb debugger, [$dol($code),+] RES  b:b, ii:$d, maybe_r:arg),
                        BitOps::Set(b, arg) => cpu_debug!(@deb debugger, [$dol($code),+] SET  b:b, ii:$d, maybe_r:arg)
                    }
                }
            };
            (@args) => { CpuDebugArgs::None };
            (@args ii:$d:expr, maybe_r:$res:expr) => {
                match $res {
                    Ok(reg) => CpuDebugArgs::Double(cpu_debug!(@arg ii:$d), cpu_debug!(@arg r:reg)),
                    Err(_)  => CpuDebugArgs::Single(cpu_debug!(@arg ii:$d))
                }
            };
            (@args b:$b:expr, ii:$d:expr, maybe_r:$res:expr) => {
                match $res {
                    Ok(reg) => CpuDebugArgs::BitOpExt($b, cpu_debug!(@arg ii:$d), reg),
                    Err(_)  => CpuDebugArgs::Double(cpu_debug!(@arg b:$b), cpu_debug!(@arg ii:$d)),
                }
            };
            (@args maybe_r:$reg:expr, $id:ident:$e:tt) => {
                match $reg {
                    Ok(reg) => CpuDebugArgs::Double(cpu_debug!(@arg r:reg), cpu_debug!(@arg $id:$e)),
                    Err(_) => CpuDebugArgs::Single(cpu_debug!(@arg $id:$e))
                }
            };
            (@args $id:ident:$e:tt, maybe_r:$reg:expr) => {
                match $reg {
                    Ok(reg) => CpuDebugArgs::Double(cpu_debug!(@arg $id:$e), cpu_debug!(@arg r:reg)),
                    Err(_) => CpuDebugArgs::Single(cpu_debug!(@arg $id:$e))
                }
            };
            (@args $id1:ident:$e1:tt) => { CpuDebugArgs::Single(cpu_debug!(@arg $id1:$e1)) };
            (@args $id1:ident:$e1:tt, $id2:ident:$e2:tt) => {
                CpuDebugArgs::Double(cpu_debug!(@arg $id1:$e1), cpu_debug!(@arg $id2:$e2))
            };
            (@arg n:$n:expr)      => { CpuDebugArg::Imm8($n) };
            (@arg b:$b:expr)      => { CpuDebugArg::Bit($b) };
            (@arg m:$m:tt)        => { CpuDebugArg::IntMode(interrupt_mode!($m)) };
            (@arg nn:$nn:expr)    => { CpuDebugArg::Imm16($nn) };
            (@arg p:$nn:expr)     => { CpuDebugArg::Imm8($nn as u8) };
            (@arg nn_0:$nn:expr)  => { CpuDebugArg::Imm16($nn.0) };
            (@arg r:A)            => { CpuDebugArg::Reg8(None, Reg8::A) };
            (@arg r:I)            => { CpuDebugArg::I };
            (@arg r:R)            => { CpuDebugArg::R };
            (@arg r:$reg:expr)    => { CpuDebugArg::Reg8(None, $reg) };
            (@arg q:$reg:expr)    => { CpuDebugArg::Reg8($maybe_prefix, $reg) };
            (@arg rr:HL)          => { CpuDebugArg::Reg16(None, Reg16::HL) };
            (@arg rr:DE)          => { CpuDebugArg::Reg16(None, Reg16::DE) };
            (@arg rr:SP)          => { CpuDebugArg::Reg16(None, Reg16::SP) };
            (@arg rr:$reg:expr)   => { CpuDebugArg::Reg16(None, $reg) };
            (@arg qq:ii)          => { CpuDebugArg::Reg16($maybe_prefix, Reg16::HL) };
            (@arg qq:$reg:expr)   => { CpuDebugArg::Reg16($maybe_prefix, $reg) };
            (@arg ss:AF)          => { CpuDebugArg::Stk16(StkReg16::AF) };
            (@arg ss:$reg:expr)   => { CpuDebugArg::Stk16($reg) };
            (@arg addr:HL)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::HL)) };
            (@arg addr:bc)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::BC)) };
            (@arg addr:de)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::DE)) };
            (@arg addr:SP)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::SP)) };
            (@arg addr:ii)        => { 
                match $maybe_prefix {
                    Some(prefix) => CpuDebugArg::Addr(CpuDebugAddr::IndexAddr(prefix, None)),
                    None         => panic!("addr:ii requires a prefix")
                }
            };
            (@arg addr:$reg:expr) => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr($reg)) };
            (@arg ii:$d:expr)     => { CpuDebugArg::Addr(CpuDebugAddr::IndexAddr($maybe_prefix.unwrap(), Some($d as i8))) };
            (@arg adnn:$nn:expr)  => { CpuDebugArg::Addr(CpuDebugAddr::ImmAddr($nn)) };
            (@arg port:C)         => { CpuDebugArg::Port(CpuDebugPort::RegPort) };
            (@arg port:$n:expr)   => { CpuDebugArg::Port(CpuDebugPort::ImmPort($n)) };
            (@arg cc:$cond:expr)  => { CpuDebugArg::Cond($cond) };
            (@arg rel:$e:expr)    => { CpuDebugArg::Imm16(relative_jump_address!($e).0) };
            (@arg r_addr:$res:expr) => {
                match $res {
                    Ok(reg) => CpuDebugArg::Reg8(None, reg),
                    Err(_) => CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::HL))
                }
            };
        }
    };
}

/// Defines scoped macros used by opcodes::execute_instruction!.
macro_rules! define_helpers_scoped {
    ([$dol:tt] $flags:ident, $pc:ident, $cpu:ident, $control:ident, $tsc:ident) => {

        macro_rules! fetch_next_opcode {
            () => { fetch_next_opcode_ext!($cpu, $control, $pc, $tsc) };
        }
        /// Reads 1 byte from memory via PC register ($pc). Increases $pc afterwards.
        /// Used only to read the immediate 8-bit instruction argument.
        /// For reading an op-code see fetch_next_opcode!.
        macro_rules! fetch_next_imm8 {
            (@x $add_ts_more:expr) => {
                { // pc:3, pc+=1
                    let res = $cpu.fetch_next_imm8::<_, _, {$add_ts_more}>($control, $tsc, $pc);
                    $pc = res.0;
                    res.1
                    // let code = $control.read_mem($pc.0, $tsc.add_mreq($pc.0));
                    // $dol( // pc:1 x m
                    //     $tsc.add_no_mreq($pc.0, $add_ts_more);
                    // )?
                    // $pc += Wrapping(1);
                    // code
                }
            };
            () => {
                fetch_next_imm8!(@x 0)
            };
            (no_mreq: $add_ts_more:expr) => {
                fetch_next_imm8!(@x $add_ts_more.get())
            };
        }

        /// Reads 2 bytes from memory via PC register ($pc) as a LE u16. Increases $pc afterwards.
        /// Used to read the immediate 16-bit instruction argument.
        macro_rules! fetch_next_imm16 {
            (@x $add_ts_more:expr) => {
                { // pc:3, pc+1:3, pc+=2
                    let res = $cpu.fetch_next_imm16::<_, _, {$add_ts_more}>($control, $tsc, $pc);
                    $pc = res.0;
                    res.1
                    // let nn: u16 = $control.read_mem16($pc.0, $tsc.add_mreq($pc.0));
                    // let pc1 = $pc.0.wrapping_add(1);
                    // $tsc.add_mreq(pc1);
                    // $dol( // pc:1 x m
                    //     $tsc.add_no_mreq(pc1, $add_ts_more);
                    // )?
                    // $pc += Wrapping(2);
                    // nn
                }
            };
            () => {
                fetch_next_imm16!(@x 0)
            };
            (no_mreq: $add_ts_more:expr) => {
                fetch_next_imm16!(@x $add_ts_more.get())
            };
        }

        /// Used by various 8-bit operations on the memory via one of the address registers: HL, IX or IY.
        // macro_rules! r_op_w_mem8 {
        //     // (@internal [$reg16:ident] $oper:expr) => {
        //     //     { // hl:3, hl:1, hl(write):3
        //     //         let $reg16: u16 = $cpu.regs.$reg16.get16();
        //     //         let val: u8 = $oper;
        //     //         $tsc.add_no_mreq($reg16, NO_MREQ_X1);
        //     //         $control.write_mem($reg16, val, $tsc.add_mreq($reg16));
        //     //     }
        //     // };
        //     ($opfn:ident [hl]) => {
        //         // hl:3, hl:1, hl(write):3
        //         $cpu.r_op_w_mem8($control, $tsc, |v| $opfn(v, &mut $flags))
        //         // r_op_w_mem8! { @internal [hl]
        //         //     $opfn($control.read_mem(hl, $tsc.add_mreq(hl)), &mut $flags)
        //         // }
        //     };
        //     ($opfn:ident $b:expr, [hl]) => {
        //         // hl:3, hl:1, hl(write):3
        //         $cpu.r_op_w_mem8($control, $tsc, |v| $opfn($b, v))
        //         // r_op_w_mem8! { @internal [hl]
        //         //     $opfn($b, $control.read_mem(hl, $tsc.add_mreq(hl)))
        //         // }
        //     };
        // }

        /// Reads 1 byte from memory via one of the 16-bit registers: BC, DE, HL, IX or IY.
        macro_rules! read_mem8_reg16 {
            (<- [hl] @x $add_ts_more:expr) => { // ss: 3
                $cpu.read_mem8_hl::<_, _, {$add_ts_more}>($control, $tsc)
                // { // ss: 3
                //     let hl: u16 = $cpu.regs.hl.get16();
                //     // $dol(
                //     //     $cpu.memptr.set16($memptr);
                //     // )?
                //     let val = $control.read_mem(hl, $tsc.add_mreq(hl));
                //     $dol(
                //         $tsc.add_no_mreq(hl, $add_ts_more);
                //     )?
                //     val
                // }
            };
            (<- [hl]) => {
                read_mem8_reg16!(<- [hl] @x 0)
            };
            (<- [hl] ;no_mreq: $add_ts_more:expr) => {
                read_mem8_reg16!(<- [hl] @x $add_ts_more.get())
            };
            (<- [$prefix:ident+$index8:ident] memptr=ii+d) => {
                $cpu.read_mem8_ii_d($control, $tsc, $prefix, $index8)
                // { // ii+d: 3
                    // let ii_d = indexed_address!($cpu.get_index16($prefix), $index8);
                    // // Any instruction with (INDEX+d): MEMPTR = INDEX+d
                    // $cpu.memptr.set16(ii_d);
                    // $control.read_mem(ii_d, $tsc.add_mreq(ii_d))
                // }
            };
        }

        /// Writes 1 byte into memory via one of the address registers: HL, IX or IY.
        /// Returns the u16 content of the address register for optional memptr updating.
        macro_rules! write_mem8_reg16 {
            ([hl] <- $val:expr) => { // ss: 3
                $cpu.write_mem8_hl($control, $tsc, $val)
                // { 
                    // let hl = $cpu.regs.hl.get16();
                    // // $dol(
                    // //     let (hi, lo): (u8, u8) = $memptr;
                    // //     $cpu.memptr.set(hi, lo);
                    // // )?
                    // $control.write_mem(hl, $val, $tsc.add_mreq(hl));
                // }
            };
            ([$prefix:ident+$index8:ident] <- $val:expr; memptr=ii+d) => {
                // ii+d: 3
                $cpu.write_mem8_ii_d($control, $tsc, $prefix, $index8, $val)
                // { // ii+d: 3
                //     let ii_d = indexed_address!($cpu.get_index16($prefix), $index8);
                //     // Any instruction with (INDEX+d): MEMPTR = INDEX+d
                //     $cpu.memptr.set16(ii_d);
                //     $control.write_mem(ii_d, $val, $tsc.add_mreq(ii_d));
                // }
            };
        }

        /// Reads 2 bytes from memory at immediate address $addr and $addr + 1 as an LE u16 word.
        macro_rules! read_mem16_addr16 {
            (<- [$addr:expr] memptr=addr+1) => {
                // nn:3, nn+1:3
                $cpu.read_mem16_addr16($control, $tsc, $addr)
                // { // nn:3, nn+1:3
                //     // LD rp,(addr) MEMPTR = addr + 1
                //     let val = $control.read_mem16($addr, $tsc.add_mreq($addr));
                //     let addr1 = $addr.wrapping_add(1);
                //     $cpu.memptr.set16(addr1);
                //     $tsc.add_mreq(addr1);
                //     val
                // }
            };
        }

        /// Writes 2 bytes into memory at immediate address $addr ($vlo) and $addr + 1 ($vhi).
        macro_rules! write_mem16_addr16 {
            ([$addr:expr] <- $val2:expr; memptr=addr+1) => {
                // nn:3, nn+1:3
                $cpu.write_mem16_addr16($control, $tsc, $addr, $val2)
                // { // nn:3, nn+1:3
                //     let (vhi, vlo): (u8, u8) = $val2;
                //     $control.write_mem($addr, vlo, $tsc.add_mreq($addr));
                //     let addr_1 = $addr.wrapping_add(1);
                //     // LD (addr), rp; MEMPTR = addr + 1
                //     $cpu.memptr.set16(addr_1);
                //     $control.write_mem(addr_1, vhi, $tsc.add_mreq(addr_1));
                // }
            };
        }

        /// Writes 2 bytes into memory at SP - 1 ($vhi) and SP - 2 ($vlo). Decreases SP by 2 afterwards.
        macro_rules! push2 {
            ($vhi:expr, $vlo:expr) => {
                $cpu.push2($vhi, $vlo, $control, $tsc)
            };
        }

        /// Writes a 16-bit LE integer into memory at SP - 1 and SP - 2. Decreases SP by 2 afterwards.
        macro_rules! push16 {
            ($val:expr) => {
                $cpu.push16($val, $control, $tsc)
            };
        }

        /// Reads 2 bytes from memory at SP and SP + 1 as an LE u16 integer. Increases SP by 2 afterwards.
        macro_rules! pop16 {
            () => {
                $cpu.pop16($control, $tsc)
            };
        }

        /// Used by EX (SP), ii
        macro_rules! ex_sp_nn {
            ($val2:expr) => {
                // sp:3,sp+1:3,sp+1:1,sp+1(write):3,sp(write):3,sp(write):1 x 2
                $cpu.ex_sp_nn($control, $tsc, $val2)
                // {
                //     let sp = $cpu.sp.get16();
                //     let val = $control.read_mem16(sp, $tsc.add_mreq(sp));
                //     let sp_1 = sp.wrapping_add(1);
                //     $tsc.add_mreq(sp_1);
                //     $tsc.add_no_mreq(sp_1, NO_MREQ_X1);
                //     let (vhi, vlo): (u8, u8) = $val2;
                //     $control.write_mem(sp_1, vhi, $tsc.add_mreq(sp_1));
                //     $control.write_mem(sp, vlo, $tsc.add_mreq(sp));
                //     $tsc.add_no_mreq(sp, NO_MREQ_X2);
                //     // MEMPTR = rp value after the operation
                //     $cpu.memptr.set16(val);
                //     val
                // }
            };
        }

        /// ADD|ADC|SBC ii,dd
        macro_rules! op16_reg16 {
            ($op16:ident: $reg16:expr, $nn:expr) => {
                { // ir:1 x 7
                    $reg16 = $cpu.op16_reg16($tsc, &mut $flags, ops::$op16, $reg16, $nn);
                    // $tsc.add_no_mreq($cpu.get_ir(), NO_MREQ_X7);
                    // let nn: u16 = $nn;
                    // let memptr = &mut $cpu.memptr;
                    // $reg16.op16(|v| {
                    //     // ADD/ADC/SBC rp1,rp2 MEMPTR = rp1_before_operation + 1
                    //     memptr.set16(v.wrapping_add(1));
                    //     ops::$op16(v, nn, &mut $flags)
                    // });
                    // flags_op!();
                }
            };
        }

        // RLD | RRD
        macro_rules! instr_rxd {
            ($rxd:path) => {
                {
                    // hl:3, hl:1 x 4, hl(write):3
                    $cpu.instr_rxd($control, $tsc, &mut $flags, $rxd);
                    // let hl: u16 = $cpu.regs.hl.get16();
                    // // RLD/RRD  MEMPTR = HL + 1
                    // $cpu.memptr.set16(hl.wrapping_add(1));
                    // let val = $control.read_mem(hl, $tsc.add_mreq(hl));
                    // $tsc.add_no_mreq(hl, NO_MREQ_X4);
                    // $cpu.af.op8hi(|a| {
                    //     let (a, res) : (u8, u8) = $rxd(a, val, &mut $flags);
                    //     $control.write_mem(hl, res, $tsc.add_mreq(hl));
                    //     a
                    // });
                    // flags_op!();
                }
            };
        }

        macro_rules! flags_op {
            () => {
                $cpu.flavour.flags_modified();
            }
        }

        /// Calculates a relative branch address from $pc as a Wrapping 16 bit integer
        /// and 8-bit twos compliment offset in $e.
        macro_rules! relative_jump_address {
            ($e:expr) => {
                $pc + Wrapping($e as i8 as i16 as u16)
            };
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

        macro_rules! incdec_str {
            (inc) => { "INC" };
            (dec) => { "DEC" };
        }

        macro_rules! interrupt_mode {
            (0) => { InterruptMode::Mode0 };
            (1) => { InterruptMode::Mode1 };
            (2) => { InterruptMode::Mode2 };
        }

    };
} // define_helpers_scoped

/// Depending on the environment delegates to unreachable! macro on debug or
/// core::hint::unreachable_unchecked on release.
macro_rules! debug_unreachable_unchecked {
    () => {
        {
            #[cfg(debug_assertions)]
            { unreachable!() }
            #[cfg(not(debug_assertions))]
            unsafe { core::hint::unreachable_unchecked() }
        }
    };
}
