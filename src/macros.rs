#![macro_use]
//! This module contains various macros used by instructions module.

// macro_rules! replace_expr {
//     ($_t:tt $sub:expr) => {$sub};
// }

// macro_rules! count_exprs {
//     ($($x:expr),*) => {0usize $(+ replace_expr!($x 1usize))*};
// }

/// Reads 1 byte from memory via PC register ($pc). Increases $pc afterwards.
/// Used for the M1 cycle (op-code fetch) so a proper number of M1 cycles is being added.
/// Increases the memory refresh (R) counter.
macro_rules! fetch_next_opcode {
    ($cpu:ident, $control:ident, $pc:ident, $tsc:ident) => {
        { // pc:4, pc+=1
            $cpu.inc_r();
            let code = $control.read_opcode($pc.0, $cpu.get_ir(), $tsc.add_mreq($pc.0, M1_CYCLE));
            $pc += Wrapping(1);
            code
        }
    };
}

/// Defines scoped macros used by opcodes::execute_instruction! macro.
macro_rules! define_helpers_scoped {
    ([$dol:tt] $deb:expr; $prefix:ident, $flags:ident, $pc:ident, $cpu:ident, $control:ident, $tsc:ident) => {
        /// A sugar for creating CpuDebug and calling a debugger function if it was given as $deb.
        macro_rules! cpu_debug {
            ([$dol($code:expr),+] $mnemonic:literal $dol($id:ident:$e:tt),*) => {
                cpu_debug!([$dol($code),+] str($mnemonic) $dol($id:$e),*);
            };
            ([$dol($code:expr),+] $mnemonic:ident $dol($id:ident:$e:tt),*) => {
                cpu_debug!([$dol($code),+] str(stringify!($mnemonic)) $dol($id:$e),*);
            };
            ([$dol($code:expr),+] str($mnemonic:expr) $dol($id:ident:$e:tt),*) => {
                if let Some(debugger) = $deb {
                    let mnemonic: &'static str = $mnemonic.into();
                    let args: CpuDebugArgs = cpu_debug!(@args $dol($id:$e),*);
                    let mut code = CpuDebugCode::new();
                    if $prefix != Prefix::None {
                        code.push($prefix as u8);
                    }
                    code.extend([$dol($code),+].iter().cloned());
                    let pc = $pc - Wrapping(code.len() as u16);
                    debugger(CpuDebug {
                        code,
                        mnemonic,
                        pc: pc.0,
                        prefix: $prefix,
                        args
                    });
                }
            };
            (@args)                   => { CpuDebugArgs::None };
            (@args $id1:ident:$e1:tt) => { CpuDebugArgs::Single(cpu_debug!(@arg $id1:$e1)) };
            (@args $id1:ident:$e1:tt, $id2:ident:$e2:tt) => {
                CpuDebugArgs::Double(cpu_debug!(@arg $id1:$e1), cpu_debug!(@arg $id2:$e2))
            };
            (@args maybe_b:$b:expr, ii:$d:expr, maybe_r:$res:expr) => {
                match ($b, $res) {
                    (Some(b), Ok(reg)) => CpuDebugArgs::BitOpExt(b, cpu_debug!(@arg ii:$d), reg),
                    (Some(b), Err(_))  => CpuDebugArgs::Double(cpu_debug!(@arg b:b), cpu_debug!(@arg ii:$d)),
                    (None,    Ok(reg)) => CpuDebugArgs::Double(cpu_debug!(@arg ii:$d), cpu_debug!(@arg r:reg)),
                    (None,    Err(_))  => CpuDebugArgs::Single(cpu_debug!(@arg ii:$d))
                }
            };
            (@arg n:$n:expr)      => { CpuDebugArg::Imm8($n) };
            (@arg b:$b:expr)      => { CpuDebugArg::Imm8($b as u8) };
            (@arg nn:$nn:expr)    => { CpuDebugArg::Imm16($nn) };
            (@arg nn_0:$nn:expr)  => { CpuDebugArg::Imm16($nn.0) };
            (@arg r:A)            => { CpuDebugArg::Reg8(Prefix::None, Reg8::A) };
            (@arg r:I)            => { CpuDebugArg::I };
            (@arg r:R)            => { CpuDebugArg::R };
            (@arg r:$reg:expr)    => { CpuDebugArg::Reg8(Prefix::None, $reg) };
            (@arg q:$reg:expr)    => { CpuDebugArg::Reg8($prefix, $reg) };
            (@arg rr:HL)          => { CpuDebugArg::Reg16(Prefix::None, Reg16::HL) };
            (@arg rr:DE)          => { CpuDebugArg::Reg16(Prefix::None, Reg16::DE) };
            (@arg rr:SP)          => { CpuDebugArg::Reg16(Prefix::None, Reg16::SP) };
            (@arg rr:$reg:expr)   => { CpuDebugArg::Reg16(Prefix::None, $reg) };
            (@arg qq:ii)          => { CpuDebugArg::Reg16($prefix, Reg16::HL) };
            (@arg qq:$reg:expr)   => { CpuDebugArg::Reg16($prefix, $reg) };
            (@arg ss:AF)          => { CpuDebugArg::Stk16(StkReg16::AF) };
            (@arg ss:$reg:expr)   => { CpuDebugArg::Stk16($reg) };
            (@arg addr:HL)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::HL)) };
            (@arg addr:bc)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::BC)) };
            (@arg addr:de)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::DE)) };
            (@arg addr:SP)        => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::SP)) };
            (@arg addr:ii)        => { CpuDebugArg::Addr(CpuDebugAddr::IndexAddr($prefix, None)) };
            (@arg addr:$reg:expr) => { CpuDebugArg::Addr(CpuDebugAddr::RegAddr($reg)) };
            (@arg ii:$d:expr)     => { CpuDebugArg::Addr(CpuDebugAddr::IndexAddr($prefix, Some($d as i8))) };
            (@arg adnn:$nn:expr)  => { CpuDebugArg::Addr(CpuDebugAddr::ImmAddr($nn)) };
            (@arg port:C)         => { CpuDebugArg::Port(CpuDebugPort::RegPort) };
            (@arg port:$n:expr)   => { CpuDebugArg::Port(CpuDebugPort::ImmPort($n)) };
            (@arg cc:$cond:expr)  => { CpuDebugArg::Cond($cond) };
            (@arg rel:$e:expr)    => { CpuDebugArg::Imm16(relative_jump_address!($e).0) };
            (@arg r_addr:$res:expr) => {
                match $res {
                    Ok(reg) => CpuDebugArg::Reg8(Prefix::None, reg),
                    Err(_) => CpuDebugArg::Addr(CpuDebugAddr::RegAddr(Reg16::HL))
                }
            };
        }

        /// Reads 1 byte from memory via PC register ($pc). Increases $pc afterwards.
        /// Used only to read the immediate 8-bit instruction argument.
        /// For reading an op-code see fetch_next_opcode!.
        macro_rules! fetch_next_imm8 {
            ($dol(no_mreq: 1x $add_ts_more:expr)?) => {
                { // pc:3, pc+=1
                    let code = $control.read_mem($pc.0, $tsc.add_mreq($pc.0, MEMRW_CYCLE));
                    $dol( // pc:1 x m
                        $tsc.add_no_mreq($pc.0, $add_ts_more);
                    )?
                    $pc += Wrapping(1);
                    code
                }
            };
        }

        /// Reads 2 bytes from memory via PC register ($pc) as a LE u16. Increases $pc afterwards.
        /// Used to read the immediate 16-bit instruction argument.
        macro_rules! fetch_next_imm16 {
            ($dol(no_mreq: 1x $add_ts_more:expr)?) => {
                { // pc:3, pc+1:3, pc+=2
                    let nn: u16 = $control.read_mem16($pc.0, $tsc.add_mreq($pc.0, MEMRW_CYCLE));
                    let pc1 = $pc.0.wrapping_add(1);
                    $tsc.add_mreq(pc1, MEMRW_CYCLE);
                    $dol( // pc:1 x m
                        $tsc.add_no_mreq(pc1, $add_ts_more);
                    )?
                    $pc += Wrapping(2);
                    nn
                }
            };
        }

        /// Used by various 8-bit operations on the memory via one of the address registers: HL, IX or IY.
        macro_rules! r_op_w_mem8 {
            (@internal [$reg16:ident] $oper:expr) => {
                { // hl:3, hl:1, hl(write):3
                    let $reg16: u16 = $cpu.regs.$reg16.get16();
                    let val: u8 = $oper;
                    $tsc.add_no_mreq($reg16, 1);
                    $control.write_mem($reg16, val, $tsc.add_mreq($reg16, MEMRW_CYCLE));
                }
            };
            ($opfn:ident [hl]) => {
                r_op_w_mem8! { @internal [hl]
                    $opfn($control.read_mem(hl, $tsc.add_mreq(hl, MEMRW_CYCLE)), &mut $flags)
                }
            };
            ($opfn:ident $b:expr, [hl]) => {
                r_op_w_mem8! { @internal [hl]
                    $opfn($b, $control.read_mem(hl, $tsc.add_mreq(hl, MEMRW_CYCLE)))
                }
            };
            ($opfn:ident [ii+$index8:ident] memptr=ii+d) => {
                { // ii+d:3, ii+d:1, ii+d(write):3
                    let ii_d = indexed_address!($cpu.get_index16($prefix), $index8);
                    let val: u8 = $opfn($control.read_mem(ii_d, $tsc.add_mreq(ii_d, MEMRW_CYCLE)), &mut $flags);
                    $tsc.add_no_mreq(ii_d, 1);
                    // Any instruction with (INDEX+d): MEMPTR = INDEX+d
                    $cpu.memptr.set16(ii_d);
                    $control.write_mem(ii_d, val, $tsc.add_mreq(ii_d, MEMRW_CYCLE));
                }
            };
        }

        /// Reads 1 byte from memory via one of the 16-bit registers: BC, DE, HL, IX or IY.
        macro_rules! read_mem8_reg16 {
            (<- [$reg16:ident] $dol(memptr=$memptr:expr)? $dol(;no_mreq: 1x $add_ts_more:expr)?) => {
                { // ss: 3
                    let $reg16: u16 = $cpu.regs.$reg16.get16();
                    $dol(
                        $cpu.memptr.set16($memptr);
                    )?
                    let val = $control.read_mem($reg16, $tsc.add_mreq($reg16, MEMRW_CYCLE));
                    $dol(
                        $tsc.add_no_mreq($reg16, $add_ts_more);
                    )?
                    val
                }
            };
            (<- [ii+$index8:ident] memptr=ii+d) => {
                { // ii+d: 3
                    let ii_d = indexed_address!($cpu.get_index16($prefix), $index8);
                    // Any instruction with (INDEX+d): MEMPTR = INDEX+d
                    $cpu.memptr.set16(ii_d);
                    $control.read_mem(ii_d, $tsc.add_mreq(ii_d, MEMRW_CYCLE))
                }
            };
        }

        /// Writes 1 byte into memory via one of the address registers: HL, IX or IY.
        /// Returns the u16 content of the address register for optional memptr updating.
        macro_rules! write_mem8_reg16 {
            ([$reg16:ident] <- $val:expr $dol(;memptr=($mhi:expr, $mlo:expr))?) => {
                { // ss: 3
                    let $reg16 = $cpu.regs.$reg16.get16();
                    $dol(
                        $cpu.memptr.set($mhi, $mlo);
                    )?
                    $control.write_mem($reg16, $val, $tsc.add_mreq($reg16, MEMRW_CYCLE));
                }
            };
            ([ii+$index8:ident] <- $val:expr; memptr=ii+d) => {
                { // ii+d: 3
                    let ii_d = indexed_address!($cpu.get_index16($prefix), $index8);
                    // Any instruction with (INDEX+d): MEMPTR = INDEX+d
                    $cpu.memptr.set16(ii_d);
                    $control.write_mem(ii_d, $val, $tsc.add_mreq(ii_d, MEMRW_CYCLE));
                }
            };
        }

        /// Reads 2 bytes from memory at immediate address $addr and $addr + 1 as an LE u16 word.
        macro_rules! read_mem16_addr16 {
            (<- [$addr:expr] memptr=addr+1) => {
                { // nn:3, nn+1:3
                    // LD rp,(addr) MEMPTR = addr + 1
                    let val = $control.read_mem16($addr, $tsc.add_mreq($addr, MEMRW_CYCLE));
                    let addr1 = $addr.wrapping_add(1);
                    $cpu.memptr.set16(addr1);
                    $tsc.add_mreq(addr1, MEMRW_CYCLE);
                    val
                }
            };
        }

        /// Writes 2 bytes into memory at immediate address $addr ($vlo) and $addr + 1 ($vhi).
        macro_rules! write_mem16_addr16 {
            ([$addr:expr] <- ($vhi:expr, $vlo:expr); memptr=addr+1) => {
                { // nn:3, nn+1:3
                    $control.write_mem($addr, $vlo, $tsc.add_mreq($addr, MEMRW_CYCLE));
                    let addr_1 = $addr.wrapping_add(1);
                    // LD (addr), rp; MEMPTR = addr + 1
                    $cpu.memptr.set16(addr_1);
                    $control.write_mem(addr_1, $vhi, $tsc.add_mreq(addr_1, MEMRW_CYCLE));
                }
            };
        }

        /// Writes 2 bytes into memory at SP - 1 ($vhi) and SP - 2 ($vlo). Decreases SP by 2 afterwards.
        macro_rules! push2 {
            ($vhi:expr, $vlo:expr) => {
                $cpu.push2($vhi, $vlo, $control, &mut $tsc);
            };
        }

        /// Writes a 16-bit LE integer into memory at SP - 1 and SP - 2. Decreases SP by 2 afterwards.
        macro_rules! push16 {
            ($val:expr) => {
                $cpu.push16($val, $control, &mut $tsc);
            };
        }

        /// Reads 2 bytes from memory at SP and SP + 1 as an LE u16 integer. Increases SP by 2 afterwards.
        macro_rules! pop16 {
            () => {
                $cpu.pop16($control, &mut $tsc)
            };
        }

        /// Used by EX (SP), ii
        macro_rules! ex_sp_nn {
            ($vhi:expr, $vlo:expr) => {
                { // sp:3,sp+1:3,sp+1:1,sp+1(write):3,sp(write):3,sp(write):1 x 2
                    let sp = $cpu.sp.get16();
                    let val = $control.read_mem16(sp, $tsc.add_mreq(sp, MEMRW_CYCLE));
                    let sp_1 = sp.wrapping_add(1);
                    $tsc.add_mreq(sp_1, MEMRW_CYCLE);
                    $tsc.add_no_mreq(sp_1, 1);
                    $control.write_mem(sp_1, $vhi, $tsc.add_mreq(sp_1, MEMRW_CYCLE));
                    $control.write_mem(sp, $vlo, $tsc.add_mreq(sp, MEMRW_CYCLE));
                    $tsc.add_no_mreq(sp, 2);
                    val
                }
            };
        }

        // RLD | RRD
        macro_rules! instr_rxd {
            ($rxd:path) => {
                { // hl:3, hl:1 x 4, hl(write):3
                    let hl: u16 = $cpu.regs.hl.get16();
                    // RLD/RRD  MEMPTR = HL + 1
                    $cpu.memptr.set16(hl.wrapping_add(1));
                    let val = $control.read_mem(hl, $tsc.add_mreq(hl, MEMRW_CYCLE));
                    $tsc.add_no_mreq(hl, 4);
                    $cpu.af.op8hi(|a| {
                        let (a, res) : (u8, u8) = $rxd(a, val, &mut $flags);
                        $control.write_mem(hl, res, $tsc.add_mreq(hl, MEMRW_CYCLE));
                        a
                    });
                }
            };
        }

        /// Calculates a relative branch address from $pc as a Wrapping 16 bit integer
        /// and 8-bit twos compliment offset in $e.
        macro_rules! relative_jump_address {
            ($e:expr) => {
                $pc + Wrapping($e as i8 as i16 as u16)
            };
        }
    };
} // define_helpers_scoped

/// Calculates a wrapping 16-bit $ii + $d where $ii is a 16 bit unsigned address and
/// $d can be a signed or unsigned integer representing a twos compliment 8-bit offset.
macro_rules! indexed_address {
    ($ii:expr, $d:expr) => {
        $ii.wrapping_add($d as i8 as i16 as u16)
    };
}

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
