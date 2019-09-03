#![macro_use]
//! This module contains various macros used by instructions module.

// macro_rules! replace_expr {
//     ($_t:tt $sub:expr) => {$sub};
// }

// macro_rules! count_exprs {
//     ($($x:expr),*) => {0usize $(+ replace_expr!($x 1usize))*};
// }

/// This is used by cpu_debug! to log the currently executed instruction using trace! macro.
macro_rules! cpu_trace_msg {
    ($deb:expr; $prefix:expr, [$($code:expr),+], $mnemonic:expr $(, $args:expr)*) => {
        #[cfg(debug_assertions)]
        {
            if log_enabled!(::log::Level::Trace) {
                match $prefix {
                    Prefix::None => {
                        trace!("{:8}{:12} {:02x?}", $mnemonic, format_args!($($args),*) , [$($code),+]);
                    }
                    _ => {
                        trace!("{:8}{:12} {:02x?}", $mnemonic, format_args!($($args),*) , [$prefix as u8, $($code),+]);
                    }
                }
                
            }
        }
    };
}

/// A macro that calls the debugger handler $deb if any was given.
/// When the constant None is given as $deb all the debug-related code is being opt-out by the optimizer.
macro_rules! cpu_debug {
    ($deb:expr; $prefix:expr, [$($code:expr),+], $mnemonic:expr) => {
        cpu_debug!($deb; $prefix, [$($code),+], $mnemonic, "");
    };
    ($deb:expr; $prefix:expr, [$($code:expr),+], $mnemonic:expr, $arg0:expr $(, $args:expr)*) => {
        {
            cpu_trace_msg!($deb; $prefix, [$($code),+], $mnemonic, $arg0 $(, $args)*);
            if let Some(debugger) = $deb {
                let mut code = arrayvec::ArrayVec::<[u8;4]>::new();
                code.extend([$($code),+].iter().cloned());
                debugger($prefix, code, $mnemonic, format_args!($arg0 $(, $args)*));
            }
        }
    };
}

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

/// Reads 1 byte from memory via PC register ($pc). Increases $pc afterwards.
/// Used only to read the immediate 8-bit instruction argument.
/// For reading an op-code see fetch_next_opcode!.
macro_rules! fetch_next_imm8 {
    ($control:ident, $pc:ident, $tsc:ident $(,1x $add_ts_more:expr)*) => {
        { // pc:3, pc+=1
            let code = $control.read_mem($pc.0, $tsc.add_mreq($pc.0, MEMRW_CYCLE));
            $( // pc:1 x m
                $tsc.add_no_mreq($pc.0, $add_ts_more);
            )*
            $pc += Wrapping(1);
            code
        }
    };
}

/// Reads 2 bytes from memory via PC register ($pc) as a LE u16. Increases $pc afterwards.
/// Used to read the immediate 16-bit instruction argument.
macro_rules! fetch_next_imm16 {
    ($control:ident, $pc:ident, $tsc:ident $(,1x $add_ts_more:expr)*) => {
        { // pc:3, pc+1:3, pc+=2
            let nn: u16 = $control.read_mem16($pc.0, $tsc.add_mreq($pc.0, MEMRW_CYCLE));
            let pc1 = $pc.0.wrapping_add(1);
            $tsc.add_mreq(pc1, MEMRW_CYCLE);
            $( // pc:1 x m
                $tsc.add_no_mreq(pc1, $add_ts_more);
            )*
            $pc += Wrapping(2);
            nn
        }
    };
}

/// Reads 1 byte from memory via one of the 16-bit registers: BC, DE, HL, IX or IY.
macro_rules! read_mem8_reg16 {
    (<- [$reg16:ident] $(memptr=$memptr:expr)* ; $cpu:ident, $control:ident, $tsc:ident $(,1x $add_ts_more:expr)*) => {
        { // ss: 3
            let $reg16: u16 = $cpu.regs.$reg16.get16();
            $( // only BC or DE
                $cpu.memptr.set16($memptr);
            )*
            let val = $control.read_mem($reg16, $tsc.add_mreq($reg16, MEMRW_CYCLE));
            $(
                $tsc.add_no_mreq($reg16, $add_ts_more);
            )*
            val
        }
    };
    (<- [ii+$index8:ident] memptr=ii+d; $prefix:ident, $cpu:ident, $control:ident, $tsc:ident) => {
        { // ii+d: 3
            let ii_d = indexed_address!($cpu.get_index16($prefix), $index8);
            // Any instruction with (INDEX+d): MEMPTR = INDEX+d
            $cpu.memptr.set16(ii_d);
            $control.read_mem(ii_d, $tsc.add_mreq(ii_d, MEMRW_CYCLE))
        }
    };
}

/// Reads 1 byte from memory at immediate address $addr.
macro_rules! read_mem8_addr16 {
    (<- [$addr:expr] memptr=addr+1 ; $cpu:ident, $control:ident, $tsc:ident) => {
        {   // nn: 3
            // LD A, (addr) MEMPTR = addr + 1
            $cpu.memptr.set16($addr.wrapping_add(1));
            $control.read_mem($addr, $tsc.add_mreq($addr, MEMRW_CYCLE))
        }
    };
}

/// Reads 2 bytes from memory at immediate address $addr and $addr + 1 as an LE u16 word.
macro_rules! read_mem16_addr16 {
    (<- [$addr:expr] memptr=addr+1 ; $cpu:ident, $control:ident, $tsc:ident) => {
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

/// Writes 1 byte into memory via one of the address registers: HL, IX or IY.
macro_rules! write_mem8_reg16 {
    ([$reg16:ident] <- $val:expr $(, memptr=$memptr:expr)* ; $cpu:ident, $control:ident, $tsc:ident) => {
        { // ss: 3
            let $reg16 = $cpu.regs.$reg16.get16();
            $( // only BC or DE
                $cpu.memptr.set16($memptr);
            )*
            $control.write_mem($reg16, $val, $tsc.add_mreq($reg16, MEMRW_CYCLE));
        }
    };
    ([ii+$index8:ident] <- $val:expr, memptr=ii+d; $prefix:ident, $cpu:ident, $control:ident, $tsc:ident) => {
        { // ii+d: 3
            let ii_d = indexed_address!($cpu.get_index16($prefix), $index8);
            // Any instruction with (INDEX+d): MEMPTR = INDEX+d
            $cpu.memptr.set16(ii_d);
            $control.write_mem(ii_d, $val, $tsc.add_mreq(ii_d, MEMRW_CYCLE));
        }
    };
}

/// Used by various 8-bit operations on the memory via one of the address registers: HL, IX or IY.
macro_rules! r_op_w_mem8 {
    (@internal $cpu:ident, $control:ident, $tsc:ident; [$reg16:ident] $oper:expr) => {
        { // hl:3, hl:1, hl(write):3
            let $reg16: u16 = $cpu.regs.$reg16.get16();
            let val: u8 = $oper;
            $tsc.add_no_mreq($reg16, 1);
            $control.write_mem($reg16, val, $tsc.add_mreq($reg16, MEMRW_CYCLE));
        }
    };
    ($opfn:ident [hl] ; $flags:ident, $cpu:ident, $control:ident, $tsc:ident) => {
        r_op_w_mem8! { @internal $cpu, $control, $tsc; [hl]
            $opfn($control.read_mem(hl, $tsc.add_mreq(hl, MEMRW_CYCLE)), &mut $flags)
        }
    };
    ($opfn:ident $b:expr, [hl] ; $cpu:ident, $control:ident, $tsc:ident) => {
        r_op_w_mem8! { @internal $cpu, $control, $tsc; [hl]
            $opfn($b, $control.read_mem(hl, $tsc.add_mreq(hl, MEMRW_CYCLE)))
        }
    };
    ($opfn:ident [ii+$index8:ident] memptr=ii+d; $prefix:ident, $flags:ident, $cpu:ident, $control:ident, $tsc:ident) => {
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

/// Writes 1 byte into memory at immediate address $addr.
macro_rules! write_mem8_addr16 {
    ([$addr:expr] <- $val:expr, memptr=(addr+1)&#FF|A<<8 ; $cpu:ident, $control:ident, $tsc:ident) => {
        { // nn: 3
            // MEMPTR_low = (addr + 1) & #FF,  MEMPTR_hi = A
            $cpu.memptr.set16($addr.wrapping_add(1) & 0xFF | ($val as u16) << 8);
            $control.write_mem($addr, $val, $tsc.add_mreq($addr, MEMRW_CYCLE));
        }
    };
}

/// Writes 2 bytes into memory at immediate address $addr ($vlo) and $addr + 1 ($vhi).
macro_rules! write_mem16_addr16 {
    ([$addr:expr] <- ($vhi:expr, $vlo:expr), memptr=addr+1 ; $cpu:ident, $control:ident, $tsc:ident) => {
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
    (($vhi:expr, $vlo:expr); $cpu:ident, $control:ident, $tsc:ident) => {
        { // sp-1:3,sp-2:3
            let mut sp = $cpu.sp.get16().wrapping_sub(1);
            $control.write_mem(sp, $vhi, $tsc.add_mreq(sp, MEMRW_CYCLE));
            sp = sp.wrapping_sub(1);
            $control.write_mem(sp, $vlo, $tsc.add_mreq(sp, MEMRW_CYCLE));
            $cpu.sp.set16(sp);
        }
    };
}

/// Writes a 16-bit LE integer into memory at SP - 1 and SP - 2. Decreases SP by 2 afterwards.
macro_rules! push16 {
    ($val:expr; $cpu:ident, $control:ident, $tsc:ident) => {
        { // sp-1:3,sp-2:3
            push2!((($val >> 8) as u8, $val as u8); $cpu, $control, $tsc);
        }
    };
}

/// Reads 2 bytes from memory at SP and SP + 1 as an LE u16 integer. Increases SP by 2 afterwards.
macro_rules! pop16 {
    ($cpu:ident, $control:ident, $tsc:ident) => {
        { // sp:3,sp+1:3
            let sp = $cpu.sp.get16();
            let val = $control.read_mem16(sp, $tsc.add_mreq(sp, MEMRW_CYCLE));
            $tsc.add_mreq(sp.wrapping_add(1), MEMRW_CYCLE);
            $cpu.sp.set16(sp.wrapping_add(2));
            val
        }
    };
}

/// Used by EX (SP), ii
macro_rules! ex_sp_nn {
    (($vhi:expr, $vlo:expr); $cpu:ident, $control:ident, $tsc:ident) => {
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

/// Calculates a relative branch address from $pc as a Wrapping 16 bit integer
/// and 8-bit twos compliment offset in $e.
macro_rules! relative_jump_address {
    ($pc:expr, $e:expr) => {
        $pc + Wrapping($e as i8 as i16 as u16)
    };
}

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
