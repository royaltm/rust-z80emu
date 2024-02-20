/*
    z80emu: ZiLOG Z80 microprocessor emulation library.
    Copyright (C) 2019-2024  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
#![allow(unused_imports)]

use super::*;
use super::any::*;

#[cfg(feature = "std")]
macro_rules! z80_test {
    ($factory:expr, $wrap:expr, $wrap_end:expr, $unwrap_cpu:path, $flavour:expr) => {
        struct Dummy;

        impl Io for Dummy {
            type Timestamp = i32;
            type WrIoBreak = ();
            type RetiBreak = ();
        }

        impl Memory for Dummy {
            type Timestamp = i32;
        }

        let mut tsc = TsCounter::<i32>::default();
        let mut dummy = Dummy;
        let mut cpu = $factory;
        let debug = format!("{:?}", cpu);
        assert_eq!(debug,
            concat!($wrap, "Z80 { pc: 0, sp: 0, af: 0, bc: 0, de: 0, hl: 0, af': 0, bc': 0, de': 0, hl': 0, ix: 0, iy: 0, \
                   ir: 0, r: 0, im: Mode0, iff1: false, iff2: false, halt: false, ei: false, mp: 0, prefix: None }",
                   $wrap_end)
        );
        cpu.set_pc(0x1234);
        assert_eq!(cpu.get_pc(), 0x1234);
        cpu.set_sp(0xfedc);
        assert_eq!(cpu.get_sp(), 0xfedc);
        cpu.set_acc(0x42);
        assert_eq!(cpu.get_acc(), 0x42);
        assert_eq!(cpu.get_reg(Reg8::A, None), 0x42);
        assert_eq!(cpu.get_flags(), CpuFlags::empty());
        cpu.set_flags(CpuFlags::C|CpuFlags::X);
        assert_eq!(cpu.get_flags(), CpuFlags::C|CpuFlags::X);
        assert_eq!(cpu.get_reg2(StkReg16::AF), (0x42, (CpuFlags::C|CpuFlags::X).bits()));
        cpu.ex_af_af();
        assert_eq!(cpu.get_acc(), 0);
        assert_eq!(cpu.get_reg(Reg8::A, None), 0);
        assert_eq!(cpu.get_flags(), CpuFlags::empty());
        cpu.set_reg2(StkReg16::AF, 0xAF, (CpuFlags::S|CpuFlags::Z).bits());
        assert_eq!(cpu.get_acc(), 0xAF);
        assert_eq!(cpu.get_reg(Reg8::A, None), 0xAF);
        assert_eq!(cpu.get_flags(), CpuFlags::S|CpuFlags::Z);
        assert_eq!(cpu.get_reg2(StkReg16::AF), (0xAF, (CpuFlags::S|CpuFlags::Z).bits()));
        assert_eq!(cpu.get_alt_reg2(StkReg16::AF), (0x42, (CpuFlags::C|CpuFlags::X).bits()));
        cpu.set_reg16(StkReg16::BC, 0xABCD);
        assert_eq!(cpu.get_reg16(StkReg16::BC), 0xABCD);
        assert_eq!(cpu.get_reg2(StkReg16::BC), (0xAB, 0xCD));
        assert_eq!(cpu.get_reg(Reg8::B, None), 0xAB);
        assert_eq!(cpu.get_reg(Reg8::C, None), 0xCD);
        cpu.set_reg16(StkReg16::DE, 0xADEF);
        assert_eq!(cpu.get_reg16(StkReg16::DE), 0xADEF);
        assert_eq!(cpu.get_reg2(StkReg16::DE), (0xAD, 0xEF));
        assert_eq!(cpu.get_reg(Reg8::D, None), 0xAD);
        assert_eq!(cpu.get_reg(Reg8::E, None), 0xEF);
        cpu.set_reg16(StkReg16::HL, 0xF008);
        assert_eq!(cpu.get_reg16(StkReg16::HL), 0xF008);
        assert_eq!(cpu.get_reg2(StkReg16::HL), (0xF0, 0x08));
        assert_eq!(cpu.get_reg(Reg8::H, None), 0xF0);
        assert_eq!(cpu.get_reg(Reg8::L, None), 0x08);
        assert_eq!(cpu.get_alt_reg2(StkReg16::BC), (0, 0));
        assert_eq!(cpu.get_alt_reg2(StkReg16::DE), (0, 0));
        assert_eq!(cpu.get_alt_reg2(StkReg16::HL), (0, 0));
        assert_eq!(cpu.get_alt_reg16(StkReg16::BC), 0);
        assert_eq!(cpu.get_alt_reg16(StkReg16::DE), 0);
        assert_eq!(cpu.get_alt_reg16(StkReg16::HL), 0);
        cpu.exx();
        assert_eq!(cpu.get_alt_reg2(StkReg16::BC), (0xAB, 0xCD));
        assert_eq!(cpu.get_alt_reg2(StkReg16::DE), (0xAD, 0xEF));
        assert_eq!(cpu.get_alt_reg2(StkReg16::HL), (0xF0, 0x08));
        assert_eq!(cpu.get_alt_reg16(StkReg16::BC), 0xABCD);
        assert_eq!(cpu.get_alt_reg16(StkReg16::DE), 0xADEF);
        assert_eq!(cpu.get_alt_reg16(StkReg16::HL), 0xF008);
        assert_eq!(cpu.get_reg16(StkReg16::BC), 0);
        assert_eq!(cpu.get_reg16(StkReg16::DE), 0);
        assert_eq!(cpu.get_reg16(StkReg16::HL), 0);
        cpu.set_reg2(StkReg16::BC, 0x1B, 0xC2);
        assert_eq!(cpu.get_reg16(StkReg16::BC), 0x1BC2);
        cpu.set_reg2(StkReg16::DE, 0x3D, 0xE4);
        assert_eq!(cpu.get_reg16(StkReg16::DE), 0x3DE4);
        cpu.set_reg2(StkReg16::HL, 0x50, 0x06);
        assert_eq!(cpu.get_reg16(StkReg16::HL), 0x5006);
        cpu.set_index2(Prefix::Xdd, 0x55, 0xAA);
        assert_eq!(cpu.get_index16(Prefix::Xdd), 0x55AA);
        assert_eq!(cpu.get_index2(Prefix::Xdd), (0x55, 0xAA));
        cpu.set_index16(Prefix::Xdd, 0xA55A);
        assert_eq!(cpu.get_index16(Prefix::Xdd), 0xA55A);
        assert_eq!(cpu.get_index2(Prefix::Xdd), (0xA5, 0x5A));
        cpu.set_index2(Prefix::Yfd, 0x47, 0x38);
        assert_eq!(cpu.get_index16(Prefix::Yfd), 0x4738);
        assert_eq!(cpu.get_index2(Prefix::Yfd), (0x47, 0x38));
        cpu.set_index16(Prefix::Yfd, 0x5AA5);
        assert_eq!(cpu.get_index16(Prefix::Yfd), 0x5AA5);
        assert_eq!(cpu.get_index2(Prefix::Yfd), (0x5A, 0xA5));
        cpu.set_i(0x7F);
        assert_eq!(cpu.get_i(), 0x7F);
        assert_eq!(cpu.get_ir(), 0x7F00);
        cpu.set_r(0xF1);
        assert_eq!(cpu.get_r(), 0xF1);
        assert_eq!(cpu.get_ir(), 0x7FF1);
        cpu.add_r(0x8E);
        assert_eq!(cpu.get_i(), 0x7F);
        assert_eq!(cpu.get_r(), 0xFF);
        assert_eq!(cpu.get_ir(), 0x7FFF);
        assert_eq!(cpu.get_im(), InterruptMode::Mode0);
        cpu.set_im(InterruptMode::Mode2);
        assert_eq!(cpu.get_im(), InterruptMode::Mode2);
        assert_eq!(cpu.get_memptr(), 0);
        cpu.set_memptr(0xBACA);
        assert_eq!(cpu.get_memptr(), 0xBACA);
        assert!(!cpu.is_after_prefix());
        assert!(!cpu.is_irq_allowed());
        assert!(cpu.is_nmi_allowed());
        cpu.execute_instruction(&mut dummy, &mut tsc, Option::<CpuDebugFn>::None, 0xDD).unwrap();
        assert!(cpu.is_after_prefix());
        assert_eq!(cpu.get_iffs(), (false, false));
        assert!(!cpu.is_after_ei());
        assert!(!cpu.is_irq_allowed());
        assert!(!cpu.is_nmi_allowed());
        cpu.enable_interrupts();
        assert!(cpu.is_after_ei());
        assert!(!cpu.is_irq_allowed());
        assert!(!cpu.is_nmi_allowed());
        assert_eq!(cpu.get_iffs(), (true, true));
        assert!(!cpu.is_halt());
        cpu.halt();
        assert!(cpu.is_halt());
        let debug = format!("{:x?}\n{:x?}", cpu, $unwrap_cpu(cpu.clone()).flavour);
        assert_eq!(debug,
            concat!($wrap, "Z80 { \
                pc: 1234, \
                sp: fedc, \
                af: afc0, \
                bc: 1bc2, \
                de: 3de4, \
                hl: 5006, \
                af': 4209, \
                bc': abcd, \
                de': adef, \
                hl': f008, \
                ix: a55a, \
                iy: 5aa5, \
                ir: 7ff1, \
                r: 7f, \
                im: Mode2, \
                iff1: true, \
                iff2: true, \
                halt: true, \
                ei: true, \
                mp: baca, \
                prefix: Some(Xdd) \
            }", $wrap_end, "\n",
            $flavour)
        );
        assert_eq!($unwrap_cpu(cpu.clone()).ir.get16(), 0x7FF1);
        assert_eq!($unwrap_cpu(cpu.clone()).r.0, 0x7F);
        cpu.normalize_r();
        assert_eq!($unwrap_cpu(cpu.clone()).ir.get16(), 0x7FFF);
        assert_eq!($unwrap_cpu(cpu.clone()).r.0, 0xFF);
        assert_eq!(cpu.get_i(), 0x7F);
        assert_eq!(cpu.get_r(), 0xFF);
        assert_eq!(cpu.get_ir(), 0x7FFF);
        assert_eq!(cpu.get_iffs(), (true, true));
        cpu.disable_interrupts();
        assert_eq!(cpu.get_iffs(), (false, false));
        assert!(cpu.is_halt());
        assert!(cpu.is_after_prefix());
        assert!(cpu.is_after_ei());
        assert!(!cpu.is_irq_allowed());
        assert!(!cpu.is_nmi_allowed());
        assert!(!cpu.nmi(&mut dummy, &mut tsc));
        cpu.execute_instruction(&mut dummy, &mut tsc, Option::<CpuDebugFn>::None, 0x00).unwrap();
        assert!(!cpu.is_halt());
        assert!(!cpu.is_after_prefix());
        assert!(!cpu.is_after_ei());
        assert!(!cpu.is_irq_allowed());
        assert!(cpu.is_nmi_allowed());
        assert!(cpu.irq(&mut dummy, &mut tsc, Option::<CpuDebugFn>::None).is_none());
        cpu.set_iffs(true, true);
        assert_eq!(cpu.get_iffs(), (true, true));
        assert!(cpu.is_irq_allowed());
        assert!(cpu.is_nmi_allowed());
        assert!(!cpu.is_after_ei());
        assert!(cpu.nmi(&mut dummy, &mut tsc));
        assert!(!cpu.is_irq_allowed());
        assert!(cpu.is_nmi_allowed());
        assert_eq!(cpu.get_iffs(), (false, true));
        cpu.restore_iff1();
        assert_eq!(cpu.get_iffs(), (true, true));
        assert!(cpu.is_irq_allowed());
        assert!(cpu.is_nmi_allowed());
        cpu.irq(&mut dummy, &mut tsc, Option::<CpuDebugFn>::None).unwrap().unwrap();
    };
}

#[cfg(feature = "std")]
#[test]
fn z80_methods() {
    fn identity<C>(cpu: C) -> C { cpu }

    { z80_test!(Z80::<NMOS>::default(), "", "", identity,
        r#"NMOS { flags_modified: false, last_flags_modified: false }"#); }
    { z80_test!(Z80::<CMOS>::default(), "", "", identity, "CMOS"); }
    { z80_test!(Z80::<BM1>::default(), "", "", identity,
        r#"BM1 { flags_modified: false, last_flags_modified: false }"#); }
}

#[cfg(feature = "std")]
#[test]
fn z80any_methods() {
    { z80_test!(Z80Any::NMOS(Z80::default()), "NMOS(", ")", NMOS::unwrap_cpu_any,
            r#"NMOS { flags_modified: false, last_flags_modified: false }"#);
    }
    { z80_test!(Z80Any::CMOS(Z80::default()), "CMOS(", ")", CMOS::unwrap_cpu_any, "CMOS"); }
    { z80_test!(Z80Any::BM1(Z80::default()), "BM1(", ")", BM1::unwrap_cpu_any,
            r#"BM1 { flags_modified: false, last_flags_modified: false }"#);
    }
}

#[cfg(feature = "std")]
#[test]
fn z80_debug() {
    let cpu: Z80<NMOS> = Z80::new();
    let debug_msg = format!("{:#?}", cpu);
    assert_eq!(debug_msg,
r#"Z80 {
    pc: 0,
    sp: 65535,
    af: 65535,
    bc: 0,
    de: 0,
    hl: 0,
    af': 65535,
    bc': 0,
    de': 0,
    hl': 0,
    ix: 0,
    iy: 0,
    ir: 0,
    r: 0,
    im: Mode0,
    iff1: false,
    iff2: false,
    halt: false,
    ei: false,
    mp: 0,
    prefix: None,
}"#);

    let cpu = Z80Any::new_nmos();
    let debug_msg = format!("{:#x?}", cpu);
    assert_eq!(debug_msg,
r#"NMOS(
    Z80 {
        pc: 0x0,
        sp: 0xffff,
        af: 0xffff,
        bc: 0x0,
        de: 0x0,
        hl: 0x0,
        af': 0xffff,
        bc': 0x0,
        de': 0x0,
        hl': 0x0,
        ix: 0x0,
        iy: 0x0,
        ir: 0x0,
        r: 0x0,
        im: Mode0,
        iff1: false,
        iff2: false,
        halt: false,
        ei: false,
        mp: 0x0,
        prefix: None,
    },
)"#);
}
