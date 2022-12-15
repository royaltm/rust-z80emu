/*
    z80emu: a minimalistic Z80 CPU emulation library.
    Copyright (C) 2019-2022  Rafal Michalski

    For the full copyright notice, see the lib.rs file.
*/
use std::borrow::Cow;
use core::marker::PhantomData;
use core::fmt;
use core::num::Wrapping;
use ::serde::{
    Serialize, Deserialize,
    ser::{Serializer, SerializeStruct},
    de::{self, Deserializer, Visitor, SeqAccess, MapAccess}
};
use crate::RegisterPair;
use super::{Z80, NMOS, CMOS, BM1, Flavour, any::Z80Any};

impl<Q: Flavour + Serialize> Serialize for Z80<Q> {
    fn serialize<S>(&self, serializer: S) -> core::result::Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut state = serializer.serialize_struct("Z80", 18)?;
        state.serialize_field("type", Q::tag())?;
        state.serialize_field("af", &self.af)?;
        state.serialize_field("afAlt", &self.af_alt)?;
        state.serialize_field("regs", &self.regs)?;
        state.serialize_field("regsAlt", &self.regs_alt)?;
        state.serialize_field("index", &self.index)?;
        state.serialize_field("pc", &self.pc)?;
        state.serialize_field("sp", &self.sp)?;
        state.serialize_field("memptr", &self.memptr)?;
        state.serialize_field("lastEi", &self.last_ei)?;
        state.serialize_field("ir", &self.ir)?;
        state.serialize_field("im", &self.im)?;
        state.serialize_field("iff1", &self.iff1)?;
        state.serialize_field("iff2", &self.iff2)?;
        state.serialize_field("halt", &self.halt)?;
        state.serialize_field("prefix", &self.prefix)?;
        state.serialize_field("r", &self.r)?;
        state.serialize_field("flavour", &self.flavour)?;
        state.end()
    }
}

impl Serialize for Z80Any {
    fn serialize<S>(&self, serializer: S) -> core::result::Result<S::Ok, S::Error>
        where S: Serializer
    {
        match self {
            Z80Any::NMOS(z80) => z80.serialize(serializer),
            Z80Any::CMOS(z80) => z80.serialize(serializer),
            Z80Any::BM1(z80) => z80.serialize(serializer),
        }
    }
}

const Z80_FIELDS: &[&str] = &[
    "type",
    "af",
    "afAlt",
    "regs",
    "regsAlt",
    "index",
    "pc",
    "sp",
    "memptr",
    "lastEi",
    "ir",
    "im",
    "iff1",
    "iff2",
    "halt",
    "prefix",
    "r",
    "flavour",
];

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "camelCase")]
enum Z80Field {
    Type,
    Af,
    #[serde(alias = "af_alt")] AfAlt,
    Regs,
    #[serde(alias = "regs_alt")] RegsAlt,
    Index,
    Pc,
    Sp,
    Memptr,
    #[serde(alias = "last_ei")] LastEi,
    Ir,
    Im,
    Iff1,
    Iff2,
    Halt,
    Prefix,
    R,
    Flavour,
}

impl<'de, Q> Deserialize<'de> for Z80<Q>
    where Q: Flavour + Deserialize<'de>
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct Z80Visitor<Q>(PhantomData<Q>);

        impl<'de, Q> Visitor<'de> for Z80Visitor<Q>
            where Q: Flavour + Deserialize<'de>
        {
            type Value = Z80<Q>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Z80")
            }

            fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<Z80<Q>, V::Error> {
                if let Some(17) = seq.size_hint() {
                    visit_seq(self, seq, 0)
                }
                else {
                    let variant: Cow<str> = seq.next_element()?
                                     .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                    match &*variant {
                        "NMOS"|"CMOS"|"BM1" => visit_seq(self, seq, 1),
                        _ => Err(de::Error::unknown_variant(&variant, &["NMOS", "CMOS", "BM1"]))
                    }
                }
            }

            fn visit_map<V: MapAccess<'de>>(self, map: V) -> Result<Z80<Q>, V::Error> {
                let cpu_any = visit_map(map, Some(Q::tag()))?;
                Ok(Q::unwrap_cpu_any(cpu_any))
            }
        }

        deserializer.deserialize_struct("Z80", Z80_FIELDS, Z80Visitor(PhantomData))
    }
}

impl<'de> Deserialize<'de> for Z80Any {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct Z80AnyVisitor;

        impl<'de> Visitor<'de> for Z80AnyVisitor {
            type Value = Z80Any;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Z80")
            }

            fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<Z80Any, V::Error> {
                if let Some(17) = seq.size_hint() {
                    visit_seq(self, seq, 0).map(Z80Any::NMOS)
                }
                else {
                    let variant: Cow<str> = seq.next_element()?
                                     .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                    match &*variant {
                        "NMOS" => visit_seq(self, seq, 1).map(Z80Any::NMOS),
                        "CMOS" => visit_seq(self, seq, 1).map(Z80Any::CMOS),
                        "BM1"  => visit_seq(self, seq, 1).map(Z80Any::BM1),
                        _ => Err(de::Error::unknown_variant(&variant, &["NMOS", "CMOS", "BM1"]))
                    }
                }
            }

            fn visit_map<V: MapAccess<'de>>(self, map: V) -> Result<Z80Any, V::Error> {
                visit_map(map, None)
            }
        }

        deserializer.deserialize_struct("Z80", Z80_FIELDS, Z80AnyVisitor)
    }
}

fn visit_seq<'de, V, A, Q>(visitor: V, mut seq: A, n: usize) -> Result<Z80<Q>, A::Error>
    where V: Visitor<'de>,
          A: SeqAccess<'de>,
          Q: Flavour + Deserialize<'de>
{
    let af       = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n    , &visitor))?;
    let af_alt   = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 1, &visitor))?;
    let regs     = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 2, &visitor))?;
    let regs_alt = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 3, &visitor))?;
    let index    = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 4, &visitor))?;
    let pc       = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 5, &visitor))?;
    let sp       = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 6, &visitor))?;
    let memptr   = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 7, &visitor))?;
    let last_ei  = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 8, &visitor))?;
    let ir       = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 9, &visitor))?;
    let im       = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 10, &visitor))?;
    let iff1     = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 11, &visitor))?;
    let iff2     = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 12, &visitor))?;
    let halt     = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 13, &visitor))?;
    let prefix   = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 14, &visitor))?;
    let r        = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 15, &visitor))?;
    let flavour  = seq.next_element()?.ok_or_else(|| de::Error::invalid_length(n + 16, &visitor))?;

    Ok(Z80 {
        af,
        af_alt,
        regs,
        regs_alt,
        index,
        pc,
        sp,
        memptr,
        last_ei,
        ir,
        im,
        iff1,
        iff2,
        halt,
        prefix,
        r,
        flavour
    })
}

macro_rules! check_dup_assign {
    ($val:ident = $map:ident[$label:expr]) => {
        {
            if $val.is_some() {
                return Err(de::Error::duplicate_field($label));
            }
            $val = Some($map.next_value()?);
        }
    };
}

const DEFAULT_VARIANT: Cow<'_, str> = Cow::Borrowed("NMOS");

fn visit_map<'de, A>(mut map: A, force_variant: Option<&str>) -> Result<Z80Any, A::Error>
    where A: MapAccess<'de>
{
    let mut variant: Option<Cow<str>> = None;
    let mut af       = None;
    let mut af_alt   = None;
    let mut regs     = None;
    let mut regs_alt = None;
    let mut index    = None;
    let mut pc       = None;
    let mut sp       = None;
    let mut memptr   = None;
    let mut last_ei  = None;
    let mut ir       = None;
    let mut im       = None;
    let mut iff1     = None;
    let mut iff2     = None;
    let mut halt     = None;
    let mut prefix   = None;
    let mut r        = None;
    let mut flavour: Option<NMOS> = None;

    while let Some(key) = map.next_key()? {
        match key {
            Z80Field::Type    => check_dup_assign!(variant  = map["type"]),
            Z80Field::Af      => check_dup_assign!(af       = map["af"]),
            Z80Field::AfAlt   => check_dup_assign!(af_alt   = map["afAlt"]),
            Z80Field::Regs    => check_dup_assign!(regs     = map["regs"]),
            Z80Field::RegsAlt => check_dup_assign!(regs_alt = map["regsAlt"]),
            Z80Field::Index   => check_dup_assign!(index    = map["index"]),
            Z80Field::Pc      => check_dup_assign!(pc       = map["pc"]),
            Z80Field::Sp      => check_dup_assign!(sp       = map["sp"]),
            Z80Field::Memptr  => check_dup_assign!(memptr   = map["memptr"]),
            Z80Field::LastEi  => check_dup_assign!(last_ei  = map["lastEi"]),
            Z80Field::Ir      => check_dup_assign!(ir       = map["ir"]),
            Z80Field::Im      => check_dup_assign!(im       = map["im"]),
            Z80Field::Iff1    => check_dup_assign!(iff1     = map["iff1"]),
            Z80Field::Iff2    => check_dup_assign!(iff2     = map["iff2"]),
            Z80Field::Halt    => check_dup_assign!(halt     = map["halt"]),
            Z80Field::Prefix  => check_dup_assign!(prefix   = map["prefix"]),
            Z80Field::R       => check_dup_assign!(r        = map["r"]),
            Z80Field::Flavour => check_dup_assign!(flavour  = map["flavour"]),
        }
    }
    let af       = af.ok_or_else(||         de::Error::missing_field("af"))?;
    let af_alt   = af_alt.ok_or_else(||     de::Error::missing_field("afAlt"))?;
    let regs     = regs.ok_or_else(||       de::Error::missing_field("regs"))?;
    let regs_alt = regs_alt.ok_or_else(||   de::Error::missing_field("regsAlt"))?;
    let index    = index.ok_or_else(||      de::Error::missing_field("index"))?;
    let pc       = pc.ok_or_else(||         de::Error::missing_field("pc"))?;
    let sp       = sp.ok_or_else(||         de::Error::missing_field("sp"))?;
    let memptr   = memptr.unwrap_or_default();
    let last_ei  = last_ei.ok_or_else(||    de::Error::missing_field("lastEi"))?;
    let ir: RegisterPair = ir.ok_or_else(|| de::Error::missing_field("ir"))?;
    let im       = im.ok_or_else(||         de::Error::missing_field("im"))?;
    let iff1     = iff1.ok_or_else(||       de::Error::missing_field("iff1"))?;
    let iff2     = iff2.ok_or_else(||       de::Error::missing_field("iff2"))?;
    let halt     = halt.ok_or_else(||       de::Error::missing_field("halt"))?;
    let prefix   = prefix.ok_or_else(||     de::Error::missing_field("prefix"))?;
    let r        = r.unwrap_or_else(|| Wrapping(ir.get8lo()));

    match &*force_variant.map(Cow::Borrowed).unwrap_or_else(|| variant.unwrap_or(DEFAULT_VARIANT)) {
        "NMOS" => Ok(Z80Any::NMOS(Z80 {
            af,
            af_alt,
            regs,
            regs_alt,
            index,
            pc,
            sp,
            memptr,
            last_ei,
            ir,
            im,
            iff1,
            iff2,
            halt,
            prefix,
            r,
            flavour: flavour.unwrap_or_default()
        })),
        "CMOS" => Ok(Z80Any::CMOS(Z80 {
            af,
            af_alt,
            regs,
            regs_alt,
            index,
            pc,
            sp,
            memptr,
            last_ei,
            ir,
            im,
            iff1,
            iff2,
            halt,
            prefix,
            r,
            flavour: CMOS
        })),
        "BM1" => Ok(Z80Any::BM1(Z80 {
            af,
            af_alt,
            regs,
            regs_alt,
            index,
            pc,
            sp,
            memptr,
            last_ei,
            ir,
            im,
            iff1,
            iff2,
            halt,
            prefix,
            r,
            flavour: flavour.map(BM1::from).unwrap_or_default()
        })),
        v => Err(de::Error::unknown_variant(v, &["NMOS", "CMOS", "BM1"]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::z80::*;

    #[test]
    fn z80_serde() {
        let cpu: Z80<CMOS> = Z80::<CMOS>::default();
        let sercpu = serde_json::to_string(&cpu).unwrap();
        assert_eq!(sercpu,
            r#"{"type":"CMOS","af":0,"afAlt":0,"regs":{"bc":0,"de":0,"hl":0},"regsAlt":{"bc":0,"de":0,"hl":0},"index":{"ix":0,"iy":0},"pc":0,"sp":0,"memptr":0,"lastEi":false,"ir":0,"im":"Mode0","iff1":false,"iff2":false,"halt":false,"prefix":null,"r":0,"flavour":{"flagsModified":false,"lastFlagsModified":false}}"#);
        let cpu_de0: Z80<NMOS> = serde_json::from_str(&sercpu).unwrap();
        let cpu_de1: Z80<NMOS> = serde_json::from_reader(&sercpu.into_bytes()[..]).unwrap();

        let bincpu: Vec<u8> = bincode::serialize(&cpu).unwrap();
        let cpu_de2: Z80<NMOS> = bincode::deserialize(&bincpu).unwrap();
        let cpu_de3: Z80<NMOS> = bincode::deserialize_from(&bincpu[..]).unwrap();

        let cpu = cpu.into_flavour::<NMOS>();
        assert_eq!(cpu, cpu_de0);
        assert_eq!(cpu, cpu_de1);
        assert_eq!(cpu, cpu_de2);
        assert_eq!(cpu, cpu_de3);

        let sercpu = r#"{"type":"NMOS","af":0,"af_alt":0,"regs":{"bc":0,"de":0,"hl":0},"regs_alt":{"bc":0,"de":0,"hl":0},"index":{"ix":0,"iy":0},"pc":0,"sp":0,"memptr":0,"last_ei":false,"ir":0,"im":"Mode0","iff1":false,"iff2":false,"halt":false,"prefix":null,"r":0,"flavour":{"flags_modified":false,"last_flags_modified":false}}"#;
        let serlegacy = r#"{"af":0,"afAlt":0,"regs":{"bc":0,"de":0,"hl":0},"regsAlt":{"bc":0,"de":0,"hl":0},"index":{"ix":0,"iy":0},"pc":0,"sp":0,"memptr":0,"lastEi":false,"ir":0,"im":"Mode0","iff1":false,"iff2":false,"halt":false,"prefix":null,"r":0,"flavour":{"flagsModified":false,"lastFlagsModified":false}}"#;
        let sermincpu = r#"{"af":0,"af_alt":0,"regs":{"bc":0,"de":0,"hl":0},"regs_alt":{"bc":0,"de":0,"hl":0},"index":{"ix":0,"iy":0},"pc":0,"sp":0,"last_ei":false,"ir":0,"im":"Mode0","iff1":false,"iff2":false,"halt":false,"prefix":null}"#;
        let cpu_de: Z80<BM1> = serde_json::from_str(sercpu).unwrap();
        let cpu = cpu.into_flavour::<BM1>();
        assert_eq!(cpu, cpu_de);

        let cpu_de: Z80<CMOS> = serde_json::from_str(sercpu).unwrap();
        let cpu = cpu.into_flavour::<CMOS>();
        assert_eq!(cpu, cpu_de);
        let cpu_de: Z80<CMOS> = serde_json::from_str(sermincpu).unwrap();
        assert_eq!(cpu, cpu_de);
        let cpu_de: Z80<CMOS> = serde_json::from_str(serlegacy).unwrap();
        assert_eq!(cpu, cpu_de);

        let cpu_de: Z80<BM1> = serde_json::from_str(sercpu).unwrap();
        let cpu = cpu.into_flavour::<BM1>();
        assert_eq!(cpu, cpu_de);
        let cpu_de: Z80<BM1> = serde_json::from_str(sermincpu).unwrap();
        assert_eq!(cpu, cpu_de);
        let cpu_de: Z80<BM1> = serde_json::from_str(serlegacy).unwrap();
        assert_eq!(cpu, cpu_de);


        let cpu_de: Z80<NMOS> = serde_json::from_str(sercpu).unwrap();
        let cpu = cpu.into_flavour::<NMOS>();
        assert_eq!(cpu, cpu_de);
        let cpu_de: Z80<NMOS> = serde_json::from_str(sermincpu).unwrap();
        assert_eq!(cpu, cpu_de);
        let cpu_de: Z80<NMOS> = serde_json::from_str(serlegacy).unwrap();
        assert_eq!(cpu, cpu_de);

        let cpu_de0: Z80Any = serde_json::from_str(sercpu).unwrap();
        let cpu_de1: Z80Any = serde_json::from_reader(sercpu.as_bytes()).unwrap();
        let cpu = Z80Any::NMOS(cpu);
        assert_eq!(cpu, cpu_de0);
        assert_eq!(cpu, cpu_de1);

        let cpu_de: Z80Any = serde_json::from_str(sermincpu).unwrap();
        assert_eq!(cpu, cpu_de);

        let cpu_de: Z80Any = serde_json::from_str(serlegacy).unwrap();
        assert_eq!(cpu, cpu_de);

        let cpu_de0: Z80Any = bincode::deserialize(&bincpu).unwrap();
        let cpu_de1: Z80Any = bincode::deserialize_from(&bincpu[..]).unwrap();
        let cpu = cpu.into_cmos();
        assert_eq!(cpu, cpu_de0);
        assert_eq!(cpu, cpu_de1);

        let z80any = Z80Any::new_nmos();
        let json = serde_json::to_string(&z80any).unwrap();

        let z80: Z80NMOS = serde_json::from_str(&json).unwrap();
        assert_eq!(Z80Any::NMOS(z80), z80any);

        let z80: Z80CMOS = serde_json::from_str(&json).unwrap();
        assert_eq!(Z80Any::CMOS(z80), z80any.clone().into_cmos());

        let z80: Z80BM1 = serde_json::from_str(&json).unwrap();
        assert_eq!(Z80Any::BM1(z80), z80any.clone().into_bm1());
    }
}
