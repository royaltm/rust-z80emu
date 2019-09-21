//! This module contains macros that dispacthes the op-codes into menmonics and delegates the debugger
//! and execution to the instructions::instr! macro via instruction_dispatch!.
#![macro_use]
macro_rules! match_instruction {
    (@none [$code:expr]; $maybe_prefix:ident; break $main:tt) => { instruction_dispatch! {
        match ($code) {
            0x40..=0x7F => { LD r,r | LD (HL),r | LD r,(HL) | HALT break $main } // 0b01_rrr_rrr
            0x80..=0xBF => {            @ops A, r | @ops A, (HL)               } // 0b10_ops_rrr
            0xCB => {                   @rot|BIT|RES|SET r|(HL)                }
            0xDD => {                   $maybe_prefix = Some(Prefix::Xdd);     }
            0xFD => {                   $maybe_prefix = Some(Prefix::Yfd);     }
            0xED => {
                {
                    let code = fetch_next_opcode!();
                    match_instruction!(ED [code]; break $main)
                };
            }
            0x00 => {                   NOP                                    }
            0x27 => {                   DAA                                    }
            0x2F => {                   CPL                                    }
            0x3F => {                   CCF                                    }
            0x37 => {                   SCF                                    }
            0xF3 => {                   DI                                     }
            0xFB => {                   EI break $main                         }
            0x08 => {                   EX   AF, AF^                           }
            0xEB => {                   EX   DE, HL                            }
            0xD9 => {                   EXX                                    }
            0x06|0x0E|0x16|0x1E|
            0x26|0x2E|0x36|0x3E => {    LD   r,n | LD (HL),n                   } // 0b00_rrr_110
            0x0A => {                   LD   A, (BC)                           }
            0x1A => {                   LD   A, (DE)                           }
            0x3A => {                   LD   A, (nn)                           }
            0x02 => {                   LD   (BC), A                           }
            0x12 => {                   LD   (DE), A                           }
            0x32 => {                   LD   (nn), A                           }
            0x01|0x11|0x21|0x31 => {    LD   dd,nn                             } // 0b00_dd_0001
            0x22 => {                   LD   (nn),HL                           }
            0x2A => {                   LD   HL,(nn)                           }
            0xF9 => {                   LD   SP, HL                            }
            0xC5|0xD5|0xE5|0xF5 => {    PUSH ss                                } // 0b11_ss_0101
            0xC1|0xD1|0xE1|0xF1 => {    POP  ss                                } // 0b11_ss_0001
            0xE3 => {                   EX   (SP), HL                          }
            0xC6|0xCE|0xD6|0xDE|
            0xE6|0xEE|0xF6|0xFE => {    @ops A, n                              } // 0b11_ops_110
            0x04|0x0C|0x14|0x1C|
            0x24|0x2C|0x34|0x3C => {    INC  r | INC (HL)                      } // 0b00_rrr_100
            0x05|0x0D|0x15|0x1D|
            0x25|0x2D|0x35|0x3D => {    DEC  r | DEC (HL)                      } // 0b00_rrr_100
            0x09|0x19|0x29|0x39 => {    ADD  HL, dd                            } // 0b00_dd_1001
            0x03|0x13|0x23|0x33 => {    INC  dd                                } // 0b00_dd_0011
            0x0B|0x1B|0x2B|0x3B => {    DEC  dd                                } // 0b00_dd_1011
            0x07 => {                   RLCA                                   }
            0x17 => {                   RLA                                    }
            0x0F => {                   RRCA                                   }
            0x1F => {                   RRA                                    }
            0xC3 => {                   JP   nn                                }
            0xC2|0xCA|0xD2|0xDA|
            0xE2|0xEA|0xF2|0xFA => {    JP   cc, nn                            } // 0b11_ccc_010
            0xE9 => {                   JP   (HL)                              }
            0x18 => {                   JR   e                                 }
            0x38|0x30|0x28|0x20 => {    JR C,e | JR NC, e | JR Z,e | JR NZ,e   }
            0x10 => {                   DJNZ e                                 }
            0xCD => {                   CALL nn                                }
            0xC4|0xCC|0xD4|0xDC|
            0xE4|0xEC|0xF4|0xFC => {    CALL cc, nn                            } // 0b11_ccc_100
            0xC9                => {    RET                                    }
            0xC0|0xC8|0xD0|0xD8|
            0xE0|0xE8|0xF0|0xF8 => {    RET  cc                                } // 0b11_ccc_000
            0xC7|0xCF|0xD7|0xDF|
            0xE7|0xEF|0xF7|0xFF => {    RST  p                                 }
            0xDB => {                   IN   A, (n)                            }
            0xD3 => {                   OUT  (n), A | break $main              }
        }
    }};

    (DD|FD [$code:expr]; $maybe_prefix:ident; $repeat:tt) => { instruction_dispatch! {
        match ($code) {
            0x00..=0x08|0x0A..=0x18|0x1A..=0x20|0x27|0x28|0x2F..=0x33|0x37|0x38|0x3A..=0x43|
            0x47..=0x4B|0x4F..=0x53|0x57..=0x5B|0x5F|0x76|0x78..=0x7B|
            0x7F..=0x83|0x87..=0x8B|0x8F..=0x93|0x97..=0x9B|
            0x9F..=0xA3|0xA7..=0xAB|0xAF..=0xB3|0xB7..=0xBB|
            0xBF..=0xCA|0xCC..=0xDC|0xDE..=0xE0|0xE2|0xE4|0xE6..=0xE8|0xEA..=0xF8|0xFA..=0xFC|
            0xFE|0xFF => {             { $maybe_prefix = None; continue $repeat };}
            0x44..=0x46|0x4C..=0x4E|
            0x54..=0x56|0x5C..=0x5E|
            0x60..=0x75|0x77|
            0x7C..=0x7E => {               LD q,q | LD (ii+d),r | LD r,(ii+d)     } // 0b01_rrr_rrr
            0x84..=0x86|0x8C..=0x8E|
            0x94..=0x96|0x9C..=0x9E|
            0xA4..=0xA6|0xAC..=0xAE|
            0xB4..=0xB6|0xBC..=0xBE => {   @ops A, q | @ops A, (ii+d)             } // 0b10_ari_rrr
            0xCB => {                      @rot|BIT|RES|SET (ii+d)                }
            0x26|0x2E => {                 LD   q, n                              } // 0b00_rrr_110
            0x36 => {                      LD   (ii+d), n                         } // 0b00_rrr_110
            0x21 => {                      LD   ii, nn                            }
            0x22 => {                      LD   (nn), ii                          }
            0x2A => {                      LD   ii, (nn)                          }
            0xF9 => {                      LD   SP, ii                            }
            0xE5 => {                      PUSH ii                                }
            0xE1 => {                      POP  ii                                }
            0xE3 => {                      EX   (SP), ii                          }
            0x24|0x2C|0x34 => {            INC  q | INC (ii+d)                    } // 0b00_rrr_100
            0x25|0x2D|0x35 => {            DEC  q | DEC (ii+d)                    } // 0b00_rrr_100
            0x09|0x19|0x29|0x39 => {       ADD  ii, dd                            } // 0b00_dd_1001
            0x23 => {                      INC  ii                                }
            0x2B => {                      DEC  ii                                }
            0xE9 => {                      JP   (ii)                              }
            0xDD => {        { $maybe_prefix = Some(Prefix::Xdd); break $repeat };}
            0xFD => {        { $maybe_prefix = Some(Prefix::Yfd); break $repeat };}
        }
    }};

    (ED [$code:expr]; break $main:tt) => { instruction_dispatch! {
        match (0xED, $code) {
            0x00..=0x3F|0x7F..=0x9F|0xA4..=0xA7|0xAC..=0xAF|0xB4..=0xB7|0xBC..=0xFF|
            0x77 => {                   NOP *                           }
            0x44|0x4C|0x54|0x5C|
            0x64|0x6C|0x74|0x7C => {    NEG                             }
            0x46|0x4E|0x66|0x6E => {    IM 0                            }
            0x56|0x76 => {              IM 1                            }
            0x5E|0x7E => {              IM 2                            }
            0x4B|0x5B|0x6B|0x7B => {    LD   dd,(nn)                    } // 0b01_dd_1011
            0x43|0x53|0x63|0x73 => {    LD   (nn),dd                    } // 0b01_dd_0011
            0x57 => {                   LD   A, I                       }
            0x5F => {                   LD   A, R                       }
            0x47 => {                   LD   I, A                       }
            0x4F => {                   LD   R, A                       }
            0x4A|0x5A|0x6A|0x7A => {    ADC  HL, dd                     } // 0b01_ss_1010
            0x42|0x52|0x62|0x72 => {    SBC  HL, dd                     } // 0b01_ss_0010
            0x6F => {                   RLD                             }
            0x67 => {                   RRD                             }
            0x45|0x4D|0x55|0x5D|
            0x65|0x6D|0x75|0x7D => {    RETN | RETI | break $main       }
            0xA0 => {                   LDI                             }
            0xA8 => {                   LDD                             }
            0xB0 => {                   LDIR                            }
            0xB8 => {                   LDDR                            }
            0xA1 => {                   CPI                             }
            0xA9 => {                   CPD                             }
            0xB1 => {                   CPIR                            }
            0xB9 => {                   CPDR                            }
            0xA2 => {                   INI                             }
            0xAA => {                   IND                             }
            0xB2 => {                   INIR                            }
            0xBA => {                   INDR                            }
            0xA3 => {                   OUTI | break $main              }
            0xAB => {                   OUTD | break $main              }
            0xB3 => {                   OTIR | break $main              }
            0xBB => {                   OTDR | break $main              }
            0x40|0x48|0x50|0x58|
            0x60|0x68|0x70|0x78 => {    IN  r, (C)                      } // 0b01_rrr_000
            0x41|0x49|0x51|0x59|
            0x61|0x69|0x71|0x79 => {    OUT (C),r | break $main         } // 0b01_rrr_001
        }
    }};
}

/// Takes the byte in $code and according to the $maybe_prefix dispatches the op-codes via opcodes::match_instruction!.
macro_rules! execute_instruction {
    ([$code:expr]
    $deb:expr; $maybe_prefix:ident, $flags:ident, $pc:ident, $cpu:ident, $control:ident, $tsc:ident; break $main:tt) => {{

        define_cpu_debug_scoped!([$] $deb; $maybe_prefix, $flags, $pc, $cpu, $control, $tsc);
        define_helpers_scoped!([$] $flags, $pc, $cpu, $control, $tsc);

        'repeat: loop {
            match $maybe_prefix {
                None => {
                    define_instructions_scoped!($flags, $pc, $cpu, $control, $tsc);
                    match_instruction!(@none [$code]; $maybe_prefix; break $main);
                }
                Some(prefix) => {
                    define_instructions_scoped!($flags, $pc, $cpu, $control, $tsc, prefix);
                    match_instruction!(DD|FD [$code]; $maybe_prefix; 'repeat);
                    $maybe_prefix = None;
                }
            }
            break 'repeat;
        }
    }};
}

/// A sugar for reading next op-code and executing the instruction via execute_instruction!.
macro_rules! execute_next_instruction {
    ($deb:expr; $maybe_prefix:ident, $flags:ident, $pc:ident, $cpu:ident, $control:ident, $tsc:ident; break $main:tt) => {
        {
            let code = fetch_next_opcode_ext!($cpu, $control, $pc, $tsc);
            execute_instruction!([code] $deb; $maybe_prefix, $flags, $pc, $cpu, $control, $tsc; break $main)
        }
    };
}

/// Used by the match_instruction! macro to indirectly invoke the instructions::instr! macro.
macro_rules! instruction_dispatch {
    (match ($code:expr) {
            $($($mat:pat)|* => {$($mnem:tt)*})*
        }
    ) => {
        match $code {
           $($($mat)|* => run_mnemonic!{ $($mnem)* @@@ $code }),*
        }
    };

    (match ($code0:expr, $code1:expr) {
            $($($mat:pat)|* => {$($mnem:tt)*})*
        }
    ) => {
        match $code1 {
           $($($mat)|* => run_mnemonic!{ $($mnem)* @@@ $code0, $code1 }),*
        }
    };
}
