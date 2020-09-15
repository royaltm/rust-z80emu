# -*- coding: BINARY -*-
#
# The EX-ROM 2 for RAL 1243
#
# This Ruby program requires a gem found at: https://github.com/royaltm/z80-rb.
#
# Author: Rafał Michalski <royaltm75@gmail.com>
#
# SPDX-License-Identifier: BlueOak-1.0.0
# This program is free to use under the terms of the Blue Oak Model License 1.0.0.
# See: https://blueoakcouncil.org/license/1.0.0
require_relative '../rom/ral1243_rom'

class ExRom002
  ::ExRom = self unless defined?(::ExRom)
  include Z80

  VERSION = "1.0.0"

  macro_import MathInt
  label_import RAL_1243_Kernel, labels: 0x0000, macros:true

  vectors               union userrambot, RAL_1243_Kernel::Vectors

  COUNTER_RATIO = 10_000/16

  COUNTER_DONE_BIT     = 1
  COUNTER_DONE_MASK    = 1 << COUNTER_DONE_BIT

  class Vars < Label
    flags             byte
    timer_counter_lo  byte
    timer_counter_hi  byte
    timer_counter     timer_counter_lo word
    workspace         word, 2
    buffer            byte, 5
  end

  vars                  addr userrambot+0x100, Vars

  announce              exrom_announce(exrom_info, +exrom_info, start)

  exrom_info            data "Clock & timer test.\x00" +
                             "This program estimates the CPU frequency using a counter and a timer.\r\n" +
                             "Version: #{VERSION}.\x00"

  macro :print_imm_text do |_, *args|
                        call output_imm_cstr
                        db *args, 0
  end

  ns :start do
                        clrmem8 vars, +vars
                        put_char 0x15 # cursor
                        put_char 0x00 # hide
                        print_imm_text 0x0C,
                        "Estimating CPU clock frequency:\r\n",
                        "Setting 3rd CTC timer/counter to counter mode, assuming 10kHz clock.\r\n",
                        "Setting 4th CTC timer/counter to timer mode and counting ticks.\r\n"
                        ld   h, vectors >> 8
                        call_syslib :move_int_vectors

                        di
                        ld   a, 0b11010111  # enable ints, counter mode, _, raising edge, _, constant follow, reset, control
                        out  (ctc2_port), a
                        ld   a, 0b10011111  # enable ints, timer mode, x16, raising edge, CLK/TRG, constant follow, reset, control
                        out  (ctc3_port), a
                        ld   hl, ctc2_int_handler
                        ld   [vectors.ctc2], hl
                        ld   hl, ctc3_int_handler
                        ld   [vectors.ctc3], hl
                        ld   a, 256
                        out  (ctc3_port), a
                        out  (ctc2_port), a
                        ld   hl, vars.flags

    wait_loop           ei
                        halt
                        di
                        bit  COUNTER_DONE_BIT, [hl]
                        jr   Z, wait_loop
                        ld   a, 0b00000011  # reset & disable ints
                        out  (ctc2_port), a
                        out  (ctc3_port), a
                        ei
                        print_imm_text "After 25.6 ms 4th CTC timer stopped at: "
                        ld   hl, [vars.timer_counter]
                        ld   e, 0
                        call_syslib :output_uint16
                        ld   hl, [vars.timer_counter]
                        ld   bc, COUNTER_RATIO
                        call multiply16_32
                        ld   [vars.workspace[1]], hl
                        exx
                        ld   [vars.workspace[0]], hl
                        print_imm_text ".\r\nThus CPU clock is close to "
                        ld   hl, vars.buffer + +vars.buffer
                        ld   de, vars.workspace
                        ld   b, 4
                        call_syslib :uint_to_bcd
                        call_syslib :output_bcd
                        print_imm_text " Hz.\r\n\n",
                        "Time since reset: \n"

    time_loop           call_syslib_fast :get_frames_counter # frames -> hlde
                        ld   c, 100
                        call divrem32 # hlde = hlde / c, a = hlde % c
                        push af # fraction of a second
                        ld   c, 60
                        call divrem32 # hlde = hlde / c, a = hlde % c
                        push af # seconds
                        call divrem32 # hlde = hlde / c, a = hlde % c
                        push af # minutes
                        ld   c, 24
                        call divrem16 # de = de / c, a = de % c
                        push af # hours
                        ld   a, "\r".ord
                        ora  a
                        rst  0x10
                        ex   de, hl # hl: days
                        ld   e, 0
                        call_syslib_fast :output_uint16
                        print_imm_text " days, "
                      %w[: : .].each do |sep|
                        pop  af # hours, minutes, seconds
                        call output_uint00
                        ld   a, sep.ord
                        rst  0x10
                      end
                        pop  af # fractions
                        call output_uint00
                        halt
                        xor  a
                        rst  0x08
                        jr   Z, time_loop
                        jp   start
  end

  # the first countdown event starts ctc3, the second one takes measure
  ns :ctc2_int_handler do
                        ex   af, af
                        inp  a, (ctc3_port)
                        neg
                        sub  2
                        push hl
                        ld   hl, vars.flags
                        inc  [hl]
                        inc  hl
                        ld   [hl], a
                        pop  hl
                        ei
                        ex   af, af
                        reti
  end

  # increase timer counter msb, reset lsb
  ns :ctc3_int_handler do
                        ei       # allow ctc2 interrupts early
                        push hl
                        ld   hl, vars.timer_counter_lo
                        ld   [hl], 0
                        inc  hl
                        push af
                        inc  [hl]
                        pop  af
                        pop  hl
                        reti
  end

  # text immediately after call instruction, 0 terminated
  ns :output_imm_cstr do
                        pop  hl
                        jr   next_char
    char_loop           rst  0x10
                        inc  hl
    next_char           ld   a, [hl]
                        ora  a
                        jr   NZ, char_loop
                        jp   (hl)
  end

  # a: an integer to print as decimal: 0-99
  ns :output_uint00 do
                        ld   e, '0'.ord
                        ld   bc, (0<<8)|(-10 & 0xFF)
    add_loop            add  a, c
                        inc  b
                        jr   C, add_loop
                        sub  c
                        dec  b
                        ld   d, a
                        ld   a, b
                        add  a, e
                        rst  0x10
                        ld   a, d
                        add  a, e
                        jp   0x10
  end

  # HLHL' = HL * BC
  ns :multiply16_32 do
                        mul16_32 bc, tt:bc, clrhlhl:true, signed_hl:false, optimize: :time
                        ret
  end

  # divides HLDE / C, remainder in A, no zero check
  ns :divrem32 do
                        divmod h, c, check0:false, check1:false, modulo:false, optimize: :time
                        divmod l, c, clrrem:false, modulo:false, optimize: :time
                        divmod d, c, clrrem:false, modulo:false, optimize: :time
                        divmod e, c, clrrem:false, modulo:false, optimize: :time
                        ret
  end

  # divides DE / C, remainder in A, no zero check
  ns :divrem16 do
                        divmod d, c, check0:false, check1:false, modulo:false, optimize: :time
                        divmod e, c, clrrem:false, modulo:false, optimize: :time
                        ret
  end
end

if __FILE__ == $0
  exrom = ExRom.new RAL_1243_Kernel.exrombot.to_i
  puts exrom.debug
  puts "  exrom size: #{exrom.code.bytesize}"
  File.open(File.join(__dir__, "#{exrom.class.name.downcase}.bin"), 'wb') {|f| f.write exrom.code }
end
