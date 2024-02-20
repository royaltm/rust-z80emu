# -*- coding: BINARY -*-
#
# The ROM Kernel of RAL 1243
#
# This Ruby program requires a gem found at: https://github.com/royaltm/z80-rb.
#
# Author: Rafał Michalski <royaltm75@gmail.com>
#
# SPDX-License-Identifier: BlueOak-1.0.0
# This program is free to use under the terms of the Blue Oak Model License 1.0.0.
# See: https://blueoakcouncil.org/license/1.0.0
require 'z80'
require 'z80/math_i'
require 'z80/stdlib'

class RAL_1243_Kernel
  include Z80

  VERSION = "1.0.1"

  def self.swap_bytes(word)
    hi, lo = (word >> 8)&0xFF, word&0xFF
    (lo << 8) | hi
  end

  macro_import MathInt
  macro_import Stdlib

  class Vectors < Label
    ctc0                word
    ctc1                word
    ctc2                word
    ctc3                word
    pio_inp             word
    pio_out             word
  end

  export syslib

  export :auto

  memory_port           addr  124
  pio_a_data            addr  8
  pio_a_ctrl            addr  9
  pio_b_data            addr  10
  pio_b_ctrl            addr  11
  ctc0_port             addr  4
  ctc1_port             addr  5
  ctc2_port             addr  6
  ctc3_port             addr  7

  # A new character is available on the input BUS but hasn't been received
  IO_FLAG_NEW_INPUT_BIT     = 0
  IO_FLAG_NEW_INPUT_MASK    = 1 << IO_FLAG_NEW_INPUT_BIT
  # A new character is ready in the input buffer
  IO_FLAG_INPUT_READY_BIT   = 1
  IO_FLAG_INPUT_READY_MASK  = 1 << IO_FLAG_INPUT_READY_BIT
  # A new character is waiting for flushing out in the output buffer
  IO_FLAG_OUTPUT_READY_BIT  = 2
  IO_FLAG_OUTPUT_READY_MASK = 1 << IO_FLAG_OUTPUT_READY_BIT
  # An output BUS is not ready to receive a character yet
  IO_FLAG_OUTPUT_FULL_BIT   = 3
  IO_FLAG_OUTPUT_FULL_MASK  = 1 << IO_FLAG_OUTPUT_FULL_BIT
  # Setting this flag signals RST 38h handler that kernel was testing the EX-ROM.
  FLAGS_EX_ROM_TEST_BIT     = 0
  FLAGS_EX_ROM_TEST_MASK    = 1 << FLAGS_EX_ROM_TEST_BIT
  # Setting this flag signals NMI handler to handle the request.
  FLAGS_ALLOW_NMI_BIT       = 1
  FLAGS_ALLOW_NMI_MASK      = 1 << FLAGS_ALLOW_NMI_BIT

  # A struct representing system variables, visible only to ROM kernel.
  class SystemVars < Label
    frames0             byte
    frames1             byte
    frames2             byte
    frames3             byte
    char_in             byte
    io_flags            byte
    char_out            byte
    flags               byte
    ramtop              word
    tempword            word
  end

  exrombot              addr 0x2000
  userrambot            addr 0x4000

  export :noauto

  sysvars               addr 0x2000, SystemVars
  sysramsize            addr 0x2000

  ###########################################################################
  #                              M A C R O S                                #
  ###########################################################################

  macro :test_memory do |eoc, start, size|
                        ld   a, 0b01010101
    loop_main           ld   hl, start
                        ld   bc, swap_bytes(size)
    loop_iter           ld   [hl], a
                        cp   [hl]
                        jr   NZ, eoc
                        inc  hl
                        djnz loop_iter
                        dec  c
                        jr   NZ, loop_iter
                        ora  a
                        jr   Z, eoc
                        add  a, a
                        jr   loop_main
  end

  macro :setup_pio do |_, vectors|
                select(vectors & 0x00FF, &:zero?).then do |_|
                        ld   a, 0b01001111
                        out  (pio_a_ctrl), a # set INPUT MODE
                        ld   a, vectors.pio_inp
                        out  (pio_a_ctrl), a # vector
                        ld   a, 0b10000011
                        out  (pio_a_ctrl), a # enable interrupts
                        ld   a, 0b00001111
                        out  (pio_b_ctrl), a # set OUTPUT MODE
                        ld   a, vectors.pio_out
                        out  (pio_b_ctrl), a # vector
                        ld   a, 0b10000011
                        out  (pio_b_ctrl), a # enable interrupts
                end.else do
                    raise ArgumentError, "vectors address must be aligned to 256 bytes"
                end
  end

  macro :setup_ctc do |_, vectors|
                select(vectors & 0x00FF, &:zero?).then do |_|
                        ld   a, vectors.ctc0
                        out  (ctc0_port), a # vector
                        ld   a, 0b11010111  # enable ints, counter mode, _, raising edge, _, constant follow, reset, control
                        out  (ctc0_port), a
                        ld   a, 100         # 10000 HZ / 100 = 100 HZ counter
                        out  (ctc0_port), a
                        ld   a, 0b00000011  # reset & disable ints
                        out  (ctc1_port), a
                        out  (ctc2_port), a
                        out  (ctc3_port), a
                end.else do
                    raise ArgumentError, "vectors address must be aligned to 256 bytes"
                end
  end

  macro :put_imm_text do |_, *args|
                        call output_imm_cstr
                        db *args, 0
  end

  macro :put_char_repeat do |_, char, repeat|
                    char = char.ord if char.is_a?(String)
                    if address?(char) && address?(repeat)
                        ld   bc, (repeat << 8) | char&0xFF
                        call output_char_repeat.no_exx
                    elsif char == c
                        ld   b, repeat unless repeat == b
                        call output_char_repeat.no_exx
                    else
                        ld   a, char unless char == a
                        ld   b, repeat unless repeat == b
                        call output_char_repeat.char_in_a
                    end
  end

  macro :put_uint16 do |_, value, pad:0|
                        ld   hl, value unless value == hl
                        pad = pad.ord if pad.is_a?(String)
                        ld   e, pad
                        call output_uint16.no_exx
  end

  macro :put_char do |_, char|
                        char = char.ord if char.is_a?(String)
                        ld   a, char unless char == a
                        ora  a
                        rst  0x10
  end

  macro :get_char do |_|
                        scf
                        rst  0x08
  end

  macro :pause_ticks do |_, ticks|
                        ld   bc, ticks unless ticks == bc
                        call wait_ticks
  end

  macro :wait_char_timeout do |_, ticks|
                        ld   bc, ticks unless ticks == bc
                        call input_wait_timeout
  end

  macro :put_text_tok do |_, text, tok:0|
                        ld   hl, text unless text == hl
                        ld   e, tok unless tok == e
                        call output_str_tok.no_exx
  end

  ###########################################################################
  #                              K E R N E L                                #
  ###########################################################################
                        org  0x0000
  # soft/hard reset
  ns :rst00 do
                        di
                        ld   a, 1
                        ld   i, a
                        jp   reset_continue
  end

                        org  0x0008
  # input character CF=1 wait, CF=0 don't wait, on return ZF=0 success and A=char, ZF=1 no new character
  ns :rst08 do
                        exx
                        ld   hl, sysvars.io_flags
                        jp   char_input_continue
  end
  stop                  halt

                        org  0x0010
  # output character in A, CF=0 wait, CF=1 don't wait, on return ZF=1 success, ZF=0 full
  ns :rst10 do
                        exx
                        ld   hl, sysvars.io_flags
                        jp   char_output_continue
  end

                        org  0x0018
  ns :rst18 do
                        jp   (ix)
  end

                        org  0x0020
  ns :rst20 do
                        jp   (iy)
  end

                        org  0x0028
  # call a syslib function
  ns :rst28 do
                        exx
                select(syslib & 0x00FF, &:zero?).then do |_|
                        ld   h, syslib >> 8
                        ld   l, a
                        ld   a, [hl]
                        inc  l
                        ld   h, [hl]
                        ld   l, a
                end.else do
                    raise ArgumentError, "syslib address must be aligned to 256 bytes"
                end
  end

                        org  0x0030
  # jump to an address in memory at HL
  ns :rst30 do
                        jp   (hl)
  end

                        org  0x0038
  # back to menu or just return if EX_ROM_TEST
  ns :rst38 do
                        pop  af # discard 1st return address
                        ld   a, [sysvars.flags]
                        anda FLAGS_EX_ROM_TEST_MASK
                        ret  NZ
    # PATCH 1.0.1 BEGIN (set IFF2 = 0 from NMI)
    back_to_menu        di
                        ld   a, 1
    # PATCH 1.0.1 END
                        ld   i, a
                        ld   hl, [sysvars.ramtop]
                        inc  hl
                        ld   sp, hl
                        call reset_peripherals
    # PATCH 1.0.1 BEGIN
                        xor  a # clear IO/flags
                        ld   [sysvars.io_flags], a
                        call clear_ieo # a possible CTC/IEO
                        call clear_ieo # a possible PIO/IEO
    # PATCH 1.0.1 END
                        ei
                        jp   ex_rom_menu
  end

                        org  0x0066

  # check if NMI occured while in menu, if yes, just return
  # otherwise abort execution and restart from menu
  ns :nmi do
                        push hl
                        push af
                        ld   hl, sysvars.flags
                        bit  FLAGS_ALLOW_NMI_BIT, [hl]
                        jr   Z, back_to_rom
                        res  FLAGS_EX_ROM_TEST_BIT, [hl]
                        jr   rst38.back_to_menu
    back_to_rom         pop  af
                        pop  hl
                        retn
  end

  ###########################################################################
  #                              INTERRUPTS                                 #
  ###########################################################################

                        org  0x0100
  vectors               data Vectors, {
                          ctc0: ctc0_interrupt,
                          ctc1: none_int_handler,
                          ctc2: none_int_handler,
                          ctc3: none_int_handler,
                          pio_inp: char_input_int_handler,
                          pio_out: char_output_int_handler
                        }

  none_int_handler      ei
  # PATCH 1.0.1 BEGIN
  clear_ieo             reti
  # PATCH 1.0.1 END

  with_saved :ctc0_interrupt, af, hl, ret: :ei_reti do |eoc|
                        ld   hl, sysvars.frames0
                        inc  [hl]
                      3.times do
                        jr   NZ, eoc
                        inc  hl
                        inc  [hl]
                      end
  end

  ns :char_input_int_handler do
                        push af
                        push hl
                        ld   hl, sysvars.io_flags
                        bit  IO_FLAG_INPUT_READY_BIT, [hl]
                        jr   NZ, not_ready
                        inp  a, (pio_a_data)
                        set  IO_FLAG_INPUT_READY_BIT, [hl]
                        dec  hl
                        ld   [hl], a
    exit_handler        ei
                        pop  hl
                        pop  af
                        reti
    not_ready           set  IO_FLAG_NEW_INPUT_BIT, [hl]
                        jr   exit_handler
  end

  ns :char_input_continue do
                        di
                        bit  IO_FLAG_INPUT_READY_BIT, [hl]
                        jr   Z, no_new_char
                        bit  IO_FLAG_NEW_INPUT_BIT, [hl]
                        jr   NZ, new_input
                        res  IO_FLAG_INPUT_READY_BIT, [hl]
                        dec  hl
                        ora  h  # ZF=0
                        ld   a, [hl]
    exit_after_ei       ei
                        exx
                        ret
    no_new_char         jr   NC, exit_after_ei
                        ei
                        halt
                        jr   char_input_continue
    new_input           res  IO_FLAG_NEW_INPUT_BIT, [hl]
                        dec  hl
                        ld   a, [hl]
                        push af
                        inp  a, (pio_a_data)
                        ld   [hl], a
                        pop  af
                        jr   exit_after_ei
  end

  ns :char_output_int_handler do
                        push af
                        push hl
                        ld   hl, sysvars.io_flags
                        bit  IO_FLAG_OUTPUT_READY_BIT, [hl]
                        jr   Z, not_ready
                        res  IO_FLAG_OUTPUT_READY_BIT, [hl]
                        inc  hl
                        ld   a, [hl]
                        out  (pio_b_data), a
    exit_handler        ei
                        pop  hl
                        pop  af
                        reti
    not_ready           res  IO_FLAG_OUTPUT_FULL_BIT, [hl]
                        jr   exit_handler
  end

  ns :char_output_continue do
                        di
                        bit  IO_FLAG_OUTPUT_READY_BIT, [hl]
                        jr   NZ, buffer_full
                        bit  IO_FLAG_OUTPUT_FULL_BIT, [hl]
                        jr   Z, flush_immediately
                        set  IO_FLAG_OUTPUT_READY_BIT, [hl]
                        inc  hl
                        ld   [hl], a
    exit_set_zf_ei      cp   a  # ZF=1
    exit_no_wait        ei
                        exx
                        ret
    buffer_full         jr   C, exit_no_wait
                        ei
                        halt
                        jr   char_output_continue
    flush_immediately   set  IO_FLAG_OUTPUT_FULL_BIT, [hl]
                        out  (pio_b_data), a
                        jr   exit_set_zf_ei
  end

  ###########################################################################
  #                              S Y S L I B                                #
  ###########################################################################

  # wait BC ticks (tick = 1/100 sec)
  ns :wait_ticks do
                        exx
    no_exx              ld   hl, sysvars.frames0
                        di
    count_loop          ld   a, [hl]
    wait_loop           ei
                        halt
                        di
                        cp   [hl]
                        jr   Z, wait_loop
                        exx
                        dec  bc
                        ld   a, c
                        ora  b
                        exx
                        jr   NZ, count_loop
                        ei
                        exx
                        ret
  end

  # prunes any buffered input in the memory or waiting on the PIO's bus
  ns :input_prune do
                        exx
    no_exx              ora  a
                        rst  0x08
                        ret  Z
                        jr   input_prune
  end

  # peeks an input character if new without emptying the buffer, signals success with ZF=0
  # the new peeked character will be returned in A
  # when calling with CF=1 waits for new character and always succeeds
  ns :input_peek do
                        exx
    no_exx              ld   hl, sysvars.io_flags
    wait_loop           di
                        bit  IO_FLAG_INPUT_READY_BIT, [hl]
                        jr   Z, no_new_char
                        dec  hl
                        ld   a, [hl]
    exit_ei_exx         ei
                        exx
                        ret
    no_new_char         jr   NC, exit_ei_exx
                        ei
                        halt
                        jr   wait_loop
  end

  # wait max BC ticks (tick = 1/100 sec), signals new char with ZF=0, timeout if ZF=1
  ns :input_wait_timeout do
                        exx
    no_exx              ld   hl, sysvars.frames0
                        di
    wait_loop           ld   a, [sysvars.io_flags]
                        anda IO_FLAG_INPUT_READY_MASK
                        jr   NZ, exit_ei_exx
                        ld   a, [hl]
                        ei
                        halt
                        di
                        cp   [hl]
                        jr   Z, wait_loop
                        exx
                        dec  bc
                        ld   a, c
                        ora  b
                        exx
                        jr   NZ, wait_loop
    exit_ei_exx         ei
                        exx
                        ret
  end

  # waits until output buffer is being flushed
  ns :output_flush do
                        exx
      no_exx            label
      try_again         di
                        ld   a, [sysvars.io_flags]
                        anda IO_FLAG_OUTPUT_READY_MASK
                        jr   Z, exit_ei
                        ei
                        halt
                        jr   try_again
    exit_ei             ei
                        ret
  end

  # c: character to print, b: repeat count
  ns :output_char_repeat do
                        exx
    no_exx              ld   a, c
    char_in_a           label
    repeat_loop         ora  a
                        rst  0x10
                        djnz repeat_loop
                        ret
  end

  # hl: string to output, e: stop token
  ns :output_str_tok do
                        exx
    no_exx              jr   next_char
    char_loop           ora  a
                        rst  0x10
                        inc  hl
    next_char           ld   a, [hl]
                        cp   e
                        jr   NZ, char_loop
                        ret
  end

  # hl: string to output, bc: length
  ns :output_string do
                        exx
    no_exx              label
    string_loop         ld   a, c
                        ora  b
                        ret  Z
                        ld   a, [hl]
                        rst  0x10
                        dec  bc
                        inc  hl
                        jr   string_loop
  end

  # d: an integer to print as decimal, e: padding character, e: 0 no padding
  ns :output_uint8 do
                        exx
    no_exx              ld   c, -100
                        call calculate_digit_output
                        ld   c, -10
                        call calculate_digit_output
                        ld   a, d
                        jr   calculate_digit_output.output_n
    ns :calculate_digit_output do
                        ld   b, 0
                        ld   a, d
      add_loop          add  a, c
                        inc  b
                        jr   C, add_loop
                        sub  c
                        dec  b
                        ld   d, a
                        ld   a, b
                        jr   Z, check_padding
      output_n          ld   e, '0'.ord
                        add  a, e
                        jp   0x10
      check_padding     ora  e
                        ret  Z
                        jp   0x10
    end
  end

  # hl: an integer to print as decimal, e: padding character, e: 0 no padding
  ns :output_uint16 do
                        exx
    no_exx              ld   bc, -10000
                        call calculate_digit_output
                        ld   bc, -1000
                        call calculate_digit_output
                        ld   bc, -100
                        call calculate_digit_output
                        ld   bc, -10
                        call calculate_digit_output
                        ld   a, l
                        jr   calculate_digit_output.output_n
    ns :calculate_digit_output do
                        xor  a
      add_loop          add  hl, bc
                        inc  a
                        jr   C, add_loop
                        sbc  hl, bc
                        dec  a
                        jr   Z, check_padding
      output_n          ld   e, '0'.ord
                        add  a, e
                        jp   0x10
      check_padding     ora  e
                        ret  Z
                        jp   0x10
    end
  end

  # print a BCD string
  # HL an address of the first output byte
  # C a number of bytes in the BCD buffer
  ns :output_bcd do
                        exx
    no_exx              label
                        bcdtoa hl, c, skip_leading0: true do
                          add ?0.ord
                          rst 0x10
                        end
                        ret
  end

  # converts an unsigned integer of arbitrary size to a BCD string
  # HL an address of the first byte immediately following an end of the BCD buffer
  # DE an address of the integer
  # B integer size in bytes
  # result: HL contains an address of the first output byte, C contains a number of bytes written
  ns :uint_to_bcd do
                        exx
    no_exx              ld   [sysvars.tempword], hl
                        utobcd([sysvars.tempword], de, size: b, r: d, rr: de, byteorder: :lsb)
                        exx
                        ret
  end

  # moves interrupt vector table to the RAM page indicated in H
  # H must be >= 0x04
  ns :move_int_vectors do
                        exx
    no_exx              ld   a, h
                        ex   de, hl
                        ld   hl, vectors
                        ld   e, l
                        ld   bc, +vectors
                        ldir
                        ld   i, a
                        ret
  end

  # returns the current frames counter as a 32-bit integer in HL (MSB) and DE (LSB)
  ns :get_frames_counter do
                        exx
    no_exx              di
                        ld   de, [sysvars.frames0]
                        ld   hl, [sysvars.frames2]
                        ei
                        ret
  end

                        org  0x300
  # System library vector jump table used by RST 28.
  ns :syslib do
    wait_ticks_v          dw   wait_ticks.no_exx
    input_prune_v         dw   input_prune
    input_peek_v          dw   input_peek.no_exx
    input_wait_timeout_v  dw   input_wait_timeout.no_exx
    output_flush_v        dw   output_flush
    output_char_repeat_v  dw   output_char_repeat
    output_str_tok_v      dw   output_str_tok
    output_string_v       dw   output_string
    output_uint8_v        dw   output_uint8
    output_uint16_v       dw   output_uint16
    output_bcd_v          dw   output_bcd
    uint_to_bcd_v         dw   uint_to_bcd
    move_int_vectors_v    dw   move_int_vectors
    get_frames_counter_v  dw   get_frames_counter
  end

                        org  0x400
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

  ###########################################################################
  #                               R E S E T                                 #
  ###########################################################################

  ns :reset_peripherals do
                        im   2
                        setup_ctc vectors
                        setup_pio vectors
                        ret
  end

  ns :reset_continue do
                        test_memory sysvars, sysramsize # tests and clears system ram
                        jp   NZ, stop                   # fatal error no system ram
                        ld   sp, hl                     # set up temporary stack at the end of the system ram
                        out  (memory_port), a           # select ex-rom page 0
                        call reset_peripherals
                        ei
  end

  ns :say_hello_test_user_ram do
                        put_char 0x15 # cursor
                        put_char 0x00 # hide
                        put_imm_text 0x0C, # clear terminal
                          "RAL 1243 (c) All rights reserved j.k. 2019-2024\r\nKernel version: #{VERSION}.\r\n\n",
                          "Testing upper RAM: "
                        ld   hl, userrambot
    test_ram_loop       ld   [sysvars.ramtop], hl
                        ld   bc, userrambot
                        sbc  hl, bc
                        put_uint16 hl, pad: '0'
                        put_char_repeat 8, 5
                        test_memory [sysvars.ramtop], 1024
                        jr   Z, test_ram_loop
    test_over           ld   sp, hl
                        dec  hl
                        ld   [sysvars.ramtop], hl
                        xor  a
                        ld   bc, userrambot - 1
                        sbc  hl, bc
                        jr   NZ, ramok
                        put_imm_text "No user RAM detected.\r\n"
                        call output_flush.no_exx
                        di
                        halt
    ramok               put_uint16 hl, pad: '0'
                        put_imm_text " bytes.\r\n\r\n"
                        call output_flush.no_exx
                        pause_ticks 50
                        put_imm_text "Ready.\r\n"
                        call output_flush.no_exx
                        call input_prune.no_exx
                        wait_char_timeout 350
  end

  ###########################################################################
  #                                M E N U                                  #
  ###########################################################################

  ns :ex_rom_menu do
                        ld   hl, sysvars.flags
                        res  FLAGS_ALLOW_NMI_BIT, [hl]
                        set  FLAGS_EX_ROM_TEST_BIT, [hl]
                        put_char 0x15 # cursor
                        put_char 0x00 # hide
                        put_imm_text 0x0C, "RAL 1243 EX-ROM MENU:\r\n"
                        xor  a
    menu_loop           out  (memory_port), a
                        call exrombot
                        jr   NZ, skip_exrom
                        inp  a, (memory_port)
                        ld   l, a
                        ld   h, 0
                        put_uint16 hl
                        put_imm_text ". "
                        put_text_tok userrambot
                        put_imm_text "\r\n"
    skip_exrom          inp  a, (memory_port)
                        inc  a
                        jr   NZ, menu_loop
                        ld   hl, sysvars.flags
                        res  FLAGS_EX_ROM_TEST_BIT, [hl]

    prompt_again        put_imm_text "\rselect ex-rom>    \b\b\b", 0x15,0x01
                        xor  a
                        ex   af, af # CF=0
                        call input_prune.no_exx
    echo_loop           wait_char_timeout 500
                        jr   Z, check_input
                        rst  0x08
                        cp   "\n".ord
                        jr   Z, check_input
                        cp   "\r".ord
                        jr   Z, check_input
                        sub  '0'.ord
                        jr   C, prompt_again
                        cp   10
                        jr   NC, prompt_again
                        ld   l, a
                        ld   h, 0
                        ex   af, af
                        mul_const a, 10, tt:de, clrhl:false
                        xor  a
                        ora  h
                        jr   NZ, prompt_again
                        ld   a, l
                        scf
                        ex   af, af
                        add  '0'.ord
                        rst  0x10
                        ld   a, l
                        ora  a
                        jr   NZ, echo_loop
                        put_char "\b"
                        jr   echo_loop

    check_input         ex   af, af
                        jr   NC, prompt_again
                        out  (memory_port), a
                        ld   hl, sysvars.flags
                        set  FLAGS_EX_ROM_TEST_BIT, [hl]
                        call exrombot
                        jp   NZ, ex_rom_menu
                        ld   hl, sysvars.flags
                        res  FLAGS_EX_ROM_TEST_BIT, [hl]
                        put_char 0x0C
                        put_text_tok userrambot
                        inc  hl
                        push hl
                        put_imm_text "\r\n"
                        pop  hl
                        put_text_tok hl
                        put_imm_text "\r\nRun? Y/n >"
                        get_char
                        cp   'n'.ord
                        jp   Z, ex_rom_menu
                        ld   hl, sysvars.flags
                        set  FLAGS_ALLOW_NMI_BIT, [hl]
                        jp   (ix)
  end

  ##
  # These helper macros may be used by the ex-rom code.
  module Macros
    include Z80::Stdlib::Macros
    ##
    # Produces code that announces the information about this EX-ROM to system menu.
    #
    # This code must be placed at the very beginning of the EX-ROM code.
    def exrom_announce(exrom_info, exrom_info_size, start, userrambot: self.userrambot)
      raise "exrom_announce must be placed at the top" unless pc == 0
      isolate do
                        memcpy userrambot, exrom_info, +exrom_info
                        ld   ix, start # start of exrom program
                        xor  a         # ZF=1 to signal valid exrom
                        ret
      end
    end
    ##
    # Produces code which calls a system routine, providing a routine's vector. Takes 3 bytes.
    #
    # Importing syslib namespace provides all the necessary vectors as sub-labels.
    #
    # Options:
    # * +syslib+:: A +syslib+ label for accessing the system routine vectors if +vecno+ is a symbol.
    def call_syslib(vecno, syslib:self.syslib)
      vecno = syslib.__send__(:"#{vecno}_v") if vecno.is_a?(Symbol)
      raise ArgumentError, "call_syslib: vecno should be a vector or a symbol" unless address?(vecno)
      isolate do
                        ld   a, vecno
                        rst  0x28
      end
    end
    ##
    # Produces code which calls a system routine, providing a routine's vector.
    #
    # Produces faster but larger (5 bytes) code than Macros::call_syslib.
    #
    # Importing syslib namespace provides all the necessary vectors as sub-labels.
    #
    # Options:
    # * +syslib+:: A +syslib+ label for accessing the system routine vectors if +vecno+ is a symbol.
    def call_syslib_fast(vecno, syslib:self.syslib)
      vecno = syslib.__send__(:"#{vecno}_v") if vecno.is_a?(Symbol)
      raise ArgumentError, "call_syslib_fast: vecno should be a vector or a symbol" unless address?(vecno)
      isolate do
                        exx
                        ld   hl, [vecno]
                        rst  0x30
      end
    end
    ##
    # Produces code which outputs a single character to the external device.
    #
    # +char+:: A character to be output, an 8-bit register or an address.
    #
    # Options:
    # * +wait+:: If the routine should wait until the character is being successfully buffered.
    def put_char(char, wait:true)
      isolate do
                        char = char.ord if char.is_a?(String)
                        ld   a, char unless char == a
        if wait
                        ora  a
        else
                        scf
        end
                        rst  0x10
      end
    end
    ##
    # Produces code which receives a single character from the external device.
    #
    # ZF=0 indicates a success. In this instance +accumulator+ holds the character.
    # ZF=1 indicates that no new character is available.
    #
    # Options:
    # * +wait+:: If the routine should wait until the character is being successfully received.
    def get_char(wait:true)
      isolate do |eoc|
        if wait
                        scf
        else
                        ora  a
        end
                        rst  0x08
      end
    end
    ##
    # Produces code which waits a limited time for a single character from the external device.
    #
    # ZF=0 indicates a new character is available in the buffer.
    # ZF=1 indicates that no new character was available before timeout.
    #
    # +ticks+:: A number of ticks (16-bit) as a number, an address or a 16-bit register
    #           to wait for the character at least the given number of ticks before giving up.
    # Options:
    # * +syslib+:: A +syslib+ label for accessing the system routine vectors.
    def wait_char_timeout(ticks, syslib:self.syslib)
      raise ArgumentError, "wait_char_timeout: invalid arguments" unless address?(ticks) or
                                                                         [bc,de,hl,ix,iy].include?(ticks)
      isolate do |eoc|
        if address?(ticks)
                        ld   bc, ticks
        else
                        ld16 bc, ticks unless ticks == bc
        end
                        call_syslib :input_wait_timeout, syslib:syslib
      end
    end
    ##
    # Produces code which calls +wait_ticks+ system routine.
    #
    # +ticks+:: A number of ticks (16-bit) as a number, an address or a 16-bit register
    #           to wait for the character at least the given number of ticks before giving up.
    # Options:
    # * +syslib+:: A +syslib+ label for accessing the system routine vectors.
    def pause(ticks, syslib:self.syslib)
      raise ArgumentError, "wait_char_timeout: invalid arguments" unless address?(ticks) or
                                                                         [bc,de,hl,ix,iy].include?(ticks)
      isolate do |eoc|
        if address?(ticks)
                        ld   bc, ticks
        else
                        ld16 bc, ticks unless ticks == bc
        end
                        call_syslib :wait_ticks, syslib:syslib
      end
    end
  end
end

if __FILE__ == $0
  kernel = RAL_1243_Kernel.new 0x0000
  puts kernel.debug
  %w[syslib
    memory_port
    pio_a_data
    pio_a_ctrl
    pio_b_data
    pio_b_ctrl
    ctc0_port
    ctc1_port
    ctc2_port
    ctc3_port
    exrombot
    userrambot].each do |label|
      puts "#{'%12s'%label}: 0x#{'%04x'%kernel[label]} - #{'%5d'%kernel[label]}"
  end
  puts " kernel size: #{kernel.code.bytesize}"
  File.open(File.join(__dir__, "rom.bin"), 'wb') {|f| f.write kernel.code }
end
