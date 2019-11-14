# -*- coding: BINARY -*-
#
# The EX-ROM 3 for RAL 1243
#
# This Ruby program requires a gem found at: https://github.com/royaltm/z80-rb.
#
# Author: Rafał Michalski <royaltm75@gmail.com>
#
# This program is free to use under the terms of the Blue Oak Model License 1.0.0.
# https://blueoakcouncil.org/license/1.0.0
require_relative '../rom/ral1243_rom'
require 'z80/utils/vec_deque'

class ExRom003
  ::ExRom = self unless defined?(::ExRom)
  include Z80

  VERSION = "1.0.0"

  VecDequeState = Utils::VecDeque::VecDequeState

  RESERVE_STACK_SPACE = 16
  INITIAL_SNAKE_SIZE = 5
  INITIAL_TICKS_DELAY = 25
  SPEEDUP_EACH_SECONDS = 20
  AREA_W = 64
  AREA_H = 32

  macro_import MathInt
  macro_import Utils::VecDeque
  label_import RAL_1243_Kernel, labels: 0x0000, macros:true

  class Coords < Label
    x byte
    y byte
  end

  class Score < Label
    digits        byte, 4
  end

  class Vars < Label
    seed          word
    coords        Coords
    direction     byte
    prev_dir      byte
    snake_top     word
    snake         VecDequeState
    snake_negsize word
    add_snacks    byte
    speedup_count byte
    delay         word
    score         Score
  end

  raise "AREA_W must be a multiple of 8" unless AREA_W % 8 == 0
  raise "AREA_W must be between 8 and 248" unless AREA_W >= 8 and AREA_W <= 248
  raise "AREA_H must be between 8 and 248" unless AREA_H >= 8 and AREA_H <= 248

  # our interrupt vectors
  vectors               addr userrambot, RAL_1243_Kernel::Vectors
  # 2d index bitmap
  obstacles             addr :next, AREA_W/8*AREA_H
  # variables
  vars                  addr :next, Vars
  # bottom of snake's vector double ended queue
  snake_bot             addr :next, align: 2

  announce              exrom_announce(exrom_info, +exrom_info, init)

  exrom_info            data "The Sssshnake Game.\x00" +
                             "A game of \"Sssshnake\" ver.: #{VERSION}.\r\n\n" + 
                             "Control the ssshnake with cursor keys or WSAD keys.\r\n\n" +
                             "Eating snacks * gains more points but makes the ssshnake grow in size.\r\n\n" + 
                             "Don't let the ssshnake bite itself.\x00"

                        org  align: 16
  masks                 db   (0..7).map {|i| 1 << i}
  dirs                  data Coords, [0, -1], [-1, 0], [0, 1], [1, 0]

  bodies                db '|\ /',  # ↑ -> ↑←↓→
                           '\-/ ',  # ← -> ↑←↓→
                           ' /|\\', # ↓ -> ↑←↓→
                           '/ \-'   # → -> ↑←↓→

  keys                  db   17, ?w.ord, # up
                             18, ?a.ord, # left
                             19, ?s.ord, # down
                             20, ?d.ord  # right

  ###########################################################################
  #                              M A C R O S                                #
  ###########################################################################

  macro :clear_terminal do
                        put_char 0x15
                        put_char 0x00
                        put_char 0x0C
  end

  macro :put_imm_text do |_, *args|
                        call output_imm_cstr
                        db *args, 0
  end

  macro :put_char_at_cb do |_, char|
                        char = char.ord if char.is_a?(String)
                        ld   a, char unless char == a
                        call output_char_at_cb
  end

  macro :initialize_seed do
                        call_syslib :get_frames_counter
                        ld   a, r
                        xor  e
                        xor  l
                        ld   l, a
                        ld   a, r
                        xor  h
                        xor  d
                        ld   h, a
                        ld   [vars.seed], hl
  end

  # > x: column % AREA_W
  # > y: row % AREA_H
  macro :randomize_coords do |_, x, y|
    select_start_w      exx
                        call randomize
                        ld   a, l
                        exx
                        cp   AREA_W
                        jr   NC, select_start_w
                        ld   x, a
    select_start_h      exx
                        call randomize
                        ld   a, l
                        exx
                        cp   AREA_H
                        jr   NC, select_start_h
                        ld   y, a
  end

  # > a: direction 0..3
  macro :randomize_direction do
                        exx
                        call randomize
                        ld   a, l
                        exx
                        anda 3
  end

  # > hl: -> delta coordinates
  macro :direction_to_delta_ptr do |_, dir|
                        ld   a, dir unless dir == a
                        add  a, a
                        ld   hl, dirs
                        add  l
                        ld   l, a
  end

  # > tt: -> mask
  # > a: mask
  macro :get_mask do |_, xmod8, tt:de|
    th, tl = tt.split
                        ld   a, xmod8 unless xmod8 == a
                        ld   tt, masks
                        add  tl
                        ld   tl, a
                        ld   a, [tt]
  end

  # > hl: -> obstacles + y * AREA_W / 8 + x / 8
  # > tl: x & 7
  macro :coord_to_obstacle_addr do |_, x, y, tt:de|
    _, tl = tt.split
    raise ArgumentError if tt.split.include?(x) or tt.split.include?(y) or [h, l].include?(x) or [h, l].include?(y)
                        mul_const y, AREA_W/8, tt:tt, clrhl:true, signed_k:false
                        ld   tt, obstacles
                        add  hl, tt
                        ld   a, x
                        anda 7
                        ld   tl, a
                        xor  x
                        3.times { rrca }
                        adda_to h, l
  end

  # > tt: -> mask
  # > hl: -> obstacles + y * AREA_W / 8 + x / 8
  # > a: [hl]
  macro :set_obstacle do |_, x, y, tt:de|
                        coord_to_obstacle_addr x, y, tt:tt
                        get_mask tt.split[1], tt:tt
                        ora  [hl]
                        ld   [hl], a
  end

  # > tt: -> mask
  # > hl: -> obstacles + y * AREA_W / 8 + x / 8
  # > a: [hl]
  macro :clear_obstacle do |_, x, y, tt:de|
                        coord_to_obstacle_addr x, y, tt:tt
                        get_mask tt.split[1], tt:tt
                        cpl
                        anda [hl]
                        ld   [hl], a
  end

  # > tt: -> mask
  # > hl: -> obstacles + y * AREA_W / 8 + x / 8
  # > a: [hl] & mask
  # > ZF: !result
  macro :has_obstacle? do |_, x, y, tt:de|
                        coord_to_obstacle_addr x, y, tt:tt
                        get_mask tt.split[1], tt:tt
                        anda [hl]
  end

  # > a: body
  macro :find_body do
                        ld   a, [vars.prev_dir]
                        add  a, a
                        add  a, a
                        ld   l, a
                        ld   a, [vars.direction]
                        add  l # prev_dir * 4 + direction
                        ld   hl, bodies
                        add  l
                        ld   l, a
                        ld   a, [hl]
  end

  # > a: a % mod
  macro :modulo8 do |eoc, mod, t:|
    if Integer === mod and mod & (mod - 1) == 0
                        anda mod - 1
    else
                        ld   t, mod
                        cp   t
                        jr   C, eoc
      adjust            sub  t
                        jr   NC, adjust
                        add  t
    end
  end

  # > x: x + delta.x
  # > y: y + delta.y
  macro :move_snake do |_, x, y|
                        ld   hl, vars.direction
                        ld   a, [hl]
                        inc  hl
                        ld   [hl], a # prev_dir
                        direction_to_delta_ptr a
                        ld   a, x
                        add  [hl]
                        modulo8 AREA_W, t:x
                        ld   x, a
                        inc  hl
                        ld   a, y
                        add  [hl]
                        modulo8 AREA_H, t:y
                        ld   y, a
  end

  # branches to :found if snake's body contains coordinates x,y
  macro :snake_coords? do |_, x, y, found:|
                        exx
                        call snake_length
                        ld16 bc, hl
                        dec  bc
                        call snake_next_front # x
    loop1               ex   af, af
                        dec  bc
                        call snake_next_front.get_next # y
                        exx
                        cp   y
                        jr   NZ, skip_check_x
                        ex   af, af
                        cp   x
                        jr   Z, found
    skip_check_x        exx
                        ld   a, c
                        ora  b
                        jr   Z, finished
                        dec  bc
                        call snake_next_front.get_next # x
                        jp   loop1
    finished            exx
  end

  # reads keyboard and changes direction accordingly
  macro :check_key_change_dir do |eoc|
                        ld   de, keys
                        exx
                        ld   bc, [vars.delay]
                        ld   a, [vars.direction]
    char_loop0          ex   af, af
    char_loop1          wait_char_timeout bc
                        jr   Z, check_and_store_dir
                        get_char
                        exx
                        ld16 hl, de
                        ld   bc, 8
                        cpir
                        exx
                        jr   NZ, char_loop1
                        exx
                        scf
                        sbc  hl, de
                        ld   a, l
                        rra  # direction
                        exx
                        jr   char_loop0
    check_and_store_dir ex   af, af
                        ld   hl, vars.direction
                        ld   d, a
                        xor  [hl] # only turns are ok
                        rrca
                        jr   NC, eoc
                        ld   [hl], d
  end

  macro :add_snack_maybe do
                        exx
                        ld   hl, vars.add_snacks
                        ld   a, [hl]
                        anda a
                        jr   Z, skip_snack
                        dec  [hl]
                        call randomize
                        ld   a, l
                        modulo8 AREA_W, t:l
                        ld   c, a
                        ld   a, h
                        modulo8 AREA_H, t:h
                        ld   b, a
                        has_obstacle? c, b, tt:de
                        jr   NZ, skip_snack
                        ld   a, [de] # mask
                        ora  [hl]    # obstacle
                        ld   [hl], a # set obstacle
                        put_char_at_cb ?*
    skip_snack          exx
  end

  ###########################################################################
  #                                M A I N                                  #
  ###########################################################################

  ns :init do
                        ld   h, vectors >> 8
                        call_syslib :move_int_vectors
  end

  ns :start do
                        di
                        ld   a, 0b01010111  # disable ints, counter mode, _, raising edge, _, constant follow, reset, control
                        out  (ctc2_port), a
                        ld   a, 0b11010111  # enable ints, counter mode, _, raising edge, _, constant follow, reset, control
                        out  (ctc3_port), a
                        ld   hl, ctc3_int_handler
                        ld   [vectors.ctc3], hl
                        ei
                        clrmem obstacles, (+obstacles) + (+vars)
                        ld   hl, -RESERVE_STACK_SPACE*2
                        add  hl, sp
                        res  0, l
                        ld   [vars.snake_top], hl
                        ld   hl, -INITIAL_SNAKE_SIZE*2
                        ld   [vars.snake_negsize], hl
                        ld   a, INITIAL_TICKS_DELAY
                        ld   [vars.delay], a
                        ld   a, SPEEDUP_EACH_SECONDS
                        ld   [vars.speedup_count], a
                        vec_deque_clear vars.snake, vec_deque_bot:snake_bot
                        clear_terminal
                        initialize_seed
                        randomize_direction
                        ld   hl, vars.direction
                        ld   [hl], a # direction
                        inc  hl
                        ld   [hl], a # prev_dir
                        randomize_coords c, b
                        ld   a, 100
                        out  (ctc3_port), a
                        out  (ctc2_port), a # 100*100 = 10000 = 1 second
                        jp   next_move.new_coords
  end

  ns :next_move do
                        check_key_change_dir

                        ld   bc, [vars.coords]
                        find_body
                        put_char_at_cb a

                        add_snack_maybe

                        move_snake c, b

                        ld   de, 1
                        call add_score

                        call snake_length
                        ld   de, [vars.snake_negsize]
                        add  hl, de
                        jr   NC, new_coords

                        exx
                        call snake_pop_front # x
                        ld   c, a
                        call snake_pop_front # y
                        ld   b, a
                        clear_obstacle c, b, tt:de
                        put_char_at_cb ' '
                        exx

      new_coords        ld   [vars.coords], bc
                        has_obstacle? c, b, tt:de
                        jr   NZ, collision
                        ld   a, [de] # mask
                        ora  [hl]    # obstacle
                        ld   [hl], a # set obstacle
      push_coords       ld   a, c
                        call snake_push_back # x
                        ld   a, b
                        call snake_push_back # y
                        put_char_at_cb ?o
                        jp   next_move

      collision         snake_coords? c, b, found: self_bitten

      score_points      ld   de, 0x0100
                        call add_score

                        vec_deque_full? vars.snake, branch_not_full:lengthen_snake, branch_relative:true, tt:de
                        jp   push_coords

      lengthen_snake    ld   hl, [vars.snake_negsize]
                        dec  hl
                        dec  hl
                        ld   [vars.snake_negsize], hl
                        jp   push_coords

      self_bitten       put_char_at_cb ?x
  end

  ns :game_over do
                        put_imm_text "\r\nGame Over! Score: "
                        call output_score
                        pause 100
                        call_syslib :input_prune
                        get_char
                        jp   start
  end

  ###########################################################################
  #                              SUBROUTINES                                #
  ###########################################################################

  # < de - score to add in BCD
  ns :add_score do
                        ld   hl, vars.score[1] - 1
    [e, d, 0, 0].each_with_index do |v, i|
                        ret  NC if v == 0
                        dec  hl unless i.zero?
                        ld   a, [hl]
      if i.zero?
                        add  v
      else
                        adc  v
      end
                        daa
                        ld   [hl], a
    end
                        ret
  end

  ns :output_score do
                        ld   hl, vars.score
                        ld   c, 4
                        xor  a
    seek_nonzero        cp   [hl]
                        jr   NZ, found
                        dec  c
                        jr   Z, found_last
                        inc  hl
                        jr   seek_nonzero
    found_last          inc  c
    found               call_syslib :output_bcd
                        ret
  end

  snake_length          vec_deque_length vars.snake, vec_deque_bot:snake_bot, vec_deque_top:[vars.snake_top], tt: de, subroutine:true

  snake_next_front      vec_deque_next_front vars.snake, vec_deque_bot: snake_bot, vec_deque_top:[vars.snake_top], cursor: de, subroutine:true

  ns :snake_pop_front do
                        vec_deque_pop_front vars.snake, vec_deque_bot: snake_bot, vec_deque_top:[vars.snake_top], tt: de
                        ret
  end

  ns :snake_push_back do
                        vec_deque_push_back vars.snake, vec_deque_bot: snake_bot, vec_deque_top:[vars.snake_top], tt: de
                        ret
  end

  # > hl: random
  ns :randomize do
                        ld   hl, [vars.seed]
                        rnd
                        ld   [vars.seed], hl
                        ret
  end

  # < a: char
  # < c: column
  # < b: row
  ns :output_char_at_cb do
                        ex   af, af
                        put_char 0x10
                        put_char b
                        put_char c
                        ex   af, af
                        put_char a
                        ret
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

  # increase add_snacks, decrease delay if speedup_count reaches 0
  with_saved :ctc3_int_handler, af, hl, ret: :ei_reti do |eoc|
                        ld   hl, vars.add_snacks
                        inc  [hl]
                        inc  hl       # -> speedup_count
                        dec  [hl]
                        jr   NZ, eoc
                        ld   [hl], SPEEDUP_EACH_SECONDS
                        inc  hl       # -> delay LSB
                        dec  [hl]
                        jr   NZ, eoc
                        inc  [hl]     # min 1
  end
end

if __FILE__ == $0
  exrom = ExRom.new RAL_1243_Kernel.exrombot.to_i
  puts exrom.debug
  %w[vectors
    obstacles
    vars
    snake_bot
    masks
    dirs
    bodies
    keys].each do |label|
      puts "#{'%12s'%label}: 0x#{'%04x'%exrom[label]} - #{'%5d'%exrom[label]}"
  end
  puts "  exrom size: #{exrom.code.bytesize}"
  File.open(File.join(__dir__, "#{exrom.class.name.downcase}.bin"), 'wb') {|f| f.write exrom.code }
end
