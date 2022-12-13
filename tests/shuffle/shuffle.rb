# -*- coding: BINARY -*-
#
# This Ruby 2.1+ program requires https://github.com/royaltm/z80-rb.
#
# Generates files:
# - shuffle.bin  : a minikernel for shuffle_test.rs
# - shuffle.meta : a minikernel symbol table for shuffle_test.rs
# - shuffle.tap  : a ZX Spectrum program in a TAP format for generating the shuffled_*.bin templates.
#
# Author: Rafał Michalski <royaltm75@gmail.com>
#
# SPDX-License-Identifier: BlueOak-1.0.0
# This program is free to use under the terms of the Blue Oak Model License 1.0.0.
# See: https://blueoakcouncil.org/license/1.0.0
require 'z80'
require 'z80/math_i'
require 'z80/utils/shuffle'
require 'z80/utils/sort'

class Shuffle
  include Z80

  export shuffle
  export seed
  export multiplicator
  export sort
  export mul_seed
  export run_forever
  export target
  export target_end

  TARGET_SIZE = 256

  macro_import MathInt
  macro_import Utils::Shuffle
  macro_import Utils::Sort

  ns :shuffle do
                        shuffle_bytes_source_max256(random, target:target, length: TARGET_SIZE)
                        ret
    random              ld  hl, [seed]
                        rnd
                        ld  [seed], hl
                        ld  a, l
                        ret
  end

  ns :sort do
    ns :selection_asc do
                        selection_sort_bytes_max256(reverse: false, target:target_end - 1, length:TARGET_SIZE, subroutine:true)
    end
    ns :selection_desc do
                        selection_sort_bytes_max256(reverse: true , target:target, length:TARGET_SIZE, subroutine:true)
    end
    ns :insertion_asc do
                        insertion_sort_bytes_max256(reverse: false, target:target, length:TARGET_SIZE, subroutine:true)
    end
    ns :insertion_desc do
                        insertion_sort_bytes_max256(reverse: true , target:target_end - 1, length:TARGET_SIZE, subroutine:true)
    end
    ns :quick_asc do
                        ld   de, target
                        ld   hl, target_end - 1
                        quicksort_bytes(:half, reverse: false, pivot_reg: c, swap_same: false, safe_args: true)
    end
    ns :quick_desc do
                        ld   hl, target
                        ld   de, target_end - 1
                        quicksort_bytes(:half, reverse: true,  pivot_reg: b, swap_same: false, safe_args: true)
    end
  end

  ns :mul_seed do
                        ld    hl, [seed]
                        ld    bc, [multiplicator]
                        mul16_32 bc, tt:bc, clrhlhl:true, signed_hl:false, optimize: :size
                        ld    [multiplicator], hl
                        exx
                        ld    [seed], hl
                        # print out decimal uint32
                        ld    de, seed
                        ld    c, 4
                        rst   0x20
                        ret
  end

  ns :run_forever do
                        ld    ix, sort.selection_asc
                        call  run_sequence
                        ld    ix, sort.selection_desc
                        call  run_sequence
                        ld    ix, sort.insertion_asc
                        call  run_sequence
                        ld    ix, sort.insertion_desc
                        call  run_sequence
                        ld    ix, sort.quick_asc
                        call  run_sequence
                        ld    ix, sort.quick_desc
                        call  run_sequence
                        xor   a
                        out   (69), a
                        jr    run_forever

    run_sequence        ld    hl, [seed]
                        ld    [multiplicator], hl
                        push  hl
                        call  shuffle
                        rst   0x18 # call (ix)
                        call  mul_seed
                        pop   hl
                        inc   hl
                        ld    [seed], hl
                        ret
  end

  codetop               label

  seed                  dw    0
  multiplicator         dw    0

  target                bytes TARGET_SIZE
  target_end            label
end

class MiniKernel
  include Z80
  macro_import MathInt
  ns :kernel do
    ns :rst00 do
                        di
                        ld    hl, stackend
                        ld    sp, hl
                        jp    start
    end
                        org  0x0008
    isolate :rst08 do
                        jp    (hl)
    end

    ns :start do        ld    hl, [routine]
                        rst   0x08
                        halt
    end
                        org  0x0010
    # output character A=char
    isolate :rst10 do
                        out   (66), a
                        ret
    end
                        org  0x0018
    isolate :rst18 do
                        jp   (ix)
    end
                        org  0x0020
    # print a decimal number pointed at by +de+ register with the number of bytes in c register
    with_saved :rst20, af, bc, de, :exx, bc, de, hl, :exx, ret: true do |eoc|
                        utobcd bcdbufend, de, size: c, byteorder: :lsb
                        exx
                        bcdtoa hl, c, skip_leading0:true do |eoc|
                          add '0'.ord
                          rst 0x10
                        end
    end
  end
                        import  Shuffle

  routine               dw    shuffle

                        bytes 20
  bcdbufend             label

  stackbot              words 256
  stackend              label
end


if __FILE__ == $0
  require 'json'
  require 'zxlib/basic'

  class ShuffleTest
    include Z80
    include Z80::TAP

    with_saved :start_test, :exx, hl, :exx, ret: true do
                        call shuffle
                        ld   bc, [seed]
    end

                        import  Shuffle
  end

  program_code = ShuffleTest.new 0x8000
  puts program_code.debug
  program = ZXLib::Basic.parse_source <<-EOBasic
  10 INPUT "seed (0-65535):", seed
  20 POKE #{program_code[:seed]},seed-256*INT (seed/256): POKE #{program_code[:seed]+1},INT (seed/256)
  30 PRINT "out seed: ";USR #{program_code[:start_test]}
  40 PRINT "Export raw binary data at:"'"Address: #{'%04x'%program_code[:target]}h #{program_code[:target]}"'"Length: 256"
9998 GO TO 10
9999 CLEAR #{program_code.org-1}: LOAD ""CODE : RUN
EOBasic

  Dir.chdir(__dir__) do
    program.save_tap("shuffle", line: 9999)
    program_code.save_tap("shuffle", append: true)
    Z80::TAP.parse_file("shuffle.tap") { |hb| puts hb.to_s }

    kernel = MiniKernel.new 0
    puts kernel.debug
    File.open("shuffle.bin", 'wb') {|f| f.write kernel.code }
    File.open("shuffle.meta", 'w') {|f| f.write kernel.labels.to_json }
  end
  puts "The binary output to save after running the program:"
  puts "  256 bytes starting from: 0x#{'%04x'%program_code[:target]} (#{program_code[:target]})"
end
