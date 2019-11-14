# -*- coding: BINARY -*-
#
# This Ruby program requires https://github.com/royaltm/z80-rb.
#
# Generates files:
# - shuffle.bin  : a minikernel for shuffle_test.rs
# - shuffle.meta : a minikernel symbol table for shuffle_test.rs
# - shuffle.tap  : a ZX Spectrum program in a TAP format for generating the shuffled_*.bin templates.
#
# Author: Rafał Michalski <royaltm75@gmail.com>
#
# This program is free to use under the terms of the Blue Oak Model License 1.0.0.
# https://blueoakcouncil.org/license/1.0.0
require 'z80'
require 'z80/math_i'
require 'z80/utils/shuffle'

class Shuffle
  include Z80

  export start
  export seed
  export target

  macro_import MathInt
  macro_import Utils::Shuffle

  ns :start do
                        shuffle_bytes_source_max256(random, target:target, length: 256)
                        ret
    random              ld  hl, [seed]
                        rnd
                        ld  [seed], hl
                        ld  a, l
                        ret
  end
  seed                  dw    0
  target                bytes 256
end

class MiniKernel
  include Z80
                        di
                        ld    hl, stackend
                        ld    sp, hl
                        call  start
                        halt

                        import  Shuffle
  stackbot              words   5
  stackend              label
end


if __FILE__ == $0
  require 'json'
  require 'zxlib/basic'

  class ShuffleTest
    include Z80
    include Z80::TAP

    with_saved :start_test, :exx, hl, :exx, ret: true do
                        call start
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
