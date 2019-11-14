# -*- coding: BINARY -*-
#
# The EX-ROM 1 for RAL 1243
#
# This Ruby program requires a gem found at: https://github.com/royaltm/z80-rb.
#
# Author: Rafał Michalski <royaltm75@gmail.com>
#
# This program is free to use under the terms of the Blue Oak Model License 1.0.0.
# https://blueoakcouncil.org/license/1.0.0
require_relative '../rom/ral1243_rom'

class ExRom001
  ::ExRom = self unless defined?(::ExRom)
  include Z80

  VERSION = "1.0.0"

  label_import RAL_1243_Kernel, labels: 0x0000, macros:true


  announce              exrom_announce(exrom_info, +exrom_info, start)

  exrom_info            data "Echo test.\x00" + 
                             "This is the echo test. Version: #{VERSION}\r\n" +
                             "Type whatever you want, everything will be echoed back to the terminal.\x00"

  ns :start do
                        put_char 0x15
                        put_char 0x02
                        put_char 0x0C
    main_loop           get_char
                        put_char a
                        jp   main_loop
  end
end

if __FILE__ == $0
  exrom = ExRom.new RAL_1243_Kernel.exrombot.to_i
  puts exrom.debug
  puts "  exrom size: #{exrom.code.bytesize}"
  File.open(File.join(__dir__, "#{exrom.class.name.downcase}.bin"), 'wb') {|f| f.write exrom.code }
end
