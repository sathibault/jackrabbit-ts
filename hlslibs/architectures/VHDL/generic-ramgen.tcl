###############################################################################
# Copyright 2002-2007 Impulse Accelerated Technologies, Inc.
# All Rights Reserved.
###############################################################################

proc GenerateRAM {fd ramlist} {
	foreach ram $ramlist {
		set ename [lindex $ram 0]
		set datawidth [lindex $ram 1]
		set realdepth [lindex $ram 2]
		set addrwidth [calcAddrBits $realdepth]
		set init [lindex $ram 3]
		puts $fd "library ieee;"
		puts $fd "use ieee.std_logic_1164.all;"
		puts $fd "use ieee.numeric_std.all;"
		puts $fd ""
		puts $fd "entity $ename is"
		puts $fd "  port ("
		puts $fd "    rst,clk : in std_ulogic;"
		puts $fd "    we : in std_ulogic;"
		puts $fd "    addr : in std_ulogic_vector([expr $addrwidth - 1] downto 0);"
		puts $fd "    din : in std_ulogic_vector([expr $datawidth - 1] downto 0);"
		puts $fd "    dout : out std_ulogic_vector([expr $datawidth - 1] downto 0)"
		puts $fd "  );"
		puts $fd "end $ename;"
		puts $fd ""
		puts $fd "architecture behavior of $ename is"
		puts $fd "  type memtype is array (0 to [expr $realdepth - 1]) of"
		puts $fd "    std_ulogic_vector([expr $datawidth - 1] downto 0);"
		if {$init != ""} {
			puts -nonewline $fd "  constant INIT: memtype := ("
			puts -nonewline $fd [join $init ","]
			if {[llength $init] < $realdepth} {
				puts -nonewline $fd ",others => "
				puts -nonewline $fd \"[string repeat "U" $datawidth]\"
			}
			puts $fd ");"
		}
		puts $fd "  signal mem : memtype;"
		puts $fd "  signal index: natural;"
		puts $fd "begin"
		puts $fd "  index <= to_integer(unsigned(addr));"
		puts $fd ""
		puts $fd "  dout <= mem(index);"
		puts $fd ""
		if {$init == ""} {
			puts $fd "  write: process (clk)"
			puts $fd "  begin"
			puts $fd "    if (clk'event and clk='1') then"
			puts $fd "      if (we = '1') then"
			puts $fd "        mem(index) <= din;"
			puts $fd "      end if;"
			puts $fd "    end if;"
			puts $fd "  end process;"
		} else {
			puts $fd "  write: process (rst,clk)"
			puts $fd "  begin"
			puts $fd "    if (rst = '1') then"
			puts $fd "      mem <= INIT;"
			puts $fd "    elsif (clk'event and clk='1') then"
			puts $fd "      if (we = '1') then"
			puts $fd "        mem(index) <= din;"
			puts $fd "      end if;"
			puts $fd "    end if;"
			puts $fd "  end process;"
		}
		puts $fd "end behavior;"
		puts $fd ""
	}
}

