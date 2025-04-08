proc GenerateRAM {fd ramlist} {
	foreach ram $ramlist {
		set ename [lindex $ram 0]
		set datawidth [lindex $ram 1]
		set realdepth [lindex $ram 2]
		set addrwidth [calcAddrBits $realdepth]
		set init [lindex $ram 3]
		set type [lindex $ram 4]
		set readonly [lindex $ram 5]
		puts $fd "library ieee;"
		puts $fd "use ieee.std_logic_1164.all;"
		puts $fd "use ieee.std_logic_arith.all;"
		puts $fd ""
		puts $fd "entity $ename is"
		puts $fd "  port ("
		puts $fd "    rst,clk : in std_ulogic;"
		puts $fd "    we : in std_ulogic;"
		puts $fd "    addr : in std_ulogic_vector([expr $addrwidth - 1] downto 0);"
		if {$type == "dualsync"} {
			puts $fd "    addr2 : in std_ulogic_vector([expr $addrwidth - 1] downto 0);"
		}
		puts $fd "    din : in std_ulogic_vector([expr $datawidth - 1] downto 0);"
		if {$type == "dualsync"} {
			puts $fd "    dout : out std_ulogic_vector([expr $datawidth - 1] downto 0);"
			puts $fd "    dout2 : out std_ulogic_vector([expr $datawidth - 1] downto 0)"
		} else {
			puts $fd "    dout : out std_ulogic_vector([expr $datawidth - 1] downto 0)"
		}
		puts $fd "  );"
		puts $fd "end $ename;"
		puts $fd ""
		puts $fd "architecture $type of $ename is"
		puts $fd "  type memtype is array (0 to [expr $realdepth - 1]) of"
		puts $fd "    std_ulogic_vector([expr $datawidth - 1] downto 0);"
		if {$readonly && $init != ""} {
			puts -nonewline $fd "  constant mem : memtype"
		} else {
			puts -nonewline $fd "  signal mem : memtype"
		}
		if {$init != ""} {
			if {[llength $init] > $realdepth} {
				set init [lrange $init 0 [expr $realdepth - 1]]
			}
			puts -nonewline $fd " := ("
			puts -nonewline $fd [join $init ","]
			if {[llength $init] < $realdepth} {
				puts -nonewline $fd ",others => "
				puts -nonewline $fd \"[string repeat "U" $datawidth]\"
			}
			puts $fd ");"
		} else {
			puts $fd ";"
		}
		if {$type == "sync"} {
			puts $fd "  signal raddr: unsigned([expr $addrwidth - 1] downto 0);"
		} elseif {$type == "dualsync"} {
			puts $fd "  signal raddr: unsigned([expr $addrwidth - 1] downto 0);"
			puts $fd "  signal raddr2: unsigned([expr $addrwidth - 1] downto 0);"
		}
		puts $fd "begin"
		if {$type == "sync" || $type == "dualsync" || !$readonly} {
			puts $fd "  writemem: process (clk)"
			puts $fd "  begin"
			puts $fd "    if (clk'event and clk='1') then"
			if {!$readonly} {
				puts $fd "      if (we = '1') then"
				puts $fd "        mem(conv_integer(unsigned(addr))) <= din;"
				puts $fd "      end if;"
			}
			if {$type == "sync"} {
				puts $fd "      raddr <= unsigned(addr);"
			} elseif {$type == "dualsync"} {
				puts $fd "      raddr <= unsigned(addr);"
				puts $fd "      raddr2 <= unsigned(addr2);"
			}
			puts $fd "    end if;"
			puts $fd "  end process;"
			puts $fd ""
		}
		if {$type == "sync"} {
			puts $fd "  dout <= mem(conv_integer(raddr));"
		} elseif {$type == "dualsync"} {
			puts $fd "  dout <= mem(conv_integer(raddr));"
			puts $fd "  dout2 <= mem(conv_integer(raddr2));"
		} else {
			puts $fd "  dout <= mem(conv_integer(unsigned(addr)));"
		}
		puts $fd "end $type;"
		puts $fd ""
	}
}

