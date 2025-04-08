--------------------------------------------------------------------------
-- Copyright (c) 2002-2007 by Impulse Accelerated Technologies, Inc.    --
-- All rights reserved.                                                 --
--                                                                      --
-- This source file may be used and redistributed without charge        --
-- subject to the provisions of the IMPULSE ACCELERATED TECHNOLOGIES,   --
-- INC. REDISTRIBUTABLE IP LICENSE AGREEMENT, and provided that this    --
-- copyright statement is not removed from the file, and that any       --
-- derivative work contains this copyright notice.                      –-
--------------------------------------------------------------------------
--                                                                      --
-- gmem.vhd: Generic synthesizable RAM.                                 --
--                                                                      --
-- Change History
-- --------------
-- 06/05/2003 - Scott Thibault
--   File created.
-- 09/11/2003 - Scott Thibault
--   Syntax changes for ModelSim compatibility.
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gmem is
  generic (
    DATAWIDTH : natural := 32;
    ADDRWIDTH : natural := 256
  );

  port (
    rst,clk : in std_ulogic;
    we : in std_ulogic;
    addr : in std_ulogic_vector(ADDRWIDTH-1 downto 0);
    din : in std_ulogic_vector(DATAWIDTH-1 downto 0);
    dout : out std_ulogic_vector(DATAWIDTH-1 downto 0)
  );
end gmem;

architecture rtl of gmem is
  constant size : natural := 2 ** ADDRWIDTH;
  type memtype is array (0 to size-1) of
    std_ulogic_vector(DATAWIDTH-1 downto 0);
  signal mem : memtype;
  signal index: natural;
begin
  index <= to_integer(unsigned(addr));

  dout <= mem(index);

  write: process (clk)
  begin
    if (clk'event and clk='1') then
      if (we = '1') then
        mem(index) <= din;
      end if;
    end if;
  end process;

end rtl;
