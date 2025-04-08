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
-- cregister.vhd: Implements the Impulse C register.                    --
--                                                                      --
-- Change History
-- --------------
-- 04/21/2004 - Scott Thibault
--   File created.
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity cregister is
  generic (
    datawidth : positive := 8
  );
  port (
    clk : in std_ulogic;
    input_en : in std_ulogic;
    input_data : in std_ulogic_vector (datawidth-1 downto 0);
    output_data : out std_ulogic_vector (datawidth-1 downto 0)
  );
end;

architecture rtl of cregister is
  signal value : std_ulogic_vector (datawidth-1 downto 0);
begin
  process (clk)
  begin
    if clk'event and clk = '1' then
      if (input_en = '1') then
        value <= input_data;
      end if;
    end if;
  end process;

  output_data <= value;
end;

