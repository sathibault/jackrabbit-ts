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
-- csignal.vhd: Implements the Impulse C signal.                        --
--                                                                      --
-- Change History
-- --------------
-- 07/03/2003 - Scott Thibault
--   File created.
-- 04/25/2006 - Scott Thibault
--   _nodata version
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity csignal is
  generic (
    datawidth : positive := 8
  );
  port (
    reset, clk : in std_ulogic;
    input_en : in std_ulogic;
    input_data : in std_ulogic_vector (datawidth-1 downto 0);
    output_en : in std_ulogic;
    output_rdy : out std_ulogic;
    output_data : out std_ulogic_vector (datawidth-1 downto 0)
  );
end;

architecture rtl of csignal is
  signal value : std_ulogic_vector (datawidth-1 downto 0);
  signal signaled : std_ulogic;
begin
  process (clk)
  begin
    if clk'event and clk = '1' then
      if (input_en = '1') then
        value <= input_data;
      end if;
    end if;
  end process;

  process (reset, clk)
  begin
    if reset = '1' then
      signaled <= '0';
    elsif clk'event and clk = '1' then  -- rising clock edge
      signaled <= input_en or (signaled and not output_en);
    end if;
  end process;

  output_rdy <= signaled;
  output_data <= value;
end;

library ieee;
use ieee.std_logic_1164.all;

entity csignal_nodata is
  port (
    reset, clk : in std_ulogic;
    input_en : in std_ulogic;
    output_en : in std_ulogic;
    output_rdy : out std_ulogic
  );
end;

architecture rtl of csignal_nodata is
  signal signaled : std_ulogic;
begin
  process (reset, clk)
  begin
    if reset = '1' then
      signaled <= '0';
    elsif clk'event and clk = '1' then  -- rising clock edge
      signaled <= input_en or (signaled and not output_en);
    end if;
  end process;

  output_rdy <= signaled;
end;
