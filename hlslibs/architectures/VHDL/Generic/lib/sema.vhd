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
-- Copyright (c) 2006 by GMCS, Inc.                                     --
-- All rights reserved.                                                 --
--                                                                      --
-- This source file may be used and distributed without restriction     --
-- provided that this copyright statement is not removed from the file  --
-- and that any derivative work contains this copyright notice.         --
--                                                                      --
-- sema.vhd: Implements the Impulse C semaphore.                        --
--                                                                      --
-- Change History
-- --------------
-- 04/25/2006 - Scott Thibault
--   File created.
-- 04/02/2007 - Scott Thibault
--   Added width generic for value
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity sema is
  generic (
    count : positive := 1;
    init : positive := 1;
    max : positive := 1;
    reqWidth : positive := 1;
    maxWidth : positive := 1
  );
  port (
    reset, clk : in std_ulogic;
    dec : in std_ulogic_vector (reqWidth downto 1);
    inc : in std_ulogic_vector (reqWidth downto 1);
    ack : out std_ulogic_vector (reqWidth downto 1)
  );
end;

architecture rtl of sema is
  signal value, next_value : unsigned (maxWidth-1 downto 0);
  signal req, nonzero: std_ulogic;
  signal accept : unsigned (0 downto 0);
  signal dec_r, selected, reqv : std_ulogic_vector (reqWidth downto 1);
begin
  reqv <= dec or (dec_r and not selected);
  req <= '1' when reqv /= (reqWidth downto 1 => '0') else '0';
  nonzero <= '1' when value /= 0 else '0';
  accept(0) <= req and nonzero;
  
  process (reset,clk)
  begin
    if reset = '1' then
      value <= conv_unsigned(init,maxWidth);
    elsif clk'event and clk = '1' then
      value <= next_value;
    end if;
  end process;

  process (value, inc, accept)
    variable no : unsigned (maxWidth - 1 downto 0);
    variable v : unsigned (0 downto 0);
  begin
    no := (others => '0');
    for i in count downto 1 loop
      v(0) := inc(i);
      no := no + v;
    end loop;
    no := no - accept;
    next_value <= value + no;
  end process;
  
  process (reset,clk)
  begin
    if reset = '1' then
      dec_r <= (others => '0');
    elsif clk'event and clk = '1' then
      dec_r <= reqv;
    end if;
  end process;

  process (reset, clk)
    variable ok, found : std_ulogic;
  begin
    if reset = '1' then
      selected <= (others => '0');
    elsif clk'event and clk = '1' then  -- rising clock edge
      found := '0';
      for i in count downto 1 loop
        ok := reqv(i) and accept(0) and not found;
        selected(i) <= ok;
        found := found or ok;
      end loop;
    end if;
  end process;

  ack <= selected;
end;
