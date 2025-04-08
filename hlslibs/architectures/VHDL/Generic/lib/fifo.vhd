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
-- fifo.vhd: Generic synthesizable FIFO.                                --
--                                                                      --
-- Change History
-- --------------
-- 06/05/2003 - Scott Thibault
--   File created.
-- 03/17/2004 - Scott Thibault
--   Converted to a synchronous fifo.
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity sync_fifo is
  generic (DATAWIDTH : positive := 8; ADDRESSWIDTH : positive := 4);
  port (
    reset, clk : std_ulogic;
    read, write : std_ulogic;
    din : std_ulogic_vector (DATAWIDTH-1 downto 0);
    empty, full : inout std_ulogic;
    dout : out std_ulogic_vector (DATAWIDTH-1 downto 0)
  );
end;

architecture rtl of sync_fifo is
  constant DEPTH : positive := 2**ADDRESSWIDTH;

  type memtype is array (DEPTH-1 downto 0) of std_ulogic_vector(DATAWIDTH-1 downto 0);
  signal mem : memtype;

  signal allow_wr, allow_rd, almost_empty : std_ulogic;
  signal in_ptr, next_inptr, out_ptr, next_outptr : unsigned(ADDRESSWIDTH-1 downto 0);
begin
  next_inptr <= in_ptr + 1;
  next_outptr <= out_ptr + 1;

  allow_wr <= '1' when (write = '1' and full = '0') else '0';
  writeptr : process (reset,clk)
  begin
    if (reset = '1') then
      in_ptr <= (others => '0');
    elsif (clk'event and clk='1') then
      if (allow_wr = '1') then
        in_ptr <= next_inptr;
      end if;
    end if;
  end process;

  allow_rd <= '1' when (read = '1' and empty = '0') else '0';
  readptr : process (reset,clk)
  begin
    if (reset = '1') then
      out_ptr <= (others => '0');
    elsif (clk'event and clk='1') then
      if (allow_rd = '1') then
        out_ptr <= next_outptr;
      end if;
    end if;
  end process;

  -- The following will be detected as ALTSYNCRAM
  syncmem : process (reset,clk)
  begin
    if (clk'event and clk='1') then
      if (allow_wr = '1') then
        mem(conv_integer(in_ptr)) <= din;
      end if;
      dout <= mem(conv_integer(out_ptr));
    end if;
  end process;

  almost_empty <= '1' when next_outptr = in_ptr else '0';

  status : process (reset,clk)
  begin
    if (reset = '1') then
      empty <= '1';
    elsif (clk'event and clk='1') then
      if (allow_wr = '1') then
        empty <= '0';
      elsif (allow_rd = '1') then
        empty <= almost_empty;
      end if;
    end if;
  end process;

  full <= '1' when next_inptr = out_ptr else '0';  
end;

