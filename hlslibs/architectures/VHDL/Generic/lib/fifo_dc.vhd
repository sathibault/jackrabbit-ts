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
-- fifo_dc.vhd: Generic synthesizable dual-clock FIFO.                  --
--                                                                      --
-- Change History
-- --------------
-- 06/05/2003 - Scott Thibault
--   File created.
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity fifo_dc is
  generic (DATAWIDTH : positive := 8; ADDRESSWIDTH : positive := 4);
  port (
    r_reset, rclk : std_ulogic;
    w_reset, wclk : std_ulogic;
    read, write : std_ulogic;
    din : std_ulogic_vector (DATAWIDTH-1 downto 0);
    empty, full : inout std_ulogic;
    dout : out std_ulogic_vector (DATAWIDTH-1 downto 0)
  );
end;

architecture rtl of fifo_dc is
  -- Convert binary vector to gray code vector
  function to_gray (vec : unsigned) return unsigned is
    alias av : unsigned(vec'length - 1 downto 0) is vec;
    variable res : unsigned(vec'length - 1 downto 0);
  begin
    res(res'left) := av(res'left);
    for i in res'left - 1 downto 0 loop
      res(i) := av(i) xor av(i + 1);
    end loop;  -- i
    return res;
  end to_gray;
  
  constant DEPTH : positive := 2**ADDRESSWIDTH;

  type memtype is array (DEPTH-1 downto 0) of std_ulogic_vector(DATAWIDTH-1 downto 0);
  signal mem : memtype;

  -- Write side signals
  signal in_ptr, next_in_ptr , in_ptr_gray, out_ptr_gray1, out_ptr_gray2 : unsigned(ADDRESSWIDTH-1 downto 0);

  -- Read side signals
  signal out_ptr, next_out_ptr , out_ptr_gray, in_ptr_gray1, in_ptr_gray2 : unsigned(ADDRESSWIDTH-1 downto 0);
begin
  -- Write side
  next_in_ptr <= in_ptr + 1;

  full <= '1' when to_gray(next_in_ptr) = out_ptr_gray2 else '0';

  writemem : process (w_reset,wclk)
  begin
    if (w_reset = '1') then
      in_ptr <= (others => '0');
      in_ptr_gray <= (others => '0');
    elsif (wclk'event and wclk='1') then
      if (write = '1' and full='0') then
        mem(conv_integer(in_ptr)) <= din;
        in_ptr <= next_in_ptr;
        in_ptr_gray <= to_gray(next_in_ptr);
      end if;
    end if;
  end process;

  -- synchronize out_ptr_gray
  process (w_reset,wclk)
  begin
    if (w_reset = '1') then
      out_ptr_gray1 <= (others => '0');
      out_ptr_gray2 <= (others => '0');
    elsif (wclk'event and wclk='1') then
      out_ptr_gray1 <= out_ptr_gray;
      out_ptr_gray2 <= out_ptr_gray1;
    end if;
  end process;

  -- Read side
  next_out_ptr <= out_ptr + 1;

  empty <= '1' when in_ptr_gray2 = to_gray(out_ptr) else '0';

  readmem : process (r_reset,rclk)
  begin
    if (r_reset = '1') then
      out_ptr <= (others => '0');
      out_ptr_gray <= (others => '0');
    elsif (rclk'event and rclk='1') then
      if (read = '1' and empty = '0') then
        out_ptr <= next_out_ptr;
        out_ptr_gray <= to_gray(next_out_ptr);
      end if;
    end if;
  end process;

  dout <= mem(conv_integer(out_ptr));

  -- synchronize int_ptr_gray
  process (r_reset,rclk)
  begin
    if (r_reset = '1') then
      in_ptr_gray1 <= (others => '0');
      in_ptr_gray2 <= (others => '0');
    elsif (rclk'event and rclk='1') then
      in_ptr_gray1 <= in_ptr_gray;
      in_ptr_gray2 <= in_ptr_gray1;
    end if;
  end process;

end;

