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
-- stream_dc.vhd: Wrapper for fifo_dc module to interface with          --
--   CoBuilder-generated VHDL modules.                                  --
--                                                                      --
-- Change History
-- --------------
-- 09/22/2003 - Scott Thibault
--   File created.
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity stream_dc is
  generic (
    datawidth : positive := 8;
    addrwidth : positive := 4
  );
  port (
    ireset, iclk : in std_ulogic;
    input_en : in std_ulogic;
    input_rdy : out std_ulogic;
    input_eos : in std_ulogic;
    input_data : in std_ulogic_vector (datawidth-1 downto 0);
    oreset, oclk : in std_ulogic;
    output_en : in std_ulogic;
    output_rdy : out std_ulogic;
    output_eos : out std_ulogic;
    output_data : out std_ulogic_vector (datawidth-1 downto 0)
  );
end;

architecture rtl of stream_dc is

  component fifo_dc is
    generic (DATAWIDTH : in positive;
             ADDRESSWIDTH : in positive);
    port (r_reset, rclk : std_ulogic;
          w_reset, wclk : std_ulogic;
          read : in std_ulogic;
          write : in std_ulogic;
          din : in std_ulogic_vector (DATAWIDTH-1 downto 0);
          empty : inout std_ulogic;
          full : inout std_ulogic;
          dout : out std_ulogic_vector (DATAWIDTH-1 downto 0));
  end component;

  signal empty, full : std_ulogic;
  signal fifo_in, fifo_out : std_ulogic_vector (datawidth downto 0);
begin
  output_rdy <= not empty;
  input_rdy <= not full;
  fifo_in <= input_eos & input_data;
  output_data <= fifo_out(datawidth-1 downto 0);
  output_eos <= fifo_out(datawidth) when empty = '0' else '0';
  fifo_1 : fifo_dc
    generic map (datawidth+1, addrwidth)
    port map (oreset, oclk, ireset, iclk, output_en, input_en, fifo_in, empty, full, fifo_out);
end;

