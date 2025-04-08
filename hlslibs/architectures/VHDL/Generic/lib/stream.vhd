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
-- stream.vhd: Wrapper for fifo module to interface with CoBuilder-     --
--   generated VHDL modules.                                            --
--                                                                      --
-- Change History
-- --------------
-- 06/05/2003 - Scott Thibault
--   File created.
-- 09/12/2003 - Scott Thibault
--   Syntax changes for ModelSim compatibility.
-- 03/17/2004 - Scott Thibault
--   Modified to use new synchronous fifo.  Data is prefetched from
--   the fifo to provide one output per cycle.
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity stream is
  generic (
    datawidth : positive := 8;
    addrwidth : positive := 4
  );
  port (
    reset, clk : in std_ulogic;
    input_en : in std_ulogic;
    input_rdy : out std_ulogic;
    input_eos : in std_ulogic;
    input_data : in std_ulogic_vector (datawidth-1 downto 0);
    output_en : in std_ulogic;
    output_rdy : out std_ulogic;
    output_eos : out std_ulogic;
    output_data : out std_ulogic_vector (datawidth-1 downto 0)
  );
end;

architecture rtl of stream is

  component sync_fifo is
    generic (DATAWIDTH : in positive;
             ADDRESSWIDTH : in positive);
    port (reset : in std_ulogic;
          clk : in std_ulogic;
          read : in std_ulogic;
          write : in std_ulogic;
          din : in std_ulogic_vector (DATAWIDTH-1 downto 0);
          empty : inout std_ulogic;
          full : inout std_ulogic;
          dout : out std_ulogic_vector (DATAWIDTH-1 downto 0));
  end component;

  signal empty, full : std_ulogic;
  signal fifo_in, fifo_out : std_ulogic_vector (datawidth downto 0);
  signal rd_fifo, reading, available : std_ulogic;
  signal oreg, dout : std_ulogic_vector (datawidth downto 0);
begin
  input_rdy <= not full;
  fifo_in <= input_eos & input_data;
  output_data <= dout(datawidth-1 downto 0);
  output_eos <= dout(datawidth) when available = '1' else '0';
  fifo_1 : sync_fifo
    generic map (datawidth+1, addrwidth)
    port map (reset, clk, rd_fifo, input_en, fifo_in, empty, full, fifo_out);

  --
  -- Simulate asynchronous output by prefetching FIFO data.
  --

  -- If the FIFO is not empty and we don't have any data (i.e., not available)
  -- or its being consumed (i.e., output_en) then read the next value.
  rd_fifo <= not empty and (not available or output_en);

  process (reset,clk)
  begin
    if (reset = '1') then
      reading <= '0';
    elsif (clk'event and clk='1') then
      reading <= rd_fifo;
    end if;
  end process;

  -- Capture output from FIFO
  process (clk)
  begin
    if (clk'event and clk='1') then
      if (reading = '1') then
        oreg <= fifo_out;
      end if;
    end if;
  end process;

  dout <= fifo_out when reading='1' else oreg;

  process (reset, clk)
  begin
    if (reset = '1') then
      available <= '0';
    elsif (clk'event and clk='1') then
      available <= rd_fifo or (available and not output_en);
    end if;
  end process;
  
  output_rdy <= available;
end;

library ieee;
use ieee.std_logic_1164.all;

entity stream_narrow is
  generic (
    inwidth : positive := 64;
    outwidth : positive := 32
  );
  port (
    reset, clk : in std_ulogic;
    input_en : out std_ulogic;
    input_rdy : in std_ulogic;
    input_eos : in std_ulogic;
    input_data : in std_ulogic_vector (inwidth-1 downto 0);
    output_en : in std_ulogic;
    output_rdy : out std_ulogic;
    output_eos : out std_ulogic;
    output_data : out std_ulogic_vector (outwidth-1 downto 0)
  );
end;

architecture rtl of stream_narrow is
  constant NWORDS : positive := (inwidth + outwidth - 1) / outwidth;
  constant TMP_WIDTH : positive := NWORDS * outwidth;
  signal accept,final : std_ulogic;
  signal state : std_ulogic_vector(1 to NWORDS);
  signal datareg : std_ulogic_vector(1 to TMP_WIDTH);
  signal dataeos: std_ulogic;
begin
  -- internal logic

  accept <= input_rdy and (not state(1) or final);
  
  process (clk, reset)
  begin
    if reset = '1' then
      state <= (others => '0');
    elsif clk'event and clk = '1' then  -- rising clock edge
      if accept = '1' then
        state <= (others => '1');
      elsif output_en = '1' then
        if dataeos = '1' then
          state <= (others => '0');
        else
          state <= state(2 to NWORDS) & '0';
        end if;
      end if;
    end if;
  end process;

  process (clk)
  begin
    if clk'event and clk = '1' then  -- rising clock edge
      if accept = '1' then
        datareg(1 + TMP_WIDTH - inwidth to inwidth) <= input_data;
        dataeos <= input_eos;
      elsif output_en = '1' then
        datareg(1 to TMP_WIDTH - outwidth) <= datareg(outwidth + 1 to TMP_WIDTH);
      end if;
    end if;
  end process;

  final <= output_en and not state(2);

  -- input stream signals
  input_en <= accept;

  -- output stream signals
  output_rdy <= state(1);
  output_data <= datareg(1 to outwidth);
  output_eos <= dataeos;
end rtl;

library ieee;
use ieee.std_logic_1164.all;

entity stream_widen is
  generic (
    inwidth : positive := 32;
    outwidth : positive := 64
  );
  port (
    reset, clk : in std_ulogic;
    input_en : in std_ulogic;
    input_rdy : out std_ulogic;
    input_eos : in std_ulogic;
    input_data : in std_ulogic_vector (inwidth-1 downto 0);
    output_en : out std_ulogic;
    output_rdy : in std_ulogic;
    output_eos : out std_ulogic;
    output_data : out std_ulogic_vector (outwidth-1 downto 0)
  );
end;

architecture rtl of stream_widen is
  constant NWORDS : positive := (outwidth + inwidth - 1) / inwidth;
  constant TMP_WIDTH : positive := NWORDS * inwidth;
  signal emit: std_ulogic;
  signal state : std_ulogic_vector(1 to NWORDS);
  signal datareg : std_ulogic_vector(1 to TMP_WIDTH);
  signal dataeos: std_ulogic;
begin
  emit <= output_rdy and state(1);
  
  process (clk, reset)
  begin
    if reset = '1' then
      state <= (others => '0');
    elsif clk'event and clk = '1' then  -- rising clock edge
      if input_en = '1' and input_eos = '1' then
        state <= (others => '1');
      elsif emit = '1' then
        state(1 to NWORDS - 1) <= (others => '0');
        state(NWORDS) <= input_en;
      elsif input_en = '1' then
        state <= state(2 to NWORDS) & '1';
      end if;
    end if;
  end process;

  process (clk)
  begin
    if clk'event and clk = '1' then  -- rising clock edge
      if input_en = '1' then
        datareg(1 to TMP_WIDTH - inwidth) <= datareg(inwidth + 1 to TMP_WIDTH);
        datareg(TMP_WIDTH - inwidth + 1 to TMP_WIDTH) <= input_data;
        dataeos <= input_eos;
      end if;
    end if;
  end process;

  -- input stream signals
  input_rdy <= output_rdy or not state(1);

  -- output stream signals
  output_en <= emit;
  output_data <= datareg(1 + TMP_WIDTH - outwidth to outwidth);
  output_eos <= dataeos;
end rtl;

