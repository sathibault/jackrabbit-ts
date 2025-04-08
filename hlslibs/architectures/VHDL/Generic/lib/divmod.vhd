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
--
-- divmod.vhd: A simple divider/modulus component.  This implementation
--   is multi-cycle but high frequency.
--
-- Change History
-- --------------
-- 03/25/2004 - Scott Thibault
--   File created.
--
-----------------------------------------------------------------------

-- divmod_u - unsigned version

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity divmod_u is
  generic (
    WIDTH : positive := 32);
  port (
    reset, clk : in std_ulogic;
    go : in std_ulogic;
    n : in std_ulogic_vector(WIDTH-1 downto 0);
    d : in std_ulogic_vector(WIDTH-1 downto 0);
    q : out std_ulogic_vector(WIDTH-1 downto 0);
    r : out std_ulogic_vector(WIDTH-1 downto 0);
    done : inout std_ulogic);
end entity;

architecture rtl of divmod_u is
  signal start, running : std_ulogic;
  signal qreg, nreg : std_ulogic_vector(WIDTH-1 downto 0);
  signal partial, res : unsigned(WIDTH-1 downto 0);
  signal sub : unsigned(WIDTH downto 0);
  signal iter : std_ulogic_vector(1 to WIDTH);
begin
  process (clk,reset)
  begin
    if (reset = '1') then
      running <= '0';
    elsif (clk'event and clk='1') then
      if (start = '1') then
        running <= '1';
      elsif (done = '1') then
        running <= '0';
      end if;
    end if;
  end process;

  start <= go and not running;

  process (clk)
  begin
    if (clk'event and clk='1') then
      if (start = '1') then
        nreg <= n;
      else 
        nreg <= nreg(WIDTH-2 downto 0) & '0';
      end if;
    end if;
  end process;

  partial <= res(WIDTH-2 downto 0) & nreg(WIDTH-1);
  
  sub <= ('0' & partial) - unsigned('0' & d);

  process (clk, start)
  begin
    if (start = '1') then
      res <= (others => '0');  
    elsif (clk'event and clk='1') then
      if (done = '0') then
        if (sub(WIDTH)='0') then
          res <= sub(WIDTH-1 downto 0);
        else
          res <= partial;
        end if;
      end if;
    end if;
  end process;

  process (clk)
  begin
    if (clk'event and clk='1') then
      if (done = '0') then
        qreg <= qreg(WIDTH-2 downto 0) & not sub(WIDTH);
      end if;
    end if;
  end process;

  process (clk,start)
  begin
    if (start = '1') then
      iter <= (others => '0');
    elsif (clk'event and clk='1') then
      iter <= '1' & iter(1 to WIDTH-1);
    end if;
  end process;

  q <= qreg;
  r <= std_ulogic_vector(res);
  done <= iter(WIDTH);
end;

-- divmod_s - signed version

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity divmod_s is
  generic (
    WIDTH : positive := 32);
  port (
    reset, clk : in std_ulogic;
    go : in std_ulogic;
    n : in std_ulogic_vector(WIDTH-1 downto 0);
    d : in std_ulogic_vector(WIDTH-1 downto 0);
    q : out std_ulogic_vector(WIDTH-1 downto 0);
    r : out std_ulogic_vector(WIDTH-1 downto 0);
    done : inout std_ulogic);
end entity;

architecture rtl of divmod_s is
  component divmod_u is
    generic (
      WIDTH : positive := 32);
    port (
      reset, clk : in std_ulogic;
      go : in std_ulogic;
      n : in std_ulogic_vector(WIDTH-1 downto 0);
      d : in std_ulogic_vector(WIDTH-1 downto 0);
      q : out std_ulogic_vector(WIDTH-1 downto 0);
      r : out std_ulogic_vector(WIDTH-1 downto 0);
      done : inout std_ulogic);
  end component;

  signal ns, ds : signed(WIDTH-1 downto 0);
  signal nu, du : std_ulogic_vector(WIDTH-1 downto 0);
  signal qu, ru : std_ulogic_vector(WIDTH-2 downto 0);
begin
  ns <= abs signed(n);
  ds <= abs signed(d);
  nu <= std_ulogic_vector(ns);
  du <= std_ulogic_vector(ds);
  
  divmodu : divmod_u
    generic map (WIDTH => WIDTH-1)
    port map (reset, clk, go, nu(WIDTH-2 downto 0), du(WIDTH-2 downto 0), qu, ru, done);

  process (n,d,qu)
    variable qs : signed(WIDTH-1 downto 0);
  begin
    if (n(WIDTH-1) xor d(WIDTH-1)) = '1' then
      qs := -signed('0' & qu);
      q <= std_ulogic_vector(qs);
    else
      q <= '0' & qu;
    end if;
  end process;

  process (n,d,ru)
    variable rs : signed(WIDTH-1 downto 0);
  begin
    if n(WIDTH-1)  = '1' then
      rs := -signed('0' & ru);
      r <= std_ulogic_vector(rs);
    else
      r <= '0' & ru;
    end if;
  end process;
end;

