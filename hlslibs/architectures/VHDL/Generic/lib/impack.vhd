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
-- impack.vhd: Main support package for CoBuilder-generated VHDL        --
--   modules.  Exports support components and subprograms.              --
--                                                                      --
-- Change History
-- --------------
-- 06/05/2003 - Scott Thibault
--   File created.
-- 09/20/2003 - Scott Thibault
--   Added separate input and output clocks.
-- 03/25/2004 - Scott Thibault
--   Added divmod components.
-- 04/16/2004 - Scott Thibault
--   Added cregister component.
--   Modified boolean functions to return vectors.
-- 04/16/2005 - David Pellerin
--   Added mul2_u and mul2_s functions
-- 05/10/2005 - Scott Thibault
--   Added DSP functions sataddu, satadds, satredu, satreds
--
--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

package components is
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

  component divmod_s is
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

  component stream is
    generic (datawidth : in positive;
             addrwidth : in positive);
    port (reset : in std_ulogic;
          clk : in std_ulogic;
          input_en : in std_ulogic;
          input_rdy : out std_ulogic;
          input_eos : in std_ulogic;
          input_data : in std_ulogic_vector (datawidth-1 downto 0);
          output_en : in std_ulogic;
          output_rdy : out std_ulogic;
          output_eos : out std_ulogic;
          output_data : out std_ulogic_vector (datawidth-1 downto 0));
  end component;

  component stream_dc is
    generic (datawidth : positive := 8;
             addrwidth : positive := 4);
    port (ireset, iclk : in std_ulogic;
          input_en : in std_ulogic;
          input_rdy : out std_ulogic;
          input_eos : in std_ulogic;
          input_data : in std_ulogic_vector (datawidth-1 downto 0);
          oreset, oclk : in std_ulogic;
          output_en : in std_ulogic;
          output_rdy : out std_ulogic;
          output_eos : out std_ulogic;
          output_data : out std_ulogic_vector (datawidth-1 downto 0));
  end component;

  component stream_narrow
    generic (inwidth : positive := 64;
             outwidth : positive := 32);
    port (reset, clk : in std_ulogic;
          input_en : out std_ulogic;
          input_rdy : in std_ulogic;
          input_eos : in std_ulogic;
          input_data : in std_ulogic_vector (inwidth-1 downto 0);
          output_en : in std_ulogic;
          output_rdy : out std_ulogic;
          output_eos : out std_ulogic;
          output_data : out std_ulogic_vector (outwidth-1 downto 0));
  end component;
  
  component stream_widen
    generic (inwidth : positive := 32;
             outwidth : positive := 64);
    port (reset, clk : in std_ulogic;
          input_en : in std_ulogic;
          input_rdy : out std_ulogic;
          input_eos : in std_ulogic;
          input_data : in std_ulogic_vector (inwidth-1 downto 0);
          output_en : out std_ulogic;
          output_rdy : in std_ulogic;
          output_eos : out std_ulogic;
          output_data : out std_ulogic_vector (outwidth-1 downto 0));
  end component;
  
  component csignal is
    generic (datawidth : in positive);
    port (reset : in std_ulogic;
          clk : in std_ulogic;
          input_en : in std_ulogic;
          input_data : in std_ulogic_vector (datawidth-1 downto 0);
          output_en : in std_ulogic;
          output_rdy : out std_ulogic;
          output_data : out std_ulogic_vector (datawidth-1 downto 0));
  end component;
  
  component csignal_nodata is
    port (reset : in std_ulogic;
          clk : in std_ulogic;
          input_en : in std_ulogic;
          output_en : in std_ulogic;
          output_rdy : out std_ulogic);
  end component;
  
  component sema is
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
  end component;

  component gmem is
    generic (DATAWIDTH : in natural;
             ADDRWIDTH : in natural);
    port (rst, clk : in std_ulogic;
          we : in std_ulogic;
          addr : in std_ulogic_vector(ADDRWIDTH-1 downto 0);
          din : in std_ulogic_vector(DATAWIDTH-1 downto 0);
          dout : out std_ulogic_vector(DATAWIDTH-1 downto 0));
  end component;

  component cregister
    generic (DATAWIDTH : in positive);
    port (clk : in std_ulogic;
          input_en : in std_ulogic;
          input_data : in std_ulogic_vector (DATAWIDTH-1 downto 0);
          output_data : out std_ulogic_vector (DATAWIDTH-1 downto 0));
  end component;

  component bridgeAHB is
  port (
    HCLK : in std_ulogic;
    HRESETn : in std_ulogic;
    HGRANTx : in std_ulogic;
    HREADY : in std_ulogic;
    HRESP : in std_ulogic_vector (1 downto 0);
    HRDATA : in std_ulogic_vector (31 downto 0);
    HBUSREQx : out std_ulogic;
    HLOCKx : out std_ulogic;
    HADDR : inout std_ulogic_vector (31 downto 0);
    HTRANS : out std_ulogic_vector (1 downto 0);
    HWRITE : out std_ulogic;
    HSIZE : out std_ulogic_vector (2 downto 0);
    HBURST : out std_ulogic_vector (2 downto 0);
    HPROT : out std_ulogic_vector (3 downto 0);
    HWDATA : out std_ulogic_vector (31 downto 0);
    p_idata : out std_ulogic_vector (31 downto 0);
    p_addr : out std_ulogic_vector (31 downto 0);
    p_wri : out std_ulogic;
    p_odata : in std_ulogic_vector (31 downto 0);
    p_ack : out std_ulogic;
    p_req : in std_ulogic;
    p_mode : in std_ulogic;
    p_base : in std_ulogic_vector (31 downto 0);
    p_size : in std_ulogic_vector (2 downto 0);
    p_count : in std_ulogic_vector (31 downto 0));
  end component;

  function max(L, R: INTEGER) return INTEGER;
  function nop(v : std_ulogic_vector) return std_ulogic_vector;
  function sign_ext(v : std_ulogic_vector; size : natural) return std_ulogic_vector;
  function slice(v : std_ulogic_vector; first : natural; size : natural) return std_ulogic_vector;

  function neg(v1 : std_ulogic_vector) return std_ulogic_vector;

  function add(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function sub(v1, v2 : std_ulogic_vector) return std_ulogic_vector;

  function mul(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function mul2(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function mul2_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function mul2_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector;

  function cmp_less_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function cmp_less_equal_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function cmp_less_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function cmp_less_equal_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function cmp_greater_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function cmp_greater_equal_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function cmp_greater_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function cmp_greater_equal_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function eq(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function neq(v1, v2 : std_ulogic_vector) return std_ulogic_vector;

  function lnot(v1 : std_ulogic_vector) return std_ulogic_vector;

  function shl(arg: std_ulogic_vector; count: natural) return std_ulogic_vector;
  function lshr(arg: std_ulogic_vector; count: natural) return std_ulogic_vector;
  function ashr(arg: std_ulogic_vector; count: natural) return std_ulogic_vector;
  function sataddu(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function satadds(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  function satredu(arg : std_ulogic_vector; constant count: natural) return std_ulogic_vector;
  function satreds(arg : std_ulogic_vector; constant count: natural) return std_ulogic_vector;  
end package;

package body components is
  function max(L, R: INTEGER) return INTEGER is
  begin
  if L > R then
      return L;
  else
      return R;
  end if;
  end;
    
  function nop(v : std_ulogic_vector) return std_ulogic_vector is
  begin
    return v;
  end function;

  function sign_ext(v : std_ulogic_vector; size : natural) return std_ulogic_vector is
    variable res : std_ulogic_vector (size-1 downto 0);
  begin
    res(size-1 downto v'length) := (others => v(v'left));
    res(v'length-1 downto 0) := v;
    return res;
  end function;

  function slice(v : std_ulogic_vector; first : natural; size : natural) return std_ulogic_vector is
    alias av : std_ulogic_vector (v'length-1 downto 0) is v;
    variable res : std_ulogic_vector (size-1 downto 0);
  begin
    res := av(size-1 downto 0);
    return res;
  end function;

  function neg(v1 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, res : unsigned (v1'length-1 downto 0);
    variable zero : unsigned (v1'length-1 downto 0) := (others => '0');
  begin
    u1 := unsigned(v1);
    res := zero - u1;
    return std_ulogic_vector(res);
  end function;

  function add(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2, res : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    res := u1 + u2;
    return std_ulogic_vector(res);
  end function;

  function sub(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2, res : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    res := u1 - u2;
    return std_ulogic_vector(res);
  end function;

  function mul(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : unsigned (v1'length-1 downto 0);
    variable res : unsigned (v1'length+v2'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    res := u1 * u2;
    return std_ulogic_vector(res(v1'length-1 downto 0));
  end function;

  function mul2(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    constant size : integer := max(v1'length,v2'length);
    variable u1, u2 : signed (size-1 downto 0);
    variable res : signed (size+size-1 downto 0);
  begin
    u1 := CONV_SIGNED(signed(v1),size);
    u2 := CONV_SIGNED(signed(v2),size);
    res := u1 * u2;
    return std_ulogic_vector(res(v1'length+v2'length-1 downto 0));
  end function;

  function mul2_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    constant size : integer := max(v1'length,v2'length);
    variable u1, u2 : signed (size-1 downto 0);
    variable res : signed (size+size-1 downto 0);
  begin
    u1 := CONV_SIGNED(signed(v1),size);
    u2 := CONV_SIGNED(signed(v2),size);
    res := u1 * u2;
    return std_ulogic_vector(res(v1'length+v2'length-1 downto 0));
  end function;

  function mul2_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    constant size : integer := max(v1'length,v2'length);
    variable u1, u2 : unsigned (size-1 downto 0);
    variable res : unsigned (size+size-1 downto 0);
  begin
    u1 := CONV_UNSIGNED(unsigned(v1),size);
    u2 := CONV_UNSIGNED(unsigned(v2),size);
    res := u1 * u2;
    return std_ulogic_vector(res(v1'length+v2'length-1 downto 0));
  end function;

  function cmp_less_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    if (u1 < u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function cmp_less_equal_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    if (u1 <= u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function cmp_less_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : signed (v1'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    if (u1 < u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function cmp_less_equal_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : signed (v1'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    if (u1 <= u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function cmp_greater_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    if (u1 > u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function cmp_greater_equal_u(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    if (u1 >= u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function cmp_greater_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : signed (v1'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    if (u1 > u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function cmp_greater_equal_s(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2 : signed (v1'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    if (u1 >= u2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function eq(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
  begin
	assert v1'length = v2'length report "Mismatched operand widths in eq";
    if (v1 = v2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function neq(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
  begin
	assert v1'length = v2'length report "Mismatched operand widths in neq";
    if (v1 /= v2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function lnot(v1 : std_ulogic_vector) return std_ulogic_vector is
    variable v2 : std_ulogic_vector (v1'length - 1 downto 0) := (others => '0');
  begin
    if (v1 = v2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function shl(arg: std_ulogic_vector; count: natural) return std_ulogic_vector is
    alias av : std_ulogic_vector (arg'length-1 downto 0) is arg;
    variable res : std_ulogic_vector (arg'length-1 downto 0);
  begin
    for i in arg'length-1 downto count loop
      res(i) := av(i-count);
    end loop;
    for i in 0 to count-1 loop
      res(i) := '0';
    end loop;
    return res;
  end function;
  
  function lshr(arg: std_ulogic_vector; count: natural) return std_ulogic_vector is
    alias av : std_ulogic_vector (arg'length-1 downto 0) is arg;
    variable res : std_ulogic_vector (arg'length-1 downto 0);
  begin
    for i in count to arg'length-1 loop
      res(i-count) := av(i);
    end loop;
    for i in arg'length-1 downto arg'length-count loop
      res(i) := '0';
    end loop;
    return res;
  end function;

  function ashr(arg: std_ulogic_vector; count: natural) return std_ulogic_vector is
    alias av : std_ulogic_vector (arg'length-1 downto 0) is arg;
    variable res : std_ulogic_vector (arg'length-1 downto 0);
  begin
    for i in count to arg'length-1 loop
      res(i-count) := av(i);
    end loop;
    for i in arg'length-1 downto arg'length-count loop
      res(i) := arg(arg'left);
    end loop;
    return res;
  end function;

  function sataddu(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable s1, s2, res : unsigned (v1'length downto 0);
  begin
    s1 := '0' & unsigned(v1);
    s2 := '0' & unsigned(v2);
    res := s1 + s2;
    if res(res'left) /= '0' then
      res(res'left-1 downto 0) := (others => '1');
    end if;
    return std_ulogic_vector(res(v1'length-1 downto 0));
  end function;

  function satadds(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable s1, s2, res : signed (v1'length-1 downto 0);
  begin
    s1 := signed(v1);
    s2 := signed(v2);
    res := s1 + s2;
    if s1(s1'left) = s2(s2'left) then
      if res(res'left) /= s1(s1'left) then
        if To_X01(s1(s1'left)) = '0' then
          res(res'left) := '0';
          res(res'left-1 downto 0) := (others => '1');
        else
          res(res'left) := '1';
          res(res'left-1 downto 0) := (others => '0');
        end if;
      end if;
    end if;
    return std_ulogic_vector(res);
  end function;

  function satredu(arg: std_ulogic_vector; constant count: natural) return std_ulogic_vector is
    alias av : std_ulogic_vector (arg'length-1 downto 0) is arg;
    constant trunc : natural := arg'length-count;
    variable zero : std_ulogic_vector (trunc-1 downto 0) := (others => '0');
    variable data : std_ulogic_vector (arg'length-trunc-1 downto 0);
  begin
    if av(arg'length-1 downto arg'length-trunc) /= zero then
      data := (others => '1');
    else
      data := av(arg'length-trunc-1 downto 0);
    end if;
    return zero & data;
  end function;

  function satreds(arg: std_ulogic_vector; constant count: natural) return std_ulogic_vector is
    alias av : std_ulogic_vector (arg'length-1 downto 0) is arg;
    constant trunc : natural := arg'length-count;
    variable sign : std_ulogic_vector (trunc downto 0);
    variable data : std_ulogic_vector (arg'length-trunc-2 downto 0);
  begin
    sign := (others => arg(arg'left));
    if av(arg'length-1 downto arg'length-trunc-1) /= sign then
      if To_X01(arg(arg'left)) = '1' then
        data := (others => '0');
      else
        data := (others => '1');
      end if;
    else
      data := av(arg'length-trunc-2 downto 0);
    end if;
    return sign & data;
  end function;

end package body;

