--
-- autoip
-- Copyright (c) 2007-2010, Green Mountain Computing Systems, Inc.
-- All Rights Reserved.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

package autoip is
  attribute ccs_op : string;

  function conv_integer(arg: std_ulogic_vector) return natural;

  function ai_sign_extend(v : std_ulogic_vector; width: natural) return std_ulogic_vector;
  
  function ai_neg(v1 : std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_neg : function is  "-";

  function ai_abs(v1 : std_ulogic_vector) return std_ulogic_vector;

  function ai_add(v1 : std_ulogic_vector; c : std_ulogic) return std_ulogic_vector;

  function ai_add(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_add : function is  "+";

  function ai_addc(v1, v2 : std_ulogic_vector; c : std_ulogic) return std_ulogic_vector;
  
  function ai_sub(v1 : std_ulogic_vector; c : std_ulogic) return std_ulogic_vector;

  function ai_sub(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_sub : function is  "-";

  function ai_muls(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_muls : function is  "s*";

  function ai_mulu(v1, v2 : std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_mulu : function is  "u*";

  function ai_eq(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_eq : function is  "==";

  function ai_neq(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_neq : function is  "!=";

  function ai_lt(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_lt : function is  "<";

  function ai_le(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_le : function is  "<=";

  function ai_gt(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_gt : function is  ">";

  function ai_ge(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_ge : function is  ">=";

  function ai_lt_s(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_lt_s : function is  "<s";

  function ai_le_s(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_le_s : function is  "<=s";

  function ai_gt_s(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_gt_s : function is  ">s";

  function ai_ge_s(v1, v2 : std_ulogic_vector) return boolean;
  attribute ccs_op of ai_ge_s : function is  ">=s";

  function ai_not(v1 : std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_not : function is  "!";

  function ai_true(b : boolean) return std_ulogic_vector;
  
  function ai_shl(arg: std_ulogic_vector; count: std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_shl : function is  "<<";

  function ai_lshr(arg: std_ulogic_vector; count: std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_lshr : function is  "u>>";

  function ai_ashr(arg: std_ulogic_vector; count: std_ulogic_vector) return std_ulogic_vector;
  attribute ccs_op of ai_ashr : function is  "s>>";
end package;

package body autoip is
  function conv_integer (arg: std_ulogic_vector) return natural is
    variable result: natural := 0;
  begin
    for I in arg'range loop
      result := result+result;
      if arg(I) = '1' then
        result := result + 1;
      end if;
    end loop;
    return result;
  end CONV_INTEGER;

  function ai_sign_extend(v : std_ulogic_vector; width: natural) return std_ulogic_vector is
    variable pad : std_ulogic_vector(width-v'length-1 downto 0);
  begin
    pad := (others => v(v'left));
    return pad & v;
  end function;
  
  function ai_neg(v1 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, res : unsigned (v1'length-1 downto 0);
    variable zero : unsigned (v1'length-1 downto 0) := (others => '0');
  begin
    u1 := unsigned(v1);
    res := zero - u1;
    return std_ulogic_vector(res);
  end function;

  function ai_abs(v1 : std_ulogic_vector) return std_ulogic_vector is
    variable vs : signed (v1'length-1 downto 0);
  begin
    vs := abs signed(v1);
    return std_ulogic_vector(vs);
  end function;

  function ai_add(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2, res : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    res := u1 + u2;
    return std_ulogic_vector(res);
  end function;

  function ai_add(v1 : std_ulogic_vector; c : std_ulogic) return std_ulogic_vector is
    variable u1, res : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    res := u1 + c;
    return std_ulogic_vector(res);
  end function;
      
  function ai_addc(v1, v2 : std_ulogic_vector; c : std_ulogic) return std_ulogic_vector is
    variable u1, u2, res : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    res := u1 + u2 + c;
    return std_ulogic_vector(res);
  end function;

  function ai_sub(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1, u2, res : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    res := u1 - u2;
    return std_ulogic_vector(res);
  end function;

  function ai_sub(v1 : std_ulogic_vector; c : std_ulogic) return std_ulogic_vector is
    variable u1, res : unsigned (v1'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    res := u1 - c;
    return std_ulogic_vector(res);
  end function;

  function ai_mulu(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable u1 : unsigned (v1'length-1 downto 0);
    variable u2 : unsigned (v2'length-1 downto 0);
    variable res : unsigned (v1'length+v2'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    res := u1 * u2;
    return std_ulogic_vector(res);
  end function;

  function ai_muls(v1, v2 : std_ulogic_vector) return std_ulogic_vector is
    variable s1 : signed (v1'length-1 downto 0);
    variable s2 : signed (v2'length-1 downto 0);
    variable res : signed (v1'length+v2'length-1 downto 0);
  begin
    s1 := signed(v1);
    s2 := signed(v2);
    res := s1 * s2;
    return std_ulogic_vector(res);
  end function;
  
  function ai_eq(v1, v2 : std_ulogic_vector) return boolean is
  begin
    return v1 = v2;
  end function;

  function ai_neq(v1, v2 : std_ulogic_vector) return boolean is
  begin
    return v1 /= v2;
  end function;

  function ai_lt(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : unsigned (v1'length-1 downto 0);
    variable u2 : unsigned (v2'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    return u1 < u2;
  end function;

  function ai_le(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : unsigned (v1'length-1 downto 0);
    variable u2 : unsigned (v2'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    return u1 <= u2;
  end function;

  function ai_gt(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : unsigned (v1'length-1 downto 0);
    variable u2 : unsigned (v2'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    return u1 > u2;
  end function;

  function ai_ge(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : unsigned (v1'length-1 downto 0);
    variable u2 : unsigned (v2'length-1 downto 0);
  begin
    u1 := unsigned(v1);
    u2 := unsigned(v2);
    return u1 >= u2;
  end function;

  function ai_lt_s(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : signed (v1'length-1 downto 0);
    variable u2 : signed (v2'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    return u1 < u2;
  end function;

  function ai_le_s(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : signed (v1'length-1 downto 0);
    variable u2 : signed (v2'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    return u1 <= u2;
  end function;

  function ai_gt_s(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : signed (v1'length-1 downto 0);
    variable u2 : signed (v2'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    return u1 > u2;
  end function;

  function ai_ge_s(v1, v2 : std_ulogic_vector) return boolean is
    variable u1 : signed (v1'length-1 downto 0);
    variable u2 : signed (v2'length-1 downto 0);
  begin
    u1 := signed(v1);
    u2 := signed(v2);
    return u1 >= u2;
  end function;

  function ai_not(v1 : std_ulogic_vector) return std_ulogic_vector is
    variable v2 : std_ulogic_vector (v1'length - 1 downto 0) := (others => '0');
  begin
    if (v1 = v2) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function ai_true(b : boolean) return std_ulogic_vector is
  begin
    if (b) then
      return "1";
    else
      return "0";
    end if;
  end function;

  function ai_shl(arg: std_ulogic_vector; count: std_ulogic_vector) return std_ulogic_vector is
    variable res : unsigned (arg'length-1 downto 0);
  begin
    res:=shl(unsigned(arg),unsigned(count));
    return std_ulogic_vector(res);
  end function;
  
  function ai_lshr(arg: std_ulogic_vector; count: std_ulogic_vector) return std_ulogic_vector is
    variable res : unsigned (arg'length-1 downto 0);
  begin
    res:=shr(unsigned(arg),unsigned(count));
    return std_ulogic_vector(res);
  end function;
  
  function ai_ashr(arg: std_ulogic_vector; count: std_ulogic_vector) return std_ulogic_vector is
    variable res : signed (arg'length-1 downto 0);
  begin
    res:=shr(signed(arg),unsigned(count));
    return std_ulogic_vector(res);
  end function;
  
end package body;

