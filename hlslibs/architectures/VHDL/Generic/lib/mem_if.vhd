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
-- Copyright (c) 2006 by Green Mountain Computing Systems, Inc.
-- All rights reserved.
--
-- This source file may be used and distributed without restriction
-- provided that this copyright statement is not removed from the file
-- and that any derivative work contains this copyright notice.
-- This design is provided as is without any warranty of any kind.
--
-- mem_if.vhd: Implements misc. memory adaptors.
--
-- Change History
-- --------------
-- 04/19/2007 - Scott Thibault
--   Initial design (impc_mem_level2 not complete)
--
--------------------------------------------------------------------------

--
-- impc_mem_expander
-- buffer idata, odata
-- downsize size
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity impc_mem_expander is
    generic (
      LEVEL : natural := 2;
      bus_width : natural;
      bus_addrwidth : natural;
      wide_width : natural;
      wide_addrwidth : natural
    );
    port (
      reset : in std_ulogic;
      clk : in std_ulogic;
      mode : in std_ulogic;
      idata : in std_ulogic_vector(bus_width-1 downto 0);
      wri : in std_ulogic;
      ack : in std_ulogic;
      addr : in std_ulogic_vector(31 downto 0);
      nextaddr : in std_ulogic_vector(31 downto 0);
      dma_re : in std_ulogic;
      req : out std_ulogic;
      size : out std_ulogic_vector(bus_addrwidth-1 downto 0);
      chunk : out std_ulogic_vector(31 downto 0);
      stride : out std_ulogic_vector(31 downto 0);
      start : out std_ulogic_vector(31 downto 0);
      count : out std_ulogic_vector(31 downto 0);
      odata : out std_ulogic_vector(bus_width-1 downto 0);
      odata_w : in std_ulogic_vector(wide_width-1 downto 0);
      size_w : in std_ulogic_vector(wide_addrwidth-1 downto 0);
      chunk_w : in std_ulogic_vector(31 downto 0);
      stride_w : in std_ulogic_vector(31 downto 0);
      start_w : in std_ulogic_vector(31 downto 0);
      count_w : in std_ulogic_vector(31 downto 0);
      req_w : in std_ulogic;
      wri_w : out std_ulogic;
      ack_w : inout std_ulogic;
      addr_w : inout std_ulogic_vector(31 downto 0);
      nextaddr_w : inout std_ulogic_vector(31 downto 0);
      idata_w : out std_ulogic_vector(wide_width-1 downto 0)
      );
end;

architecture a1 of impc_mem_expander is
  constant lanes : natural := wide_width/bus_width;
  constant extbits : natural := wide_addrwidth-bus_addrwidth;

  signal shift : integer range 0 to extbits;
  signal iswide, running : std_ulogic;
  signal end_count, end_read, end_write : std_ulogic;
  signal size_ext : unsigned(extbits-1 downto 0);
  signal counter : unsigned(extbits downto 0);
  signal addr_1 : std_ulogic_vector(31 downto 0);

  signal write_buf : std_ulogic_vector(wide_width-1 downto 0);
  signal read_buf : std_ulogic_vector(wide_width-bus_width-1 downto 0);
  signal first_read, read_buf_en, read_buf_load, read_buf_new : std_ulogic;
  signal write_new : std_ulogic;
begin

  size_ext <= unsigned(size_w(wide_addrwidth-1 downto bus_addrwidth));

  iswide <= '1' when size_ext /= (size_ext'range => '0') else '0';

  size <= '1' & (bus_addrwidth-2 downto 0 => '0') when iswide = '1' else
          size_w(bus_addrwidth-1 downto 0);
  
  process (clk)
  begin  -- process
    if clk'event and clk = '1' then 
      if reset = '1' then
        running <= '0';
      else
        running <= iswide and req_w and not ack;
      end if;
    end if;
  end process;


  process (clk)
  begin  -- process
    if clk'event and clk = '1' then 
      if (running = '0' or end_write = '1' or end_read = '1') then
        counter <= to_unsigned(1,counter'length);
      elsif (wri = '1' or read_buf_en = '1') then
        counter <= counter + 1;
      end if;
    end if;
  end process;

  end_count <= '1' when counter(counter'left downto 1) = size_ext else '0';

  end_write <= wri and end_count;

  -- The idata vector may be steered depending on the bus, but if
  -- no data is steered in beyond that and the code generator is designed
  -- to take that into account.
  process (clk)
  begin    
    if clk'event and clk = '1' then
      if (wri = '1') then
        write_buf <= write_buf(wide_width-bus_width-1 downto 0) & idata;
      end if;
    end if;
  end process;

  end_read <= read_buf_en and end_count;

  process (clk)
  begin  -- process
    if clk'event and clk = '1' then
      if (running = '0') then
        first_read <= '1';
      else
        if (read_buf_en = '1') then
          first_read <= '0';
        end if;
      end if;
    end if;
  end process;
  
  process (clk)
  begin    
    if clk'event and clk = '1' then
      read_buf_new <= end_read;
    end if;
  end process;

  read_buf_load <= read_buf_new or first_read;
  
  process (clk)
  begin  -- process
    if clk'event and clk = '1' then
      if (read_buf_load = '1') then
        read_buf <= odata_w(wide_width-bus_width-1 downto 0);
      elsif (read_buf_en = '1') then
        read_buf(wide_width-bus_width-1 downto bus_width) <= read_buf(wide_width-bus_width-bus_width-1 downto 0);
      end if;
    end if;
  end process;

  
  l1: if LEVEL = 1 generate
    read_buf_en <= mode when (addr /= nextaddr) else '0';
  end generate;

  l2: if LEVEL = 2 generate
    read_buf_en <= dma_re;
    chunk <= std_ulogic_vector(unsigned(chunk_w) sll shift);
    stride <= stride_w;
    start <= start_w;
  end generate;

  process (size_w)
  begin
    shift <= 0;
     for i in 1 to extbits loop
       if (size_w(bus_addrwidth-1+i) = '1') then
         shift <= i;
       end if;
     end loop;
  end process;

  count <= std_ulogic_vector(unsigned(count_w) sll shift);

  process (clk)
  begin    
    if clk'event and clk = '1' then
      if (write_new = '1') then
        addr_1 <= addr;
      end if;
    end if;
  end process;
  
  addr_w <= addr when mode='1' else addr_1;
  nextaddr_w <= nextaddr;

  idata_w <= write_buf;
  odata <=
    odata_w(wide_width-1 downto wide_width-bus_width) when read_buf_load = '1' else
    read_buf(wide_width-bus_width-1 downto wide_width-bus_width-bus_width);
  
  process (clk)
  begin    
    if clk'event and clk = '1' then
      if reset = '1' or req_w = '0' or end_write = '1' then
        write_new <= '1';
      elsif wri = '1' then
        write_new <= '0';
      end if;
    end if;
  end process;

  process (clk)
  begin    
    if clk'event and clk = '1' then
      wri_w <= end_write;
    end if;
  end process;
  
  process (clk)
  begin    
    if clk'event and clk = '1' then
      ack_w <= ack;
    end if;
  end process;

  req <= req_w and not ack_w;
end a1;

--
-- impc_mem_level2
-- implement slice, chunk, stride, start
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity impc_mem_level2 is
    generic (
      size2_width : natural
    );
    port (
      reset : in std_ulogic;
      clk : in std_ulogic;
      req : in std_ulogic;
      addr1 : in std_ulogic_vector(31 downto 0);
      nextaddr1 : in std_ulogic_vector(31 downto 0);
      wri1 : in std_ulogic;
      ack1 : in std_ulogic;
      base1 : out std_ulogic_vector(31 downto 0);
      start1 : out std_ulogic_vector(31 downto 0);
      count1 : out std_ulogic_vector(31 downto 0);
      slice2 : in std_ulogic;
      base2 : in std_ulogic_vector(31 downto 0);
      chunk2 : in std_ulogic_vector(31 downto 0);
      stride2 : in std_ulogic_vector(31 downto 0);
      start2 : in std_ulogic_vector(31 downto 0);
      count2 : in std_ulogic_vector(31 downto 0);
      size2 : in std_ulogic_vector(size2_width-1 downto 0);
      addr2 : out std_ulogic_vector(31 downto 0);
      nextaddr2 : out std_ulogic_vector(31 downto 0);
      wri2 : out std_ulogic;
      ack2 : out std_ulogic
    );
end;

architecture a1 of impc_mem_level2 is
  signal elm_addr, elm_nextaddr : unsigned(31 downto 0);
  signal addr_byte, nextaddr_byte : unsigned(31 downto 0);
  signal addr_out, nextaddr_out : unsigned(31 downto 0);
begin  -- a1
  start1 <= X"00000000";
  base1 <= base2;
  count1 <= count2;

  elm_addr <= unsigned(addr1);
  elm_nextaddr <= unsigned(nextaddr1);

  -- level 2 represents addresses in bytes
  addr_byte <= elm_addr(31-size2_width downto 0) * unsigned(size2);
  nextaddr_byte <= elm_nextaddr(31-size2_width downto 0) * unsigned(size2);

  -- level 2 supports a start address (also in bytes)
  addr_out <= addr_byte+unsigned(start2);
  nextaddr_out <= nextaddr_byte+unsigned(start2);

  addr2 <= std_ulogic_vector(addr_out);
  nextaddr2 <= std_ulogic_vector(nextaddr_out);

  wri2 <= wri1;
  ack2 <= ack1;

end a1;

library ieee;
use ieee.std_logic_1164.all;
         
package mem_if_components is
component impc_mem_expander is
    generic (
      LEVEL : natural;
      bus_width : natural;
      bus_addrwidth : natural;
      wide_width : natural;
      wide_addrwidth : natural
    );
    port (
      reset : in std_ulogic;
      clk : in std_ulogic;
      mode : in std_ulogic;
      idata : in std_ulogic_vector(bus_width-1 downto 0);
      wri : in std_ulogic;
      ack : in std_ulogic;
      addr : in std_ulogic_vector(31 downto 0);
      nextaddr : in std_ulogic_vector(31 downto 0);
      dma_re : in std_ulogic;
      req : out std_ulogic;
      size : out std_ulogic_vector(bus_addrwidth-1 downto 0);
      chunk : out std_ulogic_vector(31 downto 0);
      stride : out std_ulogic_vector(31 downto 0);
      start : out std_ulogic_vector(31 downto 0);
      count : out std_ulogic_vector(31 downto 0);
      odata : out std_ulogic_vector(bus_width-1 downto 0);
      odata_w : in std_ulogic_vector(wide_width-1 downto 0);
      size_w : in std_ulogic_vector(wide_addrwidth-1 downto 0);
      chunk_w : in std_ulogic_vector(31 downto 0);
      stride_w : in std_ulogic_vector(31 downto 0);
      start_w : in std_ulogic_vector(31 downto 0);
      count_w : in std_ulogic_vector(31 downto 0);
      req_w : in std_ulogic;
      wri_w : out std_ulogic;
      ack_w : inout std_ulogic;
      addr_w : inout std_ulogic_vector(31 downto 0);
      nextaddr_w : inout std_ulogic_vector(31 downto 0);
      idata_w : out std_ulogic_vector(wide_width-1 downto 0)
      );
end component;

component impc_mem_level2 is
    generic (
      size2_width : natural
    );
    port (
      reset : in std_ulogic;
      clk : in std_ulogic;
      req : in std_ulogic;
      addr1 : in std_ulogic_vector(31 downto 0);
      nextaddr1 : in std_ulogic_vector(31 downto 0);
      wri1 : in std_ulogic;
      ack1 : in std_ulogic;
      base1 : out std_ulogic_vector(31 downto 0);
      start1 : out std_ulogic_vector(31 downto 0);
      count1 : out std_ulogic_vector(31 downto 0);
      slice2 : in std_ulogic;
      base2 : in std_ulogic_vector(31 downto 0);
      chunk2 : in std_ulogic_vector(31 downto 0);
      stride2 : in std_ulogic_vector(31 downto 0);
      start2 : in std_ulogic_vector(31 downto 0);
      count2 : in std_ulogic_vector(31 downto 0);
      size2 : in std_ulogic_vector(size2_width-1 downto 0);
      addr2 : out std_ulogic_vector(31 downto 0);
      nextaddr2 : out std_ulogic_vector(31 downto 0);
      wri2 : out std_ulogic;
      ack2 : out std_ulogic
    );
end component;
end mem_if_components;
