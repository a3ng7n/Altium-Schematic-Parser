library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_unsigned.all;

entity OUTPORT2x8 is
    generic (
           PORT0ADDR : integer;
           PORT1ADDR : integer
      );
    port(                                 
         CLK            : in    STD_LOGIC;
         RESET          : in    STD_LOGIC;
         WE             : in    STD_LOGIC;
         ADDR           : in    STD_LOGIC_VECTOR(7 downto 0);
         DATAI          : in    STD_LOGIC_VECTOR(7 downto 0);
         P0_OUT         : out   STD_LOGIC_VECTOR(7 downto 0);
         P1_OUT         : out   STD_LOGIC_VECTOR(7 downto 0)
         );
end OUTPORT2x8;

architecture OUTPORT2x8_ARCH of OUTPORT2x8 is

      -- Port registers
      signal port0      : STD_LOGIC_VECTOR (7 downto 0);
      signal port1      : STD_LOGIC_VECTOR (7 downto 0);

begin

   --------------------------------------------------------------------
   -- Ports
   -- Registered outputs
   --------------------------------------------------------------------
   p0_out_drv: 
      P0_OUT <= port0;
   p1_out_drv:
      P1_OUT <= port1;

   --------------------------------------------------------------------
   ports_write_proc:
   --------------------------------------------------------------------
      process (clk)
      begin
         if clk'event and clk='0' then
         
            -------------------------------------
            -- Synchronous reset
            -------------------------------------
            if RESET='1' then
               port0 <= (OTHERS => '0');
               port1 <= (OTHERS => '0');
            else
            
            -------------------------------------
            -- Synchronous write
            -------------------------------------
               if (CONV_INTEGER(ADDR)=PORT0ADDR and WE='1') then
                  port0 <= DATAI;
               end if;
               if (CONV_INTEGER(ADDR)=PORT1ADDR and WE='1') then
                  port1 <= DATAI;
               end if;
            end if;  
         end if;
      end process;
      
end OUTPORT2x8_ARCH;
