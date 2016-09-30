library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_unsigned.all;

entity INPORT2x8 is
    generic (
           PORT0ADDR : integer;
           PORT1ADDR : integer
      );
    port(
         OE             : in    STD_LOGIC;
         ADDR           : in    STD_LOGIC_VECTOR(7 downto 0);
         DATAO          : out   STD_LOGIC_VECTOR(7 downto 0);
         P0_IN          : in    STD_LOGIC_VECTOR(7 downto 0);
         P1_IN          : in    STD_LOGIC_VECTOR(7 downto 0)
         );
end INPORT2x8;

architecture INPORT2x8_ARCH of INPORT2x8 is
      
begin

   --------------------------------------------------------------------
   -- Ports read
   --------------------------------------------------------------------
   ports_read :
   --------------------------------------------------------------------
      DATAO <=
         P0_IN       when CONV_INTEGER(ADDR)=PORT0ADDR and OE='1' else
         P1_IN       when CONV_INTEGER(ADDR)=PORT1ADDR and OE='1' else
         "00000000";

end INPORT2x8_ARCH;
