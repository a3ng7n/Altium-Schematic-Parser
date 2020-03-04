library ieee;
use ieee.std_logic_1164.all ;

entity short_int is
port (
   clk     : in  std_logic ;
   int     : in  std_logic ;
   rst     : in  std_logic ;
   LCDST   : in  std_logic ;
   int_out : out std_logic
);
end ;

architecture rtl of short_int is

signal int0, int1, int2, int3 : std_logic ;

begin

process ( clk )
begin
   if rising_edge(clk) then
      if rst='1' then
         int0 <= '0' ;
         int1 <= '0' ;
         int2 <= '0' ;
         int3 <= '0' ;
      else
         int0 <= int  ;
         int1 <= int0 ;
         int2 <= int1 ;
         int3 <= int2 ;
      end if ;
   end if ;
end process;


int_out <= (not int1) and int0;
end rtl ;
