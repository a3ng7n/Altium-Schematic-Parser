-------------------------------------------------------
--- Submodule vgacfg.vhdl (VGA)
-------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
Use IEEE.std_logic_unsigned.all;

entity VGACFG is 
port
(
    RES         : out     std_logic;
    CMOD        : out     std_logic_vector(1 downto 0);
    VGAHSIZE    : out     std_logic_vector(9 downto 0);
    VGAVSIZE    : out     std_logic_vector(9 downto 0)
) ;
end VGACFG;

architecture RTL of VGACFG is
begin
    RES <= '1';
    CMOD <= "01";
    VGAHSIZE <= "1010000000";
    VGAVSIZE <= "0110010000";
end RTL;

