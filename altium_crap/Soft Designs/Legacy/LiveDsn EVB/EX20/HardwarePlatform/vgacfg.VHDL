library IEEE;
use IEEE.std_logic_1164.all;
Use IEEE.std_logic_unsigned.all;

entity VGACFG is 
port
(
    RES         : out     std_logic;
    MODE        : out     std_logic_vector(1 downto 0);
    VGAHSIZE    : out     std_logic_vector(9 downto 0);
    VGAVSIZE    : out     std_logic_vector(9 downto 0)
) ;
end VGACFG;

architecture RTL of VGACFG is
begin
    RES <= '1'; -- 640X480
    MODE <= "01";  -- 16 colour 4 BIT MODE
    VGAHSIZE <= "1000000000";  -- 512   with current controller a power of 2 will ensure X,Y address can be extracted
    VGAVSIZE <= "0111100000";  -- 480
end RTL;



