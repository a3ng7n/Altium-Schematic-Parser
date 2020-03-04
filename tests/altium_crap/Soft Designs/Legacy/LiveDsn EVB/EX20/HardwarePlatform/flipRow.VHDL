library IEEE;
use IEEE.std_logic_1164.all;
Use IEEE.std_logic_unsigned.all;

entity flipRow is 
port
(
    rowIn   : in     std_logic_vector(31 downto 0);
    flip    : in     std_logic;
    rowOut  : out    std_logic_vector(31 downto 0)
) ;
end flipRow;

architecture RTL of flipRow is
begin
    
    -- swap row Left to Right in 2 bit chunks
    rowOut(31 downto 30) <= rowIn( 1 downto  0) when flip = '1' else rowIn(31 downto 30);
    rowOut(29 downto 28) <= rowIn( 3 downto  2) when flip = '1' else rowIn(29 downto 28);
    rowOut(27 downto 26) <= rowIn( 5 downto  4) when flip = '1' else rowIn(27 downto 26);
    rowOut(25 downto 24) <= rowIn( 7 downto  6) when flip = '1' else rowIn(25 downto 24);
    rowOut(23 downto 22) <= rowIn( 9 downto  8) when flip = '1' else rowIn(23 downto 22);
    rowOut(21 downto 20) <= rowIn(11 downto 10) when flip = '1' else rowIn(21 downto 20);
    rowOut(19 downto 18) <= rowIn(13 downto 12) when flip = '1' else rowIn(19 downto 18);
    rowOut(17 downto 16) <= rowIn(15 downto 14) when flip = '1' else rowIn(17 downto 16);
    rowOut(15 downto 14) <= rowIn(17 downto 16) when flip = '1' else rowIn(15 downto 14);
    rowOut(13 downto 12) <= rowIn(19 downto 18) when flip = '1' else rowIn(13 downto 12);
    rowOut(11 downto 10) <= rowIn(21 downto 20) when flip = '1' else rowIn(11 downto 10);
    rowOut( 9 downto  8) <= rowIn(23 downto 22) when flip = '1' else rowIn( 9 downto  8);
    rowOut( 7 downto  6) <= rowIn(25 downto 24) when flip = '1' else rowIn( 7 downto  6);
    rowOut( 5 downto  4) <= rowIn(27 downto 26) when flip = '1' else rowIn( 5 downto  4);
    rowOut( 3 downto  2) <= rowIn(29 downto 28) when flip = '1' else rowIn( 3 downto  2);
    rowOut( 1 downto  0) <= rowIn(31 downto 30) when flip = '1' else rowIn( 1 downto  0);
end RTL;



