
---------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity BUFGS is
  port(I : in std_logic;
       O : out std_logic);
end BUFGS;

architecture RTL of BUFGS is
begin
  O <= I;
end;
---------------------------------------------------------------------