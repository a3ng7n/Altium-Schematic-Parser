library IEEE;
use IEEE.Std_Logic_1164.all;

entity J3x8_1x19 is
  port (
    IADDR : in std_logic_vector (15 downto 0);
    IBANK : in std_logic_vector (7  downto 0);
    OADDR : out std_logic_vector(18 downto 0);
    OUB   : out std_logic;
    OLB   : out std_logic;
    OCS0  : out std_logic;
    OCS1  : out std_logic
    );
end entity;

architecture STRUCTURE of J3x8_1x19 is
begin

  OADDR(0)  <= IADDR(0);
  OADDR(1)  <= IADDR(1);
  OADDR(2)  <= IADDR(2);
  OADDR(3)  <= IADDR(3);
  OADDR(4)  <= IADDR(4);
  OADDR(5)  <= IADDR(5);
  OADDR(6)  <= IADDR(6);
  OADDR(7)  <= IADDR(7);
  OADDR(8)  <= IADDR(8);
  OADDR(9)  <= IADDR(9);
  OADDR(10) <= IADDR(10);
  OADDR(11) <= IADDR(11);
  OADDR(12) <= IADDR(12);
  OADDR(13) <= IADDR(13);
  OADDR(14) <= IADDR(14);
  OADDR(15) <= IADDR(15);

  OLB       <= IBANK(0);
  OUB       <= not IBANK(0);
  OCS0      <= IBANK(1);
  OCS1      <= not IBANK(1);

  OADDR(16) <= IBANK(2);
  OADDR(17) <= IBANK(3);
  OADDR(18) <= IBANK(4);
end architecture;
