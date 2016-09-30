
-- BCD.VHD
-- Binary Coded Decimal Counter (0-9) with RCO

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.std_logic_unsigned.all;


--------------------BCD-----------------------------------------
entity BCD is
  port(CLEAR,CLOCK,ENABLE: in  std_logic;
          RCO:             out std_logic;
          OCD:             out std_logic_vector(3 downto 0));
end;

architecture RTL of BCD is
  signal CURRENT_COUNT,NEXT_COUNT: std_logic_vector(3 downto 0);
begin

  REGISTER_BLOCK: process (CLEAR,CLOCK,NEXT_COUNT)
  begin
     if (CLEAR='1') then
        CURRENT_COUNT<=x"0";
     elsif (CLOCK='1' and CLOCK'event) then
        CURRENT_COUNT<=NEXT_COUNT;
     end if;
  end process;

-- Binary Coded Decimal generator combinational logic block
  BCD_GENERATOR: process (CURRENT_COUNT,ENABLE)
  begin
     if (CURRENT_COUNT=x"9") and (ENABLE='1') then
        NEXT_COUNT<=x"0";
        RCO <= '1';
     else
        if (ENABLE='1') then
           NEXT_COUNT <= CURRENT_COUNT + 1;
        else
           NEXT_COUNT <= CURRENT_COUNT;
        end if;
        RCO <= '0';
     end if;
  end process;
  OCD <= CURRENT_COUNT;

end;
---------------------------------------------------------------------

---------------------------------------------------------------------