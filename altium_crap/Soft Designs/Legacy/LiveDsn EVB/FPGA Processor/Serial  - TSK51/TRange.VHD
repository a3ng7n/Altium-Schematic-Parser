------------------------------------------------------------
-- SubModule TRange
-- Created   15/11/2003 12:44:20 AM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all; 

entity TRange is port
   (
     lower       : in    std_logic_vector(15 downto 0);
     data        : in    std_logic_vector(15 downto 0);
     upper       : in    std_logic_vector(15 downto 0);
     -----     
     isBetween     : out   std_logic;
     equalsLower   : out   std_logic;
     equalsUpper   : out   std_logic
   );
end TRange;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of TRange is
begin
    isBetween <= '1' when (data>=lower) and (data<=upper) else '0';
    equalsLower   <= '1' when (data=lower) else '0';
    equalsUpper   <= '1' when (data=upper) else '0';
end structure;
------------------------------------------------------------
