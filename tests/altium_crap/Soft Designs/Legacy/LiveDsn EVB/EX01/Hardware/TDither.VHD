------------------------------------------------------------
-- SubModule TDither
-- Created   26/08/2004 5:25:24 PM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity TDither is port
   (
     CIN        : in    std_logic_vector(3 downto 0);
     COUT       : out   std_logic_vector(2 downto 0);
     CLK        : in    std_logic;
     PWMEN      : in    std_logic
   );
end TDither;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of TDither is
signal clow : std_logic_vector(2 downto 0);
signal chigh : std_logic_vector(2 downto 0);

begin
     clow  <= "000" when CIN="0000" else
              "000" when CIN="0001" else
              "001" when CIN="0010" else
              "001" when CIN="0011" else
              "010" when CIN="0100" else
              "010" when CIN="0101" else
              "011" when CIN="0110" else
              "011" when CIN="0111" else
              "100" when CIN="1000" else
              "100" when CIN="1001" else
              "101" when CIN="1010" else
              "101" when CIN="1011" else
              "110" when CIN="1100" else
              "110" when CIN="1101" else
              "111" when CIN="1110" else
              "111" when CIN="1111" else "000";

     chigh <= "000" when CIN="0000" else
              "001" when CIN="0001" else
              "001" when CIN="0010" else
              "010" when CIN="0011" else
              "010" when CIN="0100" else
              "011" when CIN="0101" else
              "011" when CIN="0110" else
              "100" when CIN="0111" else
              "100" when CIN="1000" else
              "101" when CIN="1001" else
              "101" when CIN="1010" else
              "110" when CIN="1011" else
              "110" when CIN="1100" else
              "111" when CIN="1101" else
              "111" when CIN="1110" else
              "111" when CIN="1111" else "000";
     cout <= clow when CLK='0' or PWMEN='0' else chigh;
end structure;
------------------------------------------------------------
