--------------------------------------------------------------------------------
-- SubModule LED_INT
-- Created   20/12/2004 2:16:59 PM
--------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.all;

entity LED_INT is port
   (
     DIG0       : out   std_logic_vector(7 downto 0);
     DIG1       : out   std_logic_vector(7 downto 0);
     DIG2       : out   std_logic_vector(7 downto 0);
     DIG3       : out   std_logic_vector(7 downto 0);
     DIG4       : out   std_logic_vector(7 downto 0);
     DIG5       : out   std_logic_vector(7 downto 0);
     P32        : in    std_logic_vector(31 downto 0)
   );
end LED_INT;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
architecture Structure of LED_INT is

begin
    DIG0 <= "00" & P32(0)  & P32(16)  & '0' & P32(17) & P32(1)  & '0';
    DIG1 <= "00" & P32(2)  & P32(18)  & '0' & P32(19) & P32(3)  & '0';
    DIG2 <= "00" & P32(4)  & P32(20)  & '0' & P32(21) & P32(5)  & '0';
    DIG3 <= "00" & P32(6)  & P32(22)  & '0' & P32(23) & P32(7)  & '0';
    DIG4 <= "00" & P32(8)  & P32(24)  & '0' & P32(25) & P32(9)  & '0';
    DIG5 <= "00" & P32(10) & P32(26)  & '0' & P32(27) & P32(11) & '0';
end Structure;
--------------------------------------------------------------------------------
