------------------------------------------------------------
-- SubModule BarGraph.VHD
-- Created   10/09/2004 3:19:27 PM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity BarGraph is port
   (
     DIGITA       : out   std_logic_vector(7 downto 0);
     DIGITB       : out   std_logic_vector(7 downto 0);
     DIGITC       : out   std_logic_vector(7 downto 0);
     DIGITD       : out   std_logic_vector(7 downto 0);
     DIGITE       : out   std_logic_vector(7 downto 0);
     DIGITF       : out   std_logic_vector(7 downto 0);
     INL          : in    std_logic_vector(15 downto 0);
     INR          : in    std_logic_vector(15 downto 0)
   );
end BarGraph;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of BarGraph is

-- Component Declarations

-- Signal Declarations

signal LeftBits, RightBits : std_logic_vector(11 downto 0);
signal DiffL, DiffR: std_logic_vector (15 downto 0);

begin

DiffL <= (INL - X"8000") when INL >= X"8000" else (X"8000" - INL);  -- full wave rectify
DiffR <= (INR - X"8000") when INR >= X"8000" else (X"8000" - INR);  -- Full wave rectify

LeftBits(0)  <= '1' when (DIFFL) >= X"0104" else '0';
LeftBits(1)  <= '1' when (DIFFL) >= X"0107" else '0';
LeftBits(2)  <= '1' when (DIFFL) >= X"0207" else '0';
LeftBits(3)  <= '1' when (DIFFL) >= X"02DE" else '0';
LeftBits(4)  <= '1' when (DIFFL) >= X"040C" else '0';
LeftBits(5)  <= '1' when (DIFFL) >= X"0B68" else '0';
LeftBits(6)  <= '1' when (DIFFL) >= X"101D" else '0';
LeftBits(7)  <= '1' when (DIFFL) >= X"16C3" else '0';   --  -15dB
LeftBits(8)  <= '1' when (DIFFL) >= X"2027" else '0';   --  -12dB
LeftBits(9)  <= '1' when (DIFFL) >= X"2D6B" else '0';   --   -9dB
LeftBits(10) <= '1' when (DIFFL) >= X"4027" else '0';   --   -6dB
LeftBits(11) <= '1' when (DIFFL) >= X"5A9E" else '0';   --   -3dB

RightBits(0)  <= '1' when (DIFFR) >= X"0104" else '0';
RightBits(1)  <= '1' when (DIFFR) >= X"0107" else '0';
RightBits(2)  <= '1' when (DIFFR) >= X"0207" else '0';
RightBits(3)  <= '1' when (DIFFR) >= X"02DE" else '0';
RightBits(4)  <= '1' when (DIFFR) >= X"040C" else '0';
RightBits(5)  <= '1' when (DIFFR) >= X"0B68" else '0';
RightBits(6)  <= '1' when (DIFFR) >= X"101D" else '0';
RightBits(7)  <= '1' when (DIFFR) >= X"16C3" else '0';   --  -15dB
RightBits(8)  <= '1' when (DIFFR) >= X"2027" else '0';   --  -12dB
RightBits(9)  <= '1' when (DIFFR) >= X"2D6B" else '0';   --   -9dB
RightBits(10) <= '1' when (DIFFR) >= X"4027" else '0';   --   -6dB
RightBits(11) <= '1' when (DIFFR) >= X"5A9E" else '0';   --   -3dB

DigitA <= "00" & LeftBits(0)  & RightBits(0)  & '0' & RightBits(1)  & LeftBits(1)  & '0';
DigitB <= "00" & LeftBits(2)  & RightBits(2)  & '0' & RightBits(3)  & LeftBits(3)  & '0';
DigitC <= "00" & LeftBits(4)  & RightBits(4)  & '0' & RightBits(5)  & LeftBits(5)  & '0';
DigitD <= "00" & LeftBits(6)  & RightBits(6)  & '0' & RightBits(7)  & LeftBits(7)  & '0';
DigitE <= "00" & LeftBits(8)  & RightBits(8)  & '0' & RightBits(9)  & LeftBits(9)  & '0';
DigitF <= "00" & LeftBits(10) & RightBits(10) & '0' & RightBits(11) & LeftBits(11) & '0';

end structure;
------------------------------------------------------------
