------------------------------------------------------------
-- SubModule SRAM_INT.VHDL
-- Created   1/09/2004 2:56:53 PM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity SRAM_INT is port
   (
     SRAM_DL       : inout std_logic_vector(15 downto 0); -- left channel data
     SRAM_DR       : inout std_logic_vector(15 downto 0); -- right channel data
     SRAM_E        : out   std_logic;
     SRAM_A        : out   std_logic_vector(18 downto 0);
     SRAM_WL       : out   std_logic;                     -- left channel write signal
     SRAM_WR       : out   std_logic;                     -- right channel write signal
     SRAM_R        : out   std_logic;
     SRAM_UB       : out   std_logic;
     SRAM_LB       : out   std_logic;
     BANK          : in    std_logic_vector(3 downto 0);
     W8r           : in    std_logic;
     D8IN          : in    std_logic_vector(7 downto 0);
     A8D           : in    std_logic_vector(15 downto 0);
     D16OL         : out   std_logic_vector(15 downto 0); -- left  channel data output
     D16OR         : out   std_logic_vector(15 downto 0); -- right channel data output
     R16d          : in    std_logic;
     A16D          : in    std_logic_vector(31 downto 0);
     MODE16        : in    std_logic;
     LEFT_C        : in    std_logic;                     -- writes go to left channel ram if set
     EN            : in    std_logic
   );
end SRAM_INT;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of SRAM_INT is

-- Component Declarations

-- Signal Declarations

begin

SRAM_LB <= '0' when MODE16 = '1' else (A8D(0));
SRAM_UB <= '0' when MODE16 = '1' else (NOT A8D(0));
SRAM_E  <= '0';
SRAM_A  <= A16D(18 downto 0) when MODE16 = '1' else BANK & A8D(15 downto 1);
SRAM_R  <= '0' when MODE16 = '1' else '1';

SRAM_WL <= '0' when (MODE16 = '0' AND EN = '1' AND W8r = '1' AND LEFT_C = '1') else '1';
SRAM_WR <= '0' when (MODE16 = '0' AND EN = '1' AND W8r = '1' AND LEFT_C = '0') else '1';

SRAM_DL <= D8IN & D8IN when (MODE16 = '0' AND EN = '1' AND W8r = '1') else (others => 'Z');
SRAM_DR <= D8IN & D8IN when (MODE16 = '0' AND EN = '1' AND W8r = '1') else (others => 'Z');

--D16OL   <= SRAM_DL(7 downto 0) & SRAM_DL(15 downto 8) when MODE16 = '1';
--D16OR   <= SRAM_DR(7 downto 0) & SRAM_DR(15 downto 8) when MODE16 = '1';

--D16OL   <= "0000" & SRAM_DL(7 downto 0) & SRAM_DL(15 downto 12) when MODE16 = '1';
--D16OR   <= "0000" & SRAM_DR(7 downto 0) & SRAM_DR(15 downto 12) when MODE16 = '1';

--D16OL   <= "0000" & SRAM_DL(15 downto 8) & SRAM_DL(7 downto 4) when MODE16 = '1';
--D16OR   <= "0000" & SRAM_DR(15 downto 8) & SRAM_DR(7 downto 4) when MODE16 = '1';

--D16OL   <= "0000" & SRAM_DL(15 downto 12) & SRAM_DL(11 downto 4) when MODE16 = '1';
--D16OR   <= "0000" & SRAM_DR(15 downto 12) & SRAM_DR(11 downto 4) when MODE16 = '1';

D16OL   <= SRAM_DL(15 downto 8) & SRAM_DL(7 downto 0) when MODE16 = '1' else X"8000";
D16OR   <= SRAM_DR(15 downto 8) & SRAM_DR(7 downto 0) when MODE16 = '1' else X"8000";



end structure;
------------------------------------------------------------
