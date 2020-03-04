------------------------------------------------------------
-- SubModule File Name
-- Created   25-6-2004 15:17:14
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity MEMIF is port
   (
     MEMDATAI        : in    std_logic_vector(7 downto 0);
     MEMDATAO        : out   std_logic_vector(7 downto 0);
     MEMADDR         : in    std_logic_vector(15 downto 0);
     MEMWR           : in    std_logic;
     MEMRD           : in    std_logic;
     PSWR            : in    std_logic;
     PSRD            : in    std_logic;
     DATAO           : out   std_logic_vector(7 downto 0);
     DATAI           : in    std_logic_vector(7 downto 0);
     ADDR            : out   std_logic_vector(10 downto 0);
     WR              : out   std_logic;
     RD              : out   std_logic;
     RAM0_DATA       : inout std_logic_vector(7 downto 0);
     RAM_CS          : out   std_logic;
     RAM_ADDR        : out   std_logic_vector(16 downto 0);
     RAM0_WE         : out   std_logic;
     RAM0_OE         : out   std_logic
   );
end MEMIF;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of MEMIF is

-- Component Declarations

-- Signal Declarations

begin

    RAM0_DATA <= MEMDATAI when MEMWR = '1' or PSWR = '1' else "ZZZZZZZZ";
    RAM_CS <= '1' when MEMADDR < 1536 and ( MEMWR = '1' or MEMRD = '1' ) else '0';
    RAM_ADDR(15 downto 0) <= MEMADDR;
    RAM_ADDR(16) <= '1' when PSWR = '1' or PSRD = '1' else '0';
    RAM0_WE <= '0' when PSWR = '1' or MEMWR = '1' else '1';
    RAM0_OE <= '0' when PSRD = '1' or MEMRD = '1' else '1';

    DATAO <= MEMDATAI;
    ADDR <= MEMADDR( 10 downto 0);
    WR <= '1' when MEMADDR < 1536 and MEMWR = '1' else '0';
    RD <= '1' when MEMADDR < 1536 and MEMRD = '1' else '0';

    MEMDATAO <= DATAI when MEMADDR < 1536 and MEMRD = '1' else RAM0_DATA;


end structure;
------------------------------------------------------------
