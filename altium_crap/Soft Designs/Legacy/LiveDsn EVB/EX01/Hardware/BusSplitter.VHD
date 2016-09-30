------------------------------------------------------------
-- SubModule BusSplitter.VHD
-- Created   23/08/2004 3:13:26 PM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity BusSplitter is port
   (
     Input        : in  std_logic_vector(7 downto 0);
     OutputB      : out std_logic_vector(4 downto 0);
     Output5S     : out std_logic;
     Output7S     : out std_logic
   );
end BusSplitter;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of BusSplitter is

-- Component Declarations

-- Signal Declarations

begin

  OutputB  <= Input(4 downto 0);
  Output5S <= Input(5);
  Output7S <= Input(7);

end structure;
------------------------------------------------------------
