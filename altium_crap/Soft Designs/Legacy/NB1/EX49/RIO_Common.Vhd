------------------------------------------------------------
-- SubModule RIO_COMMON.VHD
--
--  Declares common constants for project
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
 
package RIO_COMMON is
-- COMMON CONSTANTS
  constant C_HEADERBODY_INT: std_logic_vector(15 downto 0) := x"CCDD";      --message header character
  constant C_HEADEREND_INT: std_logic_vector(15 downto 0) := x"EEFF";       --message header end character
  constant C_NUMBEROF_HEADERCHARS : positive := 12;                         -- number of message header characters transmitted
                                                                            -- before Header end character

-- RECEIVER CONSTANTS
  constant C_RAM_START_ADDRESS : std_logic_vector(15 downto 0) := x"0000";  -- start address in SRAM for Received data
  constant C_RAM_END_ADDRESS   : std_logic_vector(15 downto 0) := x"8FFF";  -- end   address in SRAM for Received data            

end package RIO_COMMON;
