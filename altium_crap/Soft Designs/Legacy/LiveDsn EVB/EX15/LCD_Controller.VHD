------------------------------------------------------------
-- SubModule LCD_Controller
-- Created   24/08/2004 4:05:03 PM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity LCD_Controller is port
   (
     -- Processor interface
     LCD_E           : in    std_logic;
     LCD_RW          : in    std_logic;
     LCD_RS          : in    std_logic;
     LCD_DB_IN       : in    std_logic_vector(7 downto 0);
     LCD_DB_OUT      : out   std_logic_vector(7 downto 0);

     -- Memory interface
     MEM_W           : out   std_logic;
     MEM_AD          : out   std_logic_vector(6 downto 0);
     MEM_DB          : out   std_logic_vector(7 downto 0)
   );
end LCD_Controller;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of LCD_Controller is

signal AC : std_logic_vector(7 downto 0);
signal IR : std_logic_vector(7 downto 0);
signal DR : std_logic_vector(7 downto 0);

begin
  cpu_interface:process (LCD_E)
  begin
     if (LCD_E'event) and (LCD_E='0') then
         if LCD_RW = '0' then -- Write date
            if LCD_RS='0' then -- Write command register
               if  LCD_DB_IN="00000001" then               -- Clear command
                  AC <= "00000000"; --return home
               elsif LCD_DB_IN(7 downto 1)= "0000001" then -- Return home command
                  AC <= "00000000"; --return home
               elsif LCD_DB_IN(7) ='1' then                -- Set DRAM address
                  AC <= '0' & LCD_DB_IN(6 downto 0);
               end if;
            else   -- Write Data Register
                   DR <= LCD_DB_IN;
                   AC <= AC+1; -- Autoincrement
            end if;
         else  -- Read data
         end if;
     end if;
  end process;
  Mem_W <= LCD_E when LCD_RS='1' else '0';
  MEM_AD <= AC(6 downto 0);
  MEM_DB <= LCD_DB_IN;
  LCD_DB_OUT <= "00000000";

end structure;
------------------------------------------------------------
