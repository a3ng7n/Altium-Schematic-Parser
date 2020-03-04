--------------------------------------------------------------------------------
-- SubModule shiftreg
-- Created   2010/02/17 12:24:48
--------------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;

entity shiftreg is port
   (
     PARIN        : in    std_logic_vector(15 downto 0);
     START        : in    std_logic;
     READY        : out   std_logic;
     SEROUT       : out   std_logic;
     SCLK_I       : in    std_logic;
     RST_I        : in    std_logic;
     CLK_I        : in    std_logic
   );
end shiftreg;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
architecture Structure of shiftreg is

-- Component Declarations

-- Signal Declarations

begin

process( CLK_I )
variable state : integer range 0 to PARIN'length + 1;
variable shiftreg : std_logic_vector( PARIN'range );
variable prevclk : std_logic;
begin
    if rising_edge( CLK_I ) then
        if RST_I = '1' then
            READY <= '1';
            shiftreg := (others => '1');
            state := 0;
            SEROUT <= '1';
        elsif prevclk = '0' and SCLK_I = '1' then
            if state = 0 then
                SEROUT <= '1';
                if START = '1' then
                    shiftreg := PARIN;
                    READY <= '0';
                    state := 1;
                else
                    shiftreg( shiftreg'left ) := '1';
                end if;
            elsif state <= PARIN'length then
                SEROUT <= shiftreg( 0 );
                shiftreg := '1' & shiftreg( shiftreg'left downto 1 );
                state := state + 1;
            else
                SEROUT <= '1';
                READY <= '1';
                if START = '0' then
                    state := 0;
                end if;
            end if;
        end if;
        prevclk := SCLK_I;
    end if;
end process;

end Structure;
--------------------------------------------------------------------------------
