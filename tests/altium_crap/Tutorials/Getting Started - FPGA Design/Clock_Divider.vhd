library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity Clock_Divider is
    port (
        CLK_REF  : in  std_logic;
        CLK_OUT : out std_logic
    );
end entity;

architecture RTL of Clock_Divider is
begin
    process(CLK_REF)
        variable i : integer range 0 to 999999;
    begin
        if rising_edge(CLK_REF) then
            if i = 0 then
                CLK_OUT <= '1';
                i := 999999;
            else
                CLK_OUT <= '0';
                i := i - 1;
            end if;
        end if;
    end process;
end architecture;
