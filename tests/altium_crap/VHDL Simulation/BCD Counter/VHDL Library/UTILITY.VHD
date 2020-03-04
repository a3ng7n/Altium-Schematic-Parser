library IEEE;
use IEEE.STD_LOGIC_1164.all;

package UTILITY IS

    function fparity (vtctp : std_logic_vector) return std_logic;

end Utility;

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package body UTILITY is

    function fparity (vtctp : std_logic_vector) return std_logic is
        variable respar : std_logic;
    begin
        respar := '0';
        for i in 0 to vtctp'length - 1 loop
            if vtctp(i) = '1' then
                respar := not respar;
            end if;
        end loop;
        return respar;
    end function fparity;

end package body UTILITY;