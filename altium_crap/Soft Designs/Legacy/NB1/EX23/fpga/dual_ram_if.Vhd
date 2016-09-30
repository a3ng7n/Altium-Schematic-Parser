-----------------------------------------------------------------
--- Submodule dual_mem_if.vhdl (RAM interface)
------------------------------------------------------

--**************
-- This component connects 2 memory interfaces to a shared RAM
-- Interface 0 has the higher priority, interface 1 is disabled if interface 0 wants access to the RAM (WE0 or OE0 = '1')
-- Otherwise interface 1 has access to the RAM (Handshake line HS1 becomes '1')
--**************

library IEEE;
use IEEE.std_logic_1164.all;
Use IEEE.std_logic_unsigned.all;

entity DUAL_RAM_IF is
port (
-- interface 0
    DATAI0          : in    std_logic_vector(7 downto 0);
    DATAO0          : out   std_logic_vector(7 downto 0);
    ADDR0           : in    std_logic_vector(17 downto 0);
    WE0             : in    std_logic;
    OE0             : in    std_logic;
    CS0             : in    std_logic;

-- interface 1
    DATAI1          : in    std_logic_vector(7 downto 0);
    DATAO1          : out   std_logic_vector(7 downto 0);
    ADDR1           : in    std_logic_vector(17 downto 0);
    WE1             : in    std_logic;
    OE1             : in    std_logic;
    CS1             : in    std_logic;
    HS1             : out   std_logic;

-- RAM interface
    RAM0_DATA       : inout std_logic_vector(7 downto 0);
    RAM1_DATA       : inout std_logic_vector(7 downto 0);
    RAM_ADDR        : out   std_logic_vector(16 downto 0);
    RAM0_WE         : out   std_logic;
    RAM1_WE         : out   std_logic;
    RAM0_OE         : out   std_logic;
    RAM1_OE         : out   std_logic;
    RAM_CS          : out   std_logic;

-- Clock line
    CLK             : in    std_logic
);
end DUAL_RAM_IF;


architecture DUAL_RAM_IF_Arch of DUAL_RAM_IF is

    signal SEL      : std_logic;

begin

    ADDR_TO_RAM: process( SEL, ADDR0, ADDR1 )
    begin
        if SEL = '0' then
            RAM_ADDR <= ADDR0(16 downto 0);
        else
            RAM_ADDR <= ADDR1(16 downto 0);
        end if;
    end process;


    DATA_TO_RAM: process( SEL, DATAI0, DATAI1, ADDR0, ADDR1, WE0, WE1 )
    begin
        if SEL = '0' then
            if WE0 = '1' then
                if ADDR0(17) = '0' then
                    RAM0_DATA <= DATAI0;
                    RAM1_DATA <= (others => 'Z');
                else
                    RAM1_DATA <= DATAI0;
                    RAM0_DATA <= (others => 'Z');
                end if;
            else
                RAM0_DATA <= (others => 'Z');
                RAM1_DATA <= (others => 'Z');
            end if;
        else
            if WE1 = '1' then
                if ADDR1(17) = '0' then
                    RAM0_DATA <= DATAI1;
                    RAM1_DATA <= (others => 'Z');
                else
                    RAM1_DATA <= DATAI1;
                    RAM0_DATA <= (others => 'Z');
                end if;
            else
                RAM0_DATA <= (others => 'Z');
                RAM1_DATA <= (others => 'Z');
            end if;
        end if;
    end process;



    DATA_FROM_RAM: process( SEL, RAM0_DATA, RAM1_DATA, OE0, OE1, ADDR0, ADDR1 )
    begin
        if SEL = '0' then
            if OE0 = '1' then
                if ADDR0(17) = '0' then
                    DATAO0 <= RAM0_DATA;
                else
                    DATAO0 <= RAM1_DATA;
                end if;
            else
                DATAO0 <= (others => '0' );
            end if;
            DATAO1 <= (others => '0' );
        else
            if OE1 = '1' then
                if ADDR1(17) = '0' then
                    DATAO1 <= RAM0_DATA;
                else
                    DATAO1 <= RAM1_DATA;
                end if;
            else
                DATAO1 <= (others => '0' );
            end if;
            DATAO0 <= (others => '0' );
        end if;
    end process;


    WE_TO_RAM: process( SEL, ADDR0, ADDR1, WE0, WE1 )
    begin
        if SEL = '0' then
            if ADDR0(17) = '0' then
                RAM0_WE <= not WE0;
                RAM1_WE <= '1';
            else
                RAM1_WE <= not WE0;
                RAM0_WE <= '1';
            end if;
        else
            if ADDR1(17) = '0' then
                RAM0_WE <= not WE1;
                RAM1_WE <= '1';
            else
                RAM1_WE <= not WE1;
                RAM0_WE <= '1';
            end if;
        end if;
    end process;



    OE_TO_RAM: process( SEL, ADDR0, ADDR1, OE0, OE1 )
    begin
        if SEL = '0' then
            if ADDR0(17) = '0' then
                RAM0_OE <= not OE0;
                RAM1_OE <= '1';
            else
                RAM1_OE <= not OE0;
                RAM0_OE <= '1';
            end if;
        else
            if ADDR1(17) = '0' then
                RAM0_OE <= not OE1;
                RAM1_OE <= '1';
            else
                RAM1_OE <= not OE1;
                RAM0_OE <= '1';
            end if;
        end if;
    end process;


    CS_TO_RAM: process( SEL, ADDR0, ADDR1, CS0, CS1 )
    begin
        if SEL = '0' then
            RAM_CS <= not CS0;
        else
            RAM_CS <= not CS1;
        end if;
    end process;


    CONTROL_FF: process( CLK )
    begin
        if rising_edge( CLK ) then
            SEL <= not WE0 and not OE0;
        end if;
    end process;


    HS1 <= SEL and not WE0 and not OE0;


end DUAL_RAM_IF_Arch;



