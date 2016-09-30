library IEEE;
use IEEE.std_logic_1164.all;

entity EXT_MEM_CTRL is 
port (
    -- Selects the behaviour of the two external memory chips
    MEM_SELECT    : in   std_logic ;
    -- External data/program Memory control signals - control from the core
    E_RAM_WR      : in    std_logic;
    E_RAM_RD      : in    std_logic;
    E_ROM_WR      : in    std_logic;
    E_ROM_RD      : in    std_logic;    
    E_RAM_DATAI   : in    std_logic_vector(7 downto 0);
    E_RAM_DATAO   : out   std_logic_vector(7 downto 0);
    E_RAM_ADDR    : in    std_logic_vector(15 downto 0);

    -- External memory interface - going directly to the memory chip
    EXT_RAM0_DATA : inout std_logic_vector(7 downto 0);
    EXT_RAM0_WR   : out   std_logic;    
    EXT_RAM0_OE   : out   std_logic;
    
    EXT_RAM1_DATA : inout std_logic_vector(7 downto 0);
    EXT_RAM1_WR   : out   std_logic;    
    EXT_RAM1_OE   : out   std_logic;
    
    EXT_ADDRESS   : out   std_logic_vector(16 downto 0);
    EXT_CS        : out   std_logic
);
end EXT_MEM_CTRL;
------------------------------------------------------------

------------------------------------------------------------
architecture rtl of EXT_MEM_CTRL is
begin
    -- if MEM_SELECT is 1 then 
    -- RAM0_DATA is the data memory
    -- RAM1_DATA is the program memory    
    -- RAM_DATA is connected to RAM0_DATA which is the data memory
    -- ROM_DATA is connected to RAM1_DATA which is the program memory    
    -- the RAM0 control signals are tied to the processor RAM
    -- the RAM1 control signals are tied to the processor ROM    
    --
    -- otherwise               
    --
    -- RAM1_DATA is the data memory
    -- RAM0_DATA is the program memory        
    -- RAM_DATA is connected to RAM1_DATA which is the data memory
    -- ROM_DATA is connected to RAM0_DATA which is the program memory        
    -- the RAM0 control signals are tied to the processor ROM
    -- the RAM1 control signals are tied to the processor RAM        
    --
    
    process(E_RAM_WR, E_ROM_WR, E_RAM_RD, E_ROM_RD, MEM_SELECT, E_RAM_DATAI, EXT_RAM0_DATA, EXT_RAM1_DATA)
    begin
       if MEM_SELECT = '1' then
          if E_RAM_WR = '1' then 
             EXT_RAM0_DATA <= E_RAM_DATAI;
          else
             EXT_RAM0_DATA <= (others=>'Z');
          end if;
          
          if E_ROM_WR = '1' then 
             EXT_RAM1_DATA <= E_RAM_DATAI;
          else
             EXT_RAM1_DATA <= (others=>'Z');
          end if;                   
          
          if E_RAM_RD = '1' then   
             E_RAM_DATAO <= EXT_RAM0_DATA;
          elsif E_ROM_RD = '1' then
             E_RAM_DATAO <= EXT_RAM1_DATA;
          else
             E_RAM_DATAO <= (others  => '-');
          end if;          
          
          EXT_RAM0_WR <= not E_RAM_WR;
          EXT_RAM0_OE <= not E_RAM_RD;
          EXT_RAM1_WR <= not E_ROM_WR;
          EXT_RAM1_OE <= not E_ROM_RD;          
       else
          if E_RAM_WR = '1' then
             EXT_RAM1_DATA <= E_RAM_DATAI;
          else
             EXT_RAM1_DATA <= (others=>'Z');
          end if;
          
          if E_ROM_WR = '1' then
             EXT_RAM0_DATA <= E_RAM_DATAI;
          else
             EXT_RAM0_DATA <= (others=>'Z');
          end if;          
          
          if E_RAM_RD = '1' then
             E_RAM_DATAO <= EXT_RAM1_DATA;
          elsif E_ROM_RD = '1' then
             E_RAM_DATAO <= EXT_RAM0_DATA;
          else
             E_RAM_DATAO <= (others  => '-');
          end if;          
          
          EXT_RAM0_WR <= not E_ROM_WR;
          EXT_RAM0_OE <= not E_ROM_RD;
          EXT_RAM1_WR <= not E_RAM_WR;
          EXT_RAM1_OE <= not E_RAM_RD;          
       end if;
    end process;

    -- Chip select of the external memory is active low
    EXT_CS <= '0' ;
    
    -- Address from the processor is connected straight to the external address
    EXT_ADDRESS <= '0' & E_RAM_ADDR;
end rtl;
------------------------------------------------------------
