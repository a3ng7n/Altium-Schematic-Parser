library IEEE;
use IEEE.std_logic_1164.all;
Use IEEE.std_logic_unsigned.all;

entity mapCtl is          
port               
(
    -- Tile Map controller 
    
    
    clk         : in     std_logic;
    -- VGA I/F
    HBlank       : in     std_logic;                       
    pix          : out    std_logic_vector(7 downto 0);    -- video data
    pixX         : in     std_logic_vector(7 downto 0);    -- X location in byte addresses i.e. each address increment is 2 pix
    pixY         : in     std_logic_vector(8 downto 0);    
    -- Tile map I/F
    tileMapD    : in     std_logic_vector(7 downto 0);    
    tileMapA    : out    std_logic_vector(11 downto 0);    
    -- Tile data I/F
    tileD       : in     std_logic_vector(7 downto 0);    
    tileA       : out    std_logic_vector(11 downto 0)    
) ;
end mapCtl;

architecture beh of mapCtl is
begin

    -- generate tile map address from VGA raster
    tileMapA(11 downto 6) <=  pixY(8 downto 3);
    
    -- generate tile address from time map data
    tileA(11 downto 5) <= tileMapD(6 downto 0); -- assume top bit not used
    tileA(4 downto 2) <=  pixY(2 downto 0);   -- index into tile
    
    -- generate pixel
    process (clk, HBlank)
        -- the VGA controller assumes that data is valid on tha same clk the address goes out (i.e. no pipeline)
        -- as we are using synchronous RAM we have an address to data delay.
        -- the delay is 2 clock, one for tileMap and once for tile, however @ 2 bits per pixel there is a
        -- 2 clock delay in the VGA controller anyway
        -- so we need a horizontal count that is 1 ahead of the VGA counter.
        -- this is achieved by sending a 0 address when in blanking and starting the count @ 1 as soon as we are out of blanking.
        -- a better solution would be for the VGA controller to have a pipeline option.
    
        variable hcount : std_logic_vector(8 downto 0); -- counter is one wider than VGA counter to allow counting every clock
                                                        -- VGA counter will count every two clock due to 2 bit per pix
    begin
        if rising_edge(clk) then
            if HBlank = '1' then
                hcount := "000000010";  -- reset to 1 (shifted up for two clocks)
            else
                hcount := hcount + 1;
            end if;
        end if;

        if HBlank = '1' then
            tileMapA(5 downto 0) <=  "000000";
            tileA(1 downto 0) <=  "00";  
        else
            tileMapA(5 downto 0) <=  hcount(8 downto 3);
            tileA(1 downto 0) <=  hcount(2 downto 1);  
        end if;
        
    end process;
    pix <= tileD;
end beh;

