---------------------------------------------------------------------------------------------------
--
-- Design       : Sprite Controller
-- Author       : Andrew Betzis
-- DATE         : 11/2003
-- ABSTRACT     : Controls the display of a sprite
--                  
---------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

use work.all;    

entity sptCtlVHDL is          
port               
(
    clk     : in     std_logic;

    -- VGA I/F
    HBlank  : in     std_logic;       
    xAddr   : in     std_logic_vector(7 downto 0);
    yAddr   : in     std_logic_vector(8 downto 0);
    dOut    : out    std_logic_vector(7 downto 0);
    pixOn   : out    std_logic_vector(1 downto 0);    
    

    -- Sprite I/F, sprite is 16 x 16 2 bit per pixel 00 is transparent, other 3 code map to 4 bit LUT
    num     : in    std_logic_vector(11 downto 0);   -- which sprite in table to show for this controller
    xLoc    : in    std_logic_vector(7 downto 0);   -- X location /2 (i.e. resolution of 2 pixels
    yLoc    : in    std_logic_vector(8 downto 0);   -- Y location (line)
    lut     : in    std_logic_vector(11 downto 0);  -- colour LUT for PEL.
    flags   : in    std_logic_vector(7 downto 0);   -- flags to operate on sprites

    ena     : in    std_logic;                      -- this sprite can load row data
    rowData : in    std_logic_vector(31 downto 0);  -- 16 x 2 bpp row of sprite data
    rowAddr : out   std_logic_vector(15 downto 0);   -- (16 rows of sprite data and 16 sprites ) full vector to allow expansion
    
    xMatch  : out    std_logic;                     -- raster between left edge of sprite and end of line
    yMatch  : out    std_logic                      -- raster between top and bottom of sprite
) ;
end sptCtlVHDL;

architecture beh of sptCtlVHDL is
    -- call up flipper
    component flipRow
    port
    (
        rowIn   : in     std_logic_vector(31 downto 0);
        flip    : in     std_logic;
        rowOut  : out    std_logic_vector(31 downto 0)
    ) ;
    end component;

   signal flipData : std_logic_vector(31 downto 0);   -- flipped row data

begin
    -- call up the flipper
    FLA : flipRow port map (rowIn => rowData, flip => flags(0), rowOut => flipData);

    process(clk, lut, num, flags)
        variable rowCount : std_logic_vector(4 downto 0);   -- count which row of sprite raster is on
        variable rowStore : std_logic_vector(31 downto 0);   -- saved row of spt to show during visibile period
        variable prevXAddr0 : std_logic;                    -- get delayed LESB to detect change   
        variable yMat  : std_logic;                         -- raster is between first and last line of sprite
        variable xMat  : std_logic;                         -- raster in between left edge and end of line
                                                            -- we dont detcted the end of sprite X, just shift out transparent as end
                                                            -- could add counter to count X if needed
        variable pel : std_logic_vector(4 downto 0);        -- picture element to use for lookup to display. 00 is transparent
    
    begin
        if rising_edge(clk) then
            if (yLoc = yAddr) then
                yMat := '1';            -- raster hit first line of spr
            elsif (rowCount = "10000") then
                yMat := '0';            -- all lines of sprite shown
            end if;


            if (yLoc = yAddr) and (HBlank = '1') then   -- ratest at first line during blanking
                rowCount := "00000";                    -- reset count
            elsif (HBlank = '0') and (xAddr = xLoc) and (xMat = '0')  then
                rowCount := rowCount+1;                 -- count up at left edge of each sprite
            end if;

            if (HBlank = '0') and (xAddr = xLoc)  then
                xMat := '1';    -- line visible and raster x matches spt loc
            elsif HBlank = '1' then
                xMat := '0';    -- at end of line
            end if;

            if ((yLoc = yAddr) or (yMat = '1')) and (HBlank = '1') and (ena = '1') then
                rowStore := flipData; -- get the first row of pel (flipped if needed)
            elsif (xAddr(0) /= prevXAddr0) and (xMat = '1') and (HBlank = '0')then
                -- on each new X address (since there are 2 bit per pixel)
                rowStore := rowStore(27 downto 0) & "0000";   -- shift out Pels and shift in transparent
            end if;

            prevXAddr0 := xAddr(0);     -- get previous LSb to detect change

        end if;

        pel(1 downto 0) := rowStore(31 downto 30);  -- left PEL
        pel(3 downto 2) := rowStore(29 downto 28);  -- right PEL


        -- set pixel transparency based on match
        if ((xMat = '1') ) and not (pel(1 downto 0) = "00") then
            pixOn(0) <= '1';
        else
            pixOn(0) <= '0';
        end if;
        if ((xMat = '1') ) and not (pel(3 downto 2) = "00") then
            pixOn(1) <= '1';
        else
            pixOn(1) <= '0';
        end if;
        
        rowAddr(15 downto 4) <= num;     -- index into sprite maps
        if flags(1) = '1' then
            -- flip Y address
            rowAddr(3 downto 0) <= rowCount(3 downto 0) xor "1111";
        else
            rowAddr(3 downto 0) <= rowCount(3 downto 0);
        end if;
        
        
        -- look up colour
        if pel(1 downto 0) = "01" then
            dOut(3 downto 0) <=  lut(3 downto 0);
        elsif pel(1 downto 0) = "10" then
            dOut(3 downto 0) <=  lut(7 downto 4);
        elsif pel(1 downto 0) = "11" then
            dOut(3 downto 0) <=  lut(11 downto 8);
        end if;

        if pel(3 downto 2) = "01" then
            dOut(7 downto 4) <=  lut(3 downto 0);
        elsif pel(3 downto 2) = "10" then
            dOut(7 downto 4) <=  lut(7 downto 4);
        elsif pel(3 downto 2) = "11" then
            dOut(7 downto 4) <=  lut(11 downto 8);
        end if;

        yMatch <= yMat;
        xMatch <= xMat;
    end process;
 
end beh;


