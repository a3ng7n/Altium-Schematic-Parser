library IEEE;
use IEEE.std_logic_1164.all;
Use IEEE.std_logic_unsigned.all;

entity sptPriHit is 
port
(
    -- priority encode the sprites and the background data
    -- detect a collision with spt 7
    pix         : in     std_logic_vector(7 downto 0);  -- background
    spt0Data    : in     std_logic_vector(7 downto 0);  -- spt data
    spt0on      : in     std_logic_vector(1 downto 0);  -- which pixel is on
    spt1Data    : in     std_logic_vector(7 downto 0);
    spt1on      : in     std_logic_vector(1 downto 0);
    spt2Data    : in     std_logic_vector(7 downto 0);
    spt2on      : in     std_logic_vector(1 downto 0);
    spt3Data    : in     std_logic_vector(7 downto 0);
    spt3on      : in     std_logic_vector(1 downto 0);
    spt4Data    : in     std_logic_vector(7 downto 0);
    spt4on      : in     std_logic_vector(1 downto 0);
    spt5Data    : in     std_logic_vector(7 downto 0);
    spt5on      : in     std_logic_vector(1 downto 0);
    spt6Data    : in     std_logic_vector(7 downto 0);
    spt6on      : in     std_logic_vector(1 downto 0);
    spt7Data    : in     std_logic_vector(7 downto 0);
    spt7on      : in     std_logic_vector(1 downto 0);
    dOut        : out    std_logic_vector(7 downto 0);  -- priority encoded data
    
    --collision detect I/F
    clk         : in     std_logic;
    rstReg      : in     std_logic;              -- reset hitreg back to 7
    hit         : out    std_logic_vector(2 downto 0);   -- which spt num hit spt7
    hitReg      : out    std_logic_vector(2 downto 0)   -- which spt num hit spt7, latched
) ;
end sptPriHit;


architecture RTL of sptPriHit is
    signal hitSig   : std_logic_vector(2 downto 0);
begin

    -- sprite 0 has highest priority
    dOut(3 downto 0) <= spt0Data(3 downto 0) when spt0on(0) = '1' else
                            spt1Data(3 downto 0) when spt1on(0) = '1' else
                            spt2Data(3 downto 0) when spt2on(0) = '1' else
                            spt3Data(3 downto 0) when spt3on(0) = '1' else
                            spt4Data(3 downto 0) when spt4on(0) = '1' else
                            spt5Data(3 downto 0) when spt5on(0) = '1' else
                            spt6Data(3 downto 0) when spt6on(0) = '1' else
                            spt7Data(3 downto 0) when spt7on(0) = '1' else
                            pix(3 downto 0);
    dOut(7 downto 4) <= spt0Data(7 downto 4) when spt0on(1) = '1' else
                            spt1Data(7 downto 4) when spt1on(1) = '1' else
                            spt2Data(7 downto 4) when spt2on(1) = '1' else
                            spt3Data(7 downto 4) when spt3on(1) = '1' else
                            spt4Data(7 downto 4) when spt4on(1) = '1' else
                            spt5Data(7 downto 4) when spt5on(1) = '1' else
                            spt6Data(7 downto 4) when spt6on(1) = '1' else
                            spt7Data(7 downto 4) when spt7on(1) = '1' else
                            pix(7 downto 4);

    -- detect a collision with spt 7, report the spt number else report number 7
    hitSig(2 downto 0) <=  "000" when (spt7on(1 downto 0) and spt0on(1 downto 0)) /= "00" else
                    "001" when (spt7on(1 downto 0) and spt1on(1 downto 0)) /= "00" else
                    "010" when (spt7on(1 downto 0) and spt2on(1 downto 0)) /= "00" else
                    "011" when (spt7on(1 downto 0) and spt3on(1 downto 0)) /= "00" else
                    "100" when (spt7on(1 downto 0) and spt4on(1 downto 0)) /= "00" else
                    "101" when (spt7on(1 downto 0) and spt5on(1 downto 0)) /= "00" else
                    "110" when (spt7on(1 downto 0) and spt6on(1 downto 0)) /= "00" else
                    "111";
    hit <= hitSig;

    process(clk)
        variable reg    : std_logic_vector(2 downto 0);   -- latched sprite hit
    
    begin
        if rising_edge(clk) then
            -- this does not work on Cyclone
            -- now works on build 893
            if rstReg = '1' then
                reg := "111";   -- no hit
            elsif hitSig /= "111" then
                -- capture a hit
                reg := hitSig;
            end if;

        end if;
        
        hitReg <= reg;      -- export the register
    end process;
end RTL;



