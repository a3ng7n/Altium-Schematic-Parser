-------------------------------------------------------
--- Submodule vgaif.vhdl (VGA)
-------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
Use IEEE.std_logic_unsigned.all;

entity VGAIF is
port
(
    CLK         : in    std_logic;
    RST         : in    std_logic;
    HS          : in    std_logic;

    VGA_RD      : in    std_logic;
    VGA_DATA    : out   std_logic_vector(7 downto 0);
    VGA_PXLADDR : in    std_logic_vector(18 downto 0);

    RAM_ADDR    : out   std_logic_vector(17 downto 0);
    RAM_DI      : in    std_logic_vector(7 downto 0);
    RAM_DO      : out   std_logic_vector(7 downto 0);
    RAM_RD      : out   std_logic;
    RAM_WR      : out   std_logic;
    RAM_EN      : out   std_logic;

    SFR_ADDR    : in    std_logic_vector(6 downto 0);
    SFR_DATAI   : in    std_logic_vector(7 downto 0);
    SFR_DATAO   : out   std_logic_vector(7 downto 0);
    SFR_DATAL   : in    std_logic_vector(7 downto 0);
    SFR_WR      : in    std_logic;
    SFR_RD      : in    std_logic;

    C0_DATAO    : out   std_logic_vector(7 downto 0);
    C0_DATAI    : in    std_logic_vector(7 downto 0);
    C0_ADDR     : out   std_logic_vector(8 downto 0);
    C0_WR       : out   std_logic;

    C1_DATAO    : out   std_logic_vector(7 downto 0);
    C1_DATAI    : in    std_logic_vector(7 downto 0);
    C1_ADDR     : out   std_logic_vector(8 downto 0);
    C1_WR       : out   std_logic

) ;
end VGAIF;

architecture RTL of VGAIF is

    signal RST_PXL          : std_logic;
    signal PXL_ADDR_L       : std_logic_vector(9 downto 0);         -- PIXEL ADDRESS LOW BITS
    signal PXL_ADDR_H       : std_logic_vector(7 downto 0);         -- PIXEL ADDRESS HIGH BITS

    signal RAM_READ_ADDR    : std_logic_vector(16 downto 0);
    signal RAM_WRITE_ADDR   : std_logic_vector(16 downto 0);
    signal RAM_DOUT         : std_logic_vector(7 downto 0);
    signal RAM_READ         : std_logic;
    signal RAM_WRITE        : std_logic;

    signal RAM_TO_CACHE     : std_logic;
    signal CACHE_REQUEST    : std_logic;

    signal VGA_CMD          : std_logic_vector(7 downto 0);         -- VGA command
    signal RST_CMD          : std_logic;                            -- Reset VGA command

    signal READ_PXL         : std_logic;
    signal PXL_VALID        : std_logic;

    signal XPOS             : std_logic_vector(15 downto 0);        -- Pixel X-Position
    signal YPOS             : std_logic_vector(15 downto 0);        -- Pixel Y-Position
    signal COLOR            : std_logic_vector(7 downto 0);         -- Pixel color

    signal PXL_RAM_ADDR     : std_logic_vector( 16 downto 0 );      -- RAM address for PIXEL[XPOS,YPOS]

    alias CMD               : std_logic_vector(3 downto 0) is VGA_CMD(3 downto 0);
    alias INC_X             : std_logic_vector(1 downto 0) is VGA_CMD(7 downto 6);
    alias INC_Y             : std_logic_vector(1 downto 0) is VGA_CMD(5 downto 4);

begin

--  *****************************  SFR  ***************************************

-- WRITE to SFR
    WRITE_SFR: process( CLK, RST )
    begin
        if RST = '1' then
            VGA_CMD <= ( others => '0');
            XPOS <= ( others => '0');
            YPOS <= ( others => '0');
            COLOR <= ( others => '0');
        elsif rising_edge( CLK ) then
            if RST_CMD = '1' then
                CMD <= ( others => '0');
            elsif CMD = "0000" then
                if INC_X /= 0 then
                    XPOS <= XPOS + INC_X;
                    INC_X <= "00";
                elsif INC_Y /= 0 then
                    YPOS <= YPOS + INC_Y;
                    INC_Y <= "00";
                elsif SFR_WR = '1' and VGA_CMD = "00000000" then
                    case SFR_ADDR is
                    when "1011001" =>   -- VCMD at SFR address D9h
                        VGA_CMD <= SFR_DATAI;
                    when "1011010" =>   -- X-Pos high at SFR address DAh
                        XPOS(15 downto 8) <= SFR_DATAI;
                    when "1011011" =>   -- X-Pos low at SFR address DBh
                        XPOS( 7 downto 0) <= SFR_DATAI;
                    when "1011100" =>   -- Y-Pos high at SFR address DCh
                        YPOS(15 downto 8) <= SFR_DATAI;
                    when "1011101" =>   -- Y-Pos low at SFR address DDh
                        YPOS( 7 downto 0) <= SFR_DATAI;
                    when "1011110" =>   -- Pixel color at SFR address DEh
                        COLOR <= SFR_DATAI;
                    when others =>
                        NULL;
                    end case;
                end if;
            end if;
        end if;
    end process;


-- READ from SFR
    READ_SFR: process( SFR_RD, SFR_ADDR, VGA_CMD, XPOS, YPOS, COLOR, SFR_DATAL )   -- DO NOT USE CLK, read SFR as unclocked memory.
    begin
        if ( SFR_RD = '1' ) then
            case SFR_ADDR is
            when "1011001" =>   -- VCMD at SFR address D9h
                SFR_DATAO <= VGA_CMD;
            when "1011010" =>   -- X-Pos high at SFR address DAh
                SFR_DATAO <= XPOS(15 downto 8);
            when "1011011" =>   -- X-Pos low at SFR address DBh
                SFR_DATAO <= XPOS( 7 downto 0);
            when "1011100" =>   -- Y-Pos high at SFR address DCh
                SFR_DATAO <= YPOS(15 downto 8);
            when "1011101" =>   -- Y-Pos low at SFR address DDh
                SFR_DATAO <= YPOS( 7 downto 0);
            when "1011110" =>   -- Pixel color at SFR address DEh
                SFR_DATAO <= COLOR;
            when others =>
                SFR_DATAO <= SFR_DATAL;
            end case;
        else
            SFR_DATAO <= "00000000";
        end if;
    end process;


--  *****************************  READ PICTURE IN CACHE  ***************************************

-- RESET PIXEL COUNTER
    PIXEL_CNT_CTRL: process( CLK )
    begin
        if rising_edge(CLK) then
            if ( VGA_PXLADDR( 8 downto 0 ) < 4 ) then
                CACHE_REQUEST <= '1';
                RST_PXL <= '1';
            elsif ( VGA_PXLADDR( 8 downto 0 ) > 500 ) then
                CACHE_REQUEST <= '1';
                RST_PXL <= '0';
            else
                CACHE_REQUEST <= '0';
                RST_PXL <= '0';
            end if;
        end if;
    end process;


-- PIXEL COUNTER LOW generates low part of pixel address
    PIXEL_CNT_L: process( CLK, RST_PXL )
    begin
        if RST_PXL = '1' then
            PXL_ADDR_L <= (others => '0');
            RAM_TO_CACHE <= '1';
        elsif rising_edge( CLK ) then
            if HS = '1' then
                if PXL_ADDR_L < 512 then
                    PXL_ADDR_L <= PXL_ADDR_L + 1;
                    RAM_TO_CACHE <= '1';
                else
                    RAM_TO_CACHE <= '0';
                end if;
            end if;
        end if;
    end process;


-- PIXEL COUNTER HIGH generates high part of pixel address
    PIXEL_CNT_H: process( VGA_PXLADDR, VGA_RD )
    begin
        if VGA_RD = '1' then
            if VGA_PXLADDR(16 downto 9) < 249 then
                PXL_ADDR_H <= VGA_PXLADDR( 16 downto 9 ) + 1;
            else
                PXL_ADDR_H <= ( others => '0' );
            end if;
        end if;
    end process;


-- Address for reading from RAM to cache
    RAM_READ_ADDR(16 downto 9) <= PXL_ADDR_H(7 downto 0);
    RAM_READ_ADDR( 8 downto 0) <= PXL_ADDR_L(8 downto 0);


-- CACHE address-, data-in- and control-lines

    C0_DATAO <= RAM_DI;
    C0_ADDR <= VGA_PXLADDR(8 downto 0) when PXL_ADDR_H(0) = '0' else PXL_ADDR_L(8 downto 0);

    C1_DATAO <= RAM_DI;
    C1_ADDR <= VGA_PXLADDR(8 downto 0) when PXL_ADDR_H(0) = '1' else PXL_ADDR_L(8 downto 0);

    CACHE_WRITE_CONTROL: process( CLK )
    begin
        if rising_edge( CLK ) then
            if PXL_ADDR_H(0) = '0' then         -- Reading even line: Write odd cache
                C0_WR <= '0';
                if HS = '1' and RAM_TO_CACHE = '1' and PXL_ADDR_L(9) = '0' then
                    C1_WR <= '1';
                else
                    C1_WR <= '0';
                end if;
            else                        -- Else, write even cache
                C1_WR <= '0';
                if HS = '1' and RAM_TO_CACHE = '1' and PXL_ADDR_L(9) = '0' then
                    C0_WR <= '1';
                else
                    C0_WR <= '0';
                end if;
            end if;
        end if;
    end process;


-- Read from RAM
    RD_RAM: process ( CLK )
    begin
        if rising_edge ( CLK ) then
            if RAM_TO_CACHE = '1' then      -- RAM to CACHE
                RAM_READ <= '1';
                PXL_VALID <= '0';
            elsif HS = '1' and RAM_TO_CACHE = '0' and READ_PXL = '1' then       -- Read byte for patching one pixel
                RAM_READ <= '1';
                PXL_VALID <= '1';
            else
                RAM_READ <= '0';
                PXL_VALID <= '0';
            end if;
        end if;
    end process;


--  *****************************  SHOW PICTURE FROM CACHE  ***************************************

-- CACHE Data in
    process( VGA_RD, VGA_PXLADDR, C0_DATAI, C1_DATAI )
    begin
        if VGA_RD = '1' then
            if VGA_PXLADDR(9) = '1' then    -- Read even cache
                VGA_DATA <= C0_DATAI;
            else                            -- Else, read odd cache
                VGA_DATA <= C1_DATAI;
            end if;
        else
            VGA_DATA <= ( others => '0');
        end if;
    end process;


--  *****************************  WRITE PICTURE  ***************************************

-- Calculate Byte address of pixel[XPOS,YPOS]
    CALC_PXL_ADDR: process( CLK )
    begin
        if rising_edge( CLK ) then
            -- PXL_RAM_ADDR = YPOS * 320 + XPOS/2
            -- PXL_RAM_ADDR =       YPOS * 256              +            YPOS * 64               +            XPOS/2
            PXL_RAM_ADDR <= ( YPOS(8 downto 0)&"00000000" ) + ( "00"&YPOS(8 downto 0)&"000000" ) + ( "00000000"&XPOS(9 downto 1));
        end if;
    end process;


-- Execute command
    EXECUTE_CMD: process ( CLK, RST )
        type STATES is ( STATE_0, STATE_1, STATE_2, STATE_3, STATE_4, STATE_5, STATE_6 );
        variable CLR_STATE  : STATES;
        variable PXL1_STATE  : STATES;
        variable PXL2_STATE  : STATES;
        variable OLD_PXL    : std_logic_vector(7 downto 0);
    begin
        if RST = '1' then
            RST_CMD <= '0';
            RAM_WRITE_ADDR <= ( others => '0');
            CLR_STATE := STATE_0;
            PXL1_STATE := STATE_0;
            PXL2_STATE := STATE_0;
            OLD_PXL := ( others => '0');
            RAM_DOUT <= ( others => '0');
            RAM_WRITE <= '0';
            READ_PXL <= '0';
        elsif rising_edge( CLK ) then
            if CMD = "0000" then
                RST_CMD <= '0';
                RAM_WRITE_ADDR <= ( others => '0');
                CLR_STATE := STATE_0;
                PXL1_STATE := STATE_0;
                PXL2_STATE := STATE_0;

            --
            -- *******  Clear screen  *********
            --
            elsif CMD = "0001" then
                case CLR_STATE is
                when STATE_0 =>                -- Init signals
                    RAM_WRITE_ADDR <= ( others => '0');
                    RAM_DOUT <= COLOR;
                    CLR_STATE := STATE_1;

                when STATE_1 =>             -- Write pixel to mem
                    if HS = '1' and RAM_TO_CACHE = '0' and CACHE_REQUEST = '0' then
                         RAM_WRITE <= '1';
                         CLR_STATE := STATE_2;
                    end if;

                when STATE_2 =>              -- Set next pixel
                        RAM_WRITE <= '0';
                        CLR_STATE := STATE_3;

                when others =>
                    if RAM_WRITE_ADDR /= 128000 then
                        RAM_WRITE_ADDR <= RAM_WRITE_ADDR + 1;
                        RAM_DOUT <= COLOR;
                        CLR_STATE := STATE_1;
                    else
                         RST_CMD <= '1';
                    end if;
                end case;

            --
            -- *******  Set One Pixel  *********
            --
            elsif CMD = "0010" then
                case PXL1_STATE is
                when STATE_0 =>
                    PXL1_STATE := STATE_1;

                when STATE_1 =>
                    RAM_WRITE_ADDR <= PXL_RAM_ADDR;
                    PXL1_STATE := STATE_2;

                when STATE_2 =>
                    -- read pixels
                    READ_PXL <= '1';
                    PXL1_STATE := STATE_3;

                when STATE_3 =>
                    if PXL_VALID = '1' then
                        OLD_PXL := RAM_DI;
                        READ_PXL <= '0';
                        PXL1_STATE := STATE_4;
                    end if;

                when STATE_4 =>
                    -- get new pixel value
                    if XPOS(0) = '0' then
                        -- put in odd pixel nibble
                        RAM_DOUT(7 downto 4) <= OLD_PXL(7 downto 4);
                        RAM_DOUT(3 downto 0) <= COLOR(3 downto 0);
                    else
                        -- put in even pixel nibble
                        RAM_DOUT(7 downto 4) <= COLOR(3 downto 0);
                        RAM_DOUT(3 downto 0) <= OLD_PXL(3 downto 0);
                    end if;
                    PXL1_STATE := STATE_5;

                when STATE_5 =>
                    if HS = '1' and RAM_TO_CACHE = '0' and CACHE_REQUEST = '0' then
                        -- write new pixel value to memory
                        RAM_WRITE <= '1';
                        PXL1_STATE := STATE_6;   -- Goto next state
                    end if;

                when others =>
                    if HS = '1' then
                        -- stop writing new pixel value to memory
                        RAM_WRITE <= '0';
                        RST_CMD <= '1';
                    end if;
                end case;

            --
            -- *******  Set Two Pixel  *********
            --
            elsif CMD = "0011" then
                case PXL2_STATE is
                when STATE_0 =>
                    RAM_WRITE_ADDR <= PXL_RAM_ADDR;
                    RAM_DOUT <= COLOR;
                    PXL2_STATE := STATE_1;

                when STATE_1 =>
                    if HS = '1' and RAM_TO_CACHE = '0' and CACHE_REQUEST = '0' then
                        RAM_WRITE <= '1';
                        PXL2_STATE := STATE_2;
                    end if;

                when STATE_2 =>
                    if HS = '1' then
                        RAM_WRITE <= '0';
                        PXL2_STATE := STATE_3;
                    end if;

                when others =>
                    RST_CMD <= '1';
                end case;

            else
                RST_CMD <= '1';
            end if;
        end if;
    end process;


--  *****************************  RAM INTERFACE  ***************************************

-- TO RAM
    RAM_ADDR(16 downto 0) <= RAM_READ_ADDR when RAM_TO_CACHE = '1' else RAM_WRITE_ADDR;
    RAM_ADDR(17) <= '1';

    RAM_DO <= RAM_DOUT;

    RAM_WR <= RAM_WRITE;
    RAM_RD <= RAM_READ;
    RAM_EN <= RAM_WRITE or RAM_READ;


end RTL;














