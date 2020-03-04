------------------------------------------------------------
-- SubModule MEM_INT_8_16.VHD
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity MEM_INT_8_16 is port
   (
    -----------------------------------------------------
    --   CPU Side Signals                              --
    -----------------------------------------------------
    IADDR     : in  std_logic_vector (15 downto 0);        -- Address Lines from CPU or Counter
    MODE_16   : in  std_logic;                             -- Mode select: 0=8 bit mode, 1= 16 bit mode
    RD_IN     : in  std_logic;                             -- Read Signal Input from CPU
    WR_IN     : in  std_logic;                             -- Write Signal Input from CPU
    EN_IN     : in  std_logic;                             -- Enable signal input from CPU

    D_INP16B  : in  std_logic_vector(15 downto 0);         -- 16 bit Data bus input from CPU
    D_OUT16B  : out std_logic_vector(15 downto 0);         -- 16 bit Data bus output to CPU
    D_INP8B   : in  std_logic_vector(7 downto 0);          -- 8  bit Data bus input from CPU
    D_OUT8B   : out std_logic_vector(7 downto 0);          -- 8  bit Data bus output to CPU

    -----------------------------------------------------
    --   RAM Side Signals                              --
    -----------------------------------------------------
    DR_IO16B  : inout std_logic_vector(15 downto 0);       -- Data Bus to <--> from RAM
    OADDR     : out std_logic_vector (18 downto 0);        -- Address Lines to RAM
    WR_OUT    : out std_logic;                             -- Write output to RAM
    RD_OUT    : out std_logic;                             -- Read output to RAM
    UB_OUT    : out std_logic;                             -- Upper byte select output to RAM
    LB_OUT    : out std_logic;                             -- Lower byte select output to RAM
    EN_OUT    : out std_logic                              -- Enable output to RAM
   );
end entity;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of MEM_INT_8_16 is

-- Component Declarations


-- Signal Declarations

signal INT_RD16 : std_logic_vector(15 downto 0);           -- internal 16 bit bus out to CPU
signal INT_RD8H : std_logic_vector(7 downto 0);            -- high byte out to CPU
signal INT_RD8L : std_logic_vector(7 downto 0);            -- low byte out to CPU

signal INT_WR8H : std_logic_vector(7 downto 0);            -- high byte from CPU
signal INT_WR8L : std_logic_vector(7 downto 0);            -- low byte from CPU
signal INT_WR8  : std_logic_vector(15 downto 0);           -- internal 8 bit bus from CPU
signal INT_IN8  : std_logic_vector(7 downto 0);            -- internal 8 bit bus to CPU

signal DR_OUT16 : std_logic_vector(15 downto 0);
signal DR_OUT_16_EN : std_logic;                           -- RAM data bus output buffer enable
signal LB_IN    : std_logic;                               -- internal LB signal for 8-bit mode

begin

  LB_IN <= IADDR(0);                                       -- internal LB is A0 in 8-bit mode

  OADDR(18 downto 0)   <= "000" & IADDR(15 downto 0) when MODE_16 = '1' else "0000" & IADDR(15 downto 1); -- A0 is Ub/LB in 8 bit mode

  UB_OUT <=   NOT LB_IN  when MODE_16 = '0' else ('0');
  LB_OUT <=       LB_IN  when MODE_16 = '0' else ('0');

  WR_OUT <= WR_IN;                                         -- pass straight through
  RD_OUT <= RD_IN;                                         -- pass straight through
  EN_OUT <= EN_IN;                                         -- pass straight through

  INT_WR8  <= D_INP8B & "00000000" when (LB_IN = '1') else "00000000" & D_INP8B;                          -- MUX2 8 bit data from CPU

  INT_RD8H <= INT_RD16(15 downto 8);
  INT_RD8L <= INT_RD16(7 downto 0);
  INT_IN8  <= INT_RD8L  when (LB_IN = '0') else INT_RD8H;                                                  -- MUX3  8 bit data to CPU
  D_OUT8B  <= INT_IN8   when  ((RD_IN = '0') and (MODE_16 = '0') and (EN_IN = '0')) else (others => 'Z');  --  8 bit data buffer to CPU
  D_OUT16B <= INT_RD16  when ((RD_IN = '0') and (MODE_16 = '1') and (EN_IN = '0')) else (others => 'Z');   -- 16 bit data buffer to CPU

  DR_OUT16  <= D_INP16B when (MODE_16 = '1') else (INT_WR8);                                               -- Multiplexer MUX1 16 bit data to RAM

  DR_OUT_16_EN <= '0'   when ((EN_IN = '0') and (WR_IN = '0')) else '1';                                   -- Data out to RAM enable

  DR_IO16B  <= DR_OUT16 when DR_OUT_16_EN = '0' else (others => 'Z');                                      -- Output driver
  INT_RD16  <= DR_IO16B;

end structure;
------------------------------------------------------------
