library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
-- -----------------------------------------------------------------------------
entity DB30_MainClock is
   port ( CLKIN_IN    : in    std_logic;
          RST_IN      : in    std_logic;
          CLK0_OUT    : out   std_logic;
          CLK100_OUT  : Out   Std_Logic;
          LOCKED_OUT  : out   std_logic);
end DB30_MainClock;
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
architecture BEHAVIORAL of DB30_MainClock is
-- -----------------------------------------------------------------------------
   signal CLK0_BUF     : std_logic;
   signal CLK0_OUT_BUF : std_logic;
   signal GND_BIT      : std_logic;
   Signal CLK100_BUF   : Std_Logic;

----- component DCM -----
component DCM
  generic (
     CLKDV_DIVIDE          : real       := 2.0;
     CLKFX_DIVIDE          : integer    := 1;
     CLKFX_MULTIPLY        : integer    := 4;
     CLKIN_DIVIDE_BY_2     : boolean    := false;
     CLKIN_PERIOD          : real       := 10.0;
     CLKOUT_PHASE_SHIFT    : string     := "NONE";
     CLK_FEEDBACK          : string     := "1X";
     DESKEW_ADJUST         : string     := "SYSTEM_SYNCHRONOUS";
     DFS_FREQUENCY_MODE    : string     := "LOW";
     DLL_FREQUENCY_MODE    : string     := "LOW";
     DSS_MODE              : string     := "NONE";
     DUTY_CYCLE_CORRECTION : boolean    := true;
     FACTORY_JF            : bit_vector := X"C080";
     PHASE_SHIFT           : integer    := 0;
     SIM_MODE              : string     := "SAFE";
     STARTUP_WAIT          : boolean    := false
  );
  port (
     CLK0     : out std_ulogic := '0';
     CLK180   : out std_ulogic := '0';
     CLK270   : out std_ulogic := '0';
     CLK2X    : out std_ulogic := '0';
     CLK2X180 : out std_ulogic := '0';
     CLK90    : out std_ulogic := '0';
     CLKDV    : out std_ulogic := '0';
     CLKFX    : out std_ulogic := '0';
     CLKFX180 : out std_ulogic := '0';
     LOCKED   : out std_ulogic := '0';
     PSDONE   : out std_ulogic := '0';
     STATUS   : out std_logic_vector(7 downto 0) := "00000000";
     CLKFB    : in  std_ulogic := '0';
     CLKIN    : in  std_ulogic := '0';
     DSSEN    : in  std_ulogic := '0';
     PSCLK    : in  std_ulogic := '0';
     PSEN     : in  std_ulogic := '0';
     PSINCDEC : in  std_ulogic := '0';
     RST      : in  std_ulogic := '0'
  );
end component;

component BUFG
  port (
     O : out std_ulogic;
     I : in std_ulogic
  );
end component;

-- -----------------------------------------------------------------------------
begin
-- -----------------------------------------------------------------------------
   GND_BIT <= '0';
   CLK0_BUFG_INST_MAIN : BUFG
      port map (I => CLK0_BUF,
                O => CLK0_OUT_BUF);

   CLK100_BUFG_INST_MAIN : BUFG
      port map (I => CLK100_BUF,
                O => CLK100_OUT);

   CLK0_OUT <= CLK0_OUT_BUF;

   DCM_INST_MAIN : DCM
   generic map(
            CLK_FEEDBACK          => "1X",
            CLKDV_DIVIDE          => 2.0,
            CLKFX_DIVIDE          => 2,
            CLKFX_MULTIPLY        => 6,
            CLKIN_DIVIDE_BY_2     => FALSE,
            CLKIN_PERIOD          => 25.000,
            CLKOUT_PHASE_SHIFT    => "NONE",
            DESKEW_ADJUST         => "SYSTEM_SYNCHRONOUS",
            DFS_FREQUENCY_MODE    => "LOW",
            DLL_FREQUENCY_MODE    => "LOW",
            DUTY_CYCLE_CORRECTION => TRUE,
            FACTORY_JF            => x"8080",
            PHASE_SHIFT           => 0,
            STARTUP_WAIT          => FALSE)
      port map (
                CLKFB    => CLK0_OUT_BUF,
                CLKIN    => CLKIN_IN,
                DSSEN    => GND_BIT,
                PSCLK    => GND_BIT,
                PSEN     => GND_BIT,
                PSINCDEC => GND_BIT,
                RST      => RST_IN,
                CLKDV    => open,
                CLKFX    => open,
                CLKFX180 => open,
                CLK0     => CLK0_BUF,
                CLK2X    => CLK100_BUF,
                CLK2X180 => open,
                CLK90    => open,
                CLK180   => open,
                CLK270   => open,
                LOCKED   => LOCKED_OUT,
                PSDONE   => open,
                STATUS   => open );

-- -----------------------------------------------------------------------------
end BEHAVIORAL;
-- -----------------------------------------------------------------------------


