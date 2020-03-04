------------------------------------------------------------
-- VHDL KeyPadScanner
-- 2004 10 22 18 0 23
-- Created By "DXP VHDL Generator"
-- "Copyright (c) 2002-2004 Altium Limited"
------------------------------------------------------------

------------------------------------------------------------
-- VHDL KeyPadScanner
------------------------------------------------------------

Library IEEE;
Use     IEEE.std_logic_1164.all;

Entity KeypadScanner Is
  port
  (
    CLK_1MHZ : In    STD_LOGIC;                              -- ObjectKind=Port|PrimaryId=CLK_1MHZ
    COL      : In    STD_LOGIC_VECTOR(3 downto 0);           -- ObjectKind=Port|PrimaryId=COL[3..0]
    KEY      : Out   STD_LOGIC_VECTOR(3 downto 0);           -- ObjectKind=Port|PrimaryId=KEY[3..0]
    ROW      : Out   STD_LOGIC_VECTOR(3 downto 0);           -- ObjectKind=Port|PrimaryId=ROW[3..0]
    RST      : In    STD_LOGIC;                              -- ObjectKind=Port|PrimaryId=RST
    VALIDKEY : Out   STD_LOGIC                               -- ObjectKind=Port|PrimaryId=VALIDKEY
  );
  attribute MacroCell : boolean;

  attribute PART_NAME : string;
  attribute PART_NAME of KeypadScanner : Entity is "xc2s200epq208-5";


End KeypadScanner;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of KeypadScanner is
   Component AND4S                                           -- ObjectKind=Part|PrimaryId=U12|SecondaryId=1
      port
      (
        I0 : in  STD_LOGIC;                                  -- ObjectKind=Pin|PrimaryId=U12-I0
        I1 : in  STD_LOGIC;                                  -- ObjectKind=Pin|PrimaryId=U12-I1
        I2 : in  STD_LOGIC;                                  -- ObjectKind=Pin|PrimaryId=U12-I2
        I3 : in  STD_LOGIC;                                  -- ObjectKind=Pin|PrimaryId=U12-I3
        O  : out STD_LOGIC                                   -- ObjectKind=Pin|PrimaryId=U12-O
      );
   End Component;

   Component CB2CEB                                          -- ObjectKind=Part|PrimaryId=U22|SecondaryId=1
      port
      (
        C   : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U22-C
        CE  : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U22-CE
        CEO : out STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U22-CEO
        CLR : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U22-CLR
        Q   : out STD_LOGIC_VECTOR(1 downto 0);              -- ObjectKind=Pin|PrimaryId=U22-Q[1..0]
        TC  : out STD_LOGIC                                  -- ObjectKind=Pin|PrimaryId=U22-TC
      );
   End Component;

   Component CDIV4                                           -- ObjectKind=Part|PrimaryId=U26|SecondaryId=1
      port
      (
        CLKDV : out STD_LOGIC;                               -- ObjectKind=Pin|PrimaryId=U26-CLKDV
        CLKIN : in  STD_LOGIC                                -- ObjectKind=Pin|PrimaryId=U26-CLKIN
      );
   End Component;

   Component CDIV64                                          -- ObjectKind=Part|PrimaryId=U24|SecondaryId=1
      port
      (
        CLKDV : out STD_LOGIC;                               -- ObjectKind=Pin|PrimaryId=U24-CLKDV
        CLKIN : in  STD_LOGIC                                -- ObjectKind=Pin|PrimaryId=U24-CLKIN
      );
   End Component;

   Component D2_4EB                                          -- ObjectKind=Part|PrimaryId=U23|SecondaryId=1
      port
      (
        A : in  STD_LOGIC_VECTOR(1 downto 0);                -- ObjectKind=Pin|PrimaryId=U23-A[1..0]
        D : out STD_LOGIC_VECTOR(3 downto 0);                -- ObjectKind=Pin|PrimaryId=U23-D[3..0]
        E : in  STD_LOGIC                                    -- ObjectKind=Pin|PrimaryId=U23-E
      );
   End Component;

   Component E4_2B                                           -- ObjectKind=Part|PrimaryId=U20|SecondaryId=1
      port
      (
        A : out STD_LOGIC_VECTOR(1 downto 0);                -- ObjectKind=Pin|PrimaryId=U20-A[1..0]
        D : in  STD_LOGIC_VECTOR(3 downto 0)                 -- ObjectKind=Pin|PrimaryId=U20-D[3..0]
      );
   End Component;

   Component FD                                              -- ObjectKind=Part|PrimaryId=U14|SecondaryId=1
      port
      (
        C : in  STD_LOGIC;                                   -- ObjectKind=Pin|PrimaryId=U14-C
        D : in  STD_LOGIC;                                   -- ObjectKind=Pin|PrimaryId=U14-D
        Q : out STD_LOGIC                                    -- ObjectKind=Pin|PrimaryId=U14-Q
      );
   End Component;

   Component FD4CB                                           -- ObjectKind=Part|PrimaryId=U11|SecondaryId=1
      port
      (
        C   : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U11-C
        CLR : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U11-CLR
        D   : in  STD_LOGIC_VECTOR(3 downto 0);              -- ObjectKind=Pin|PrimaryId=U11-D[3..0]
        Q   : out STD_LOGIC_VECTOR(3 downto 0)               -- ObjectKind=Pin|PrimaryId=U11-Q[3..0]
      );
   End Component;

   Component FDC                                             -- ObjectKind=Part|PrimaryId=U10|SecondaryId=1
      port
      (
        C   : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U10-C
        CLR : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U10-CLR
        D   : in  STD_LOGIC;                                 -- ObjectKind=Pin|PrimaryId=U10-D
        Q   : out STD_LOGIC                                  -- ObjectKind=Pin|PrimaryId=U10-Q
      );
   End Component;

   Component INV                                             -- ObjectKind=Part|PrimaryId=U21|SecondaryId=1
      port
      (
        I : in  STD_LOGIC;                                   -- ObjectKind=Pin|PrimaryId=U21-I
        O : out STD_LOGIC                                    -- ObjectKind=Pin|PrimaryId=U21-O
      );
   End Component;

   Component INV4B                                           -- ObjectKind=Part|PrimaryId=U18|SecondaryId=1
      port
      (
        I : in  STD_LOGIC_VECTOR(3 downto 0);                -- ObjectKind=Pin|PrimaryId=U18-I[3..0]
        O : out STD_LOGIC_VECTOR(3 downto 0)                 -- ObjectKind=Pin|PrimaryId=U18-O[3..0]
      );
   End Component;

   Component NAND4B                                          -- ObjectKind=Part|PrimaryId=U13|SecondaryId=1
      port
      (
        I : in  STD_LOGIC_VECTOR(3 downto 0);                -- ObjectKind=Pin|PrimaryId=U13-I[3..0]
        O : out STD_LOGIC                                    -- ObjectKind=Pin|PrimaryId=U13-O
      );
   End Component;


    Signal NamedSignal_PRESSED : STD_LOGIC_VECTOR(3 downto 0); -- ObjectKind=Net|PrimaryId=PRESSED[1..0]
    Signal PinSignal_U10_Q     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU10_Q
    Signal PinSignal_U11_Q     : STD_LOGIC_VECTOR(3 downto 0); -- ObjectKind=Net|PrimaryId=KEY[3..0]
    Signal PinSignal_U12_O     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=VALIDKEY
    Signal PinSignal_U13_O     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU12_I0
    Signal PinSignal_U14_Q     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU12_I1
    Signal PinSignal_U15_Q     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU12_I2
    Signal PinSignal_U16_Q     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU12_I3
    Signal PinSignal_U17_Q     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=HZ_31
    Signal PinSignal_U18_O     : STD_LOGIC_VECTOR(3 downto 0); -- ObjectKind=Net|PrimaryId=NetU18_O[3..0]
    Signal PinSignal_U19_O     : STD_LOGIC_VECTOR(3 downto 0); -- ObjectKind=Net|PrimaryId=ROW[3..0]
    Signal PinSignal_U20_A     : STD_LOGIC_VECTOR(1 downto 0); -- ObjectKind=Net|PrimaryId=PRESSED[1..0]
    Signal PinSignal_U21_O     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU17_D
    Signal PinSignal_U22_Q     : STD_LOGIC_VECTOR(1 downto 0); -- ObjectKind=Net|PrimaryId=PRESSED[3..0]
    Signal PinSignal_U23_D     : STD_LOGIC_VECTOR(3 downto 0); -- ObjectKind=Net|PrimaryId=NetU19_I[3..0]
    Signal PinSignal_U24_CLKDV : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU24_CLKDV
    Signal PinSignal_U25_CLKDV : STD_LOGIC; -- ObjectKind=Net|PrimaryId=HZ_244
    Signal PinSignal_U26_CLKDV : STD_LOGIC; -- ObjectKind=Net|PrimaryId=NetU17_C
    Signal PowerSignal_GND     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=GND
    Signal PowerSignal_VCC     : STD_LOGIC; -- ObjectKind=Net|PrimaryId=VCC


begin
    U26 : CDIV4                                              -- ObjectKind=Part|PrimaryId=U26|SecondaryId=1
      Port Map
      (
        CLKDV => PinSignal_U26_CLKDV,                        -- ObjectKind=Pin|PrimaryId=U26-CLKDV
        CLKIN => PinSignal_U25_CLKDV                         -- ObjectKind=Pin|PrimaryId=U26-CLKIN
      );

    U25 : CDIV64                                             -- ObjectKind=Part|PrimaryId=U25|SecondaryId=1
      Port Map
      (
        CLKDV => PinSignal_U25_CLKDV,                        -- ObjectKind=Pin|PrimaryId=U25-CLKDV
        CLKIN => PinSignal_U24_CLKDV                         -- ObjectKind=Pin|PrimaryId=U25-CLKIN
      );

    U24 : CDIV64                                             -- ObjectKind=Part|PrimaryId=U24|SecondaryId=1
      Port Map
      (
        CLKDV => PinSignal_U24_CLKDV,                        -- ObjectKind=Pin|PrimaryId=U24-CLKDV
        CLKIN => CLK_1MHZ                                    -- ObjectKind=Pin|PrimaryId=U24-CLKIN
      );

    U23 : D2_4EB                                             -- ObjectKind=Part|PrimaryId=U23|SecondaryId=1
      Port Map
      (
        A => PinSignal_U22_Q,                                -- ObjectKind=Pin|PrimaryId=U23-A[1..0]
        D => PinSignal_U23_D,                                -- ObjectKind=Pin|PrimaryId=U23-D[3..0]
        E => PowerSignal_VCC                                 -- ObjectKind=Pin|PrimaryId=U23-E
      );

    U22 : CB2CEB                                             -- ObjectKind=Part|PrimaryId=U22|SecondaryId=1
      Port Map
      (
        C   => PinSignal_U17_Q,                              -- ObjectKind=Pin|PrimaryId=U22-C
        CE  => PowerSignal_VCC,                              -- ObjectKind=Pin|PrimaryId=U22-CE
        CLR => PowerSignal_GND,                              -- ObjectKind=Pin|PrimaryId=U22-CLR
        Q   => PinSignal_U22_Q                               -- ObjectKind=Pin|PrimaryId=U22-Q[1..0]
      );

    U21 : INV                                                -- ObjectKind=Part|PrimaryId=U21|SecondaryId=1
      Port Map
      (
        I => PinSignal_U17_Q,                                -- ObjectKind=Pin|PrimaryId=U21-I
        O => PinSignal_U21_O                                 -- ObjectKind=Pin|PrimaryId=U21-O
      );

    U20 : E4_2B                                              -- ObjectKind=Part|PrimaryId=U20|SecondaryId=1
      Port Map
      (
        A => PinSignal_U20_A,                                -- ObjectKind=Pin|PrimaryId=U20-A[1..0]
        D => PinSignal_U18_O                                 -- ObjectKind=Pin|PrimaryId=U20-D[3..0]
      );

    U19 : INV4B                                              -- ObjectKind=Part|PrimaryId=U19|SecondaryId=1
      Port Map
      (
        I => PinSignal_U23_D,                                -- ObjectKind=Pin|PrimaryId=U19-I[3..0]
        O => PinSignal_U19_O                                 -- ObjectKind=Pin|PrimaryId=U19-O[3..0]
      );

    U18 : INV4B                                              -- ObjectKind=Part|PrimaryId=U18|SecondaryId=1
      Port Map
      (
        I => COL,                                            -- ObjectKind=Pin|PrimaryId=U18-I[3..0]
        O => PinSignal_U18_O                                 -- ObjectKind=Pin|PrimaryId=U18-O[3..0]
      );

    U17 : FD                                                 -- ObjectKind=Part|PrimaryId=U17|SecondaryId=1
      Port Map
      (
        C => PinSignal_U26_CLKDV,                            -- ObjectKind=Pin|PrimaryId=U17-C
        D => PinSignal_U21_O,                                -- ObjectKind=Pin|PrimaryId=U17-D
        Q => PinSignal_U17_Q                                 -- ObjectKind=Pin|PrimaryId=U17-Q
      );

    U16 : FD                                                 -- ObjectKind=Part|PrimaryId=U16|SecondaryId=1
      Port Map
      (
        C => PinSignal_U25_CLKDV,                            -- ObjectKind=Pin|PrimaryId=U16-C
        D => PinSignal_U15_Q,                                -- ObjectKind=Pin|PrimaryId=U16-D
        Q => PinSignal_U16_Q                                 -- ObjectKind=Pin|PrimaryId=U16-Q
      );

    U15 : FD                                                 -- ObjectKind=Part|PrimaryId=U15|SecondaryId=1
      Port Map
      (
        C => PinSignal_U25_CLKDV,                            -- ObjectKind=Pin|PrimaryId=U15-C
        D => PinSignal_U14_Q,                                -- ObjectKind=Pin|PrimaryId=U15-D
        Q => PinSignal_U15_Q                                 -- ObjectKind=Pin|PrimaryId=U15-Q
      );

    U14 : FD                                                 -- ObjectKind=Part|PrimaryId=U14|SecondaryId=1
      Port Map
      (
        C => PinSignal_U25_CLKDV,                            -- ObjectKind=Pin|PrimaryId=U14-C
        D => PinSignal_U13_O,                                -- ObjectKind=Pin|PrimaryId=U14-D
        Q => PinSignal_U14_Q                                 -- ObjectKind=Pin|PrimaryId=U14-Q
      );

    U13 : NAND4B                                             -- ObjectKind=Part|PrimaryId=U13|SecondaryId=1
      Port Map
      (
        I => COL,                                            -- ObjectKind=Pin|PrimaryId=U13-I[3..0]
        O => PinSignal_U13_O                                 -- ObjectKind=Pin|PrimaryId=U13-O
      );

    U12 : AND4S                                              -- ObjectKind=Part|PrimaryId=U12|SecondaryId=1
      Port Map
      (
        I0 => PinSignal_U13_O,                               -- ObjectKind=Pin|PrimaryId=U12-I0
        I1 => PinSignal_U14_Q,                               -- ObjectKind=Pin|PrimaryId=U12-I1
        I2 => PinSignal_U15_Q,                               -- ObjectKind=Pin|PrimaryId=U12-I2
        I3 => PinSignal_U16_Q,                               -- ObjectKind=Pin|PrimaryId=U12-I3
        O  => PinSignal_U12_O                                -- ObjectKind=Pin|PrimaryId=U12-O
      );

    U11 : FD4CB                                              -- ObjectKind=Part|PrimaryId=U11|SecondaryId=1
      Port Map
      (
        C   => PinSignal_U12_O,                              -- ObjectKind=Pin|PrimaryId=U11-C
        CLR => RST,                                          -- ObjectKind=Pin|PrimaryId=U11-CLR
        D   => NamedSignal_PRESSED,                          -- ObjectKind=Pin|PrimaryId=U11-D[3..0]
        Q   => PinSignal_U11_Q                               -- ObjectKind=Pin|PrimaryId=U11-Q[3..0]
      );

    U10 : FDC                                                -- ObjectKind=Part|PrimaryId=U10|SecondaryId=1
      Port Map
      (
        C   => PinSignal_U12_O,                              -- ObjectKind=Pin|PrimaryId=U10-C
        CLR => RST,                                          -- ObjectKind=Pin|PrimaryId=U10-CLR
        D   => PowerSignal_VCC,                              -- ObjectKind=Pin|PrimaryId=U10-D
        Q   => PinSignal_U10_Q                               -- ObjectKind=Pin|PrimaryId=U10-Q
      );

    -- Signal Assignments
    ---------------------
    KEY                             <= PinSignal_U11_Q; -- ObjectKind=Net|PrimaryId=KEY[3..0]
    NamedSignal_PRESSED(1 downto 0) <= PinSignal_U20_A; -- ObjectKind=Net|PrimaryId=PRESSED[1..0]
    NamedSignal_PRESSED(3 downto 2) <= PinSignal_U22_Q; -- ObjectKind=Net|PrimaryId=PRESSED[3..0]
    PowerSignal_GND                 <= '0'; -- ObjectKind=Net|PrimaryId=GND
    PowerSignal_VCC                 <= '1'; -- ObjectKind=Net|PrimaryId=VCC
    ROW                             <= PinSignal_U19_O; -- ObjectKind=Net|PrimaryId=ROW[3..0]
    VALIDKEY                        <= PinSignal_U10_Q; -- ObjectKind=Net|PrimaryId=NetU10_Q

end structure;
------------------------------------------------------------

