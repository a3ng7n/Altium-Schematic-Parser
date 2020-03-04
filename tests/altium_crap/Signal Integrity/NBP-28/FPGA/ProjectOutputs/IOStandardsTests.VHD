------------------------------------------------------------
-- VHDL IOStandardsTests
-- 2006 10 17 17 13 31
-- Created By "DXP VHDL Generator"
-- "Copyright (c) 2002-2004 Altium Limited"
------------------------------------------------------------

------------------------------------------------------------
-- VHDL IOStandardsTests
------------------------------------------------------------

Library IEEE;
Use     IEEE.std_logic_1164.all;

Entity IOStandardsTests Is
  port
  (
    A             : InOut STD_LOGIC_VECTOR(25 DOWNTO 0);     -- ObjectKind=Port|PrimaryId=A[25..0]
    AUDIO_SPI_CSN : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=AUDIO_SPI_CSN
    BLUE0         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=BLUE0
    BLUE1         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=BLUE1
    BOC_CTS       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=BOC_CTS
    BOC_RTS       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=BOC_RTS
    BOC_RX        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=BOC_RX
    BOC_TX        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=BOC_TX
    BUZZER        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=BUZZER
    CAN_RXD       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=CAN_RXD
    CAN_TXD       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=CAN_TXD
    CLK_RTC       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=CLK_RTC
    CLKIN         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=CLKIN
    CLKOUT        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=CLKOUT
    D             : InOut STD_LOGIC_VECTOR(31 DOWNTO 0);     -- ObjectKind=Port|PrimaryId=D[31..0]
    DEOT0         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=DEOT0
    DREQ0         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=DREQ0
    FPGA_AUX1     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=FPGA_AUX1
    FPGA_AUX2     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=FPGA_AUX2
    FPGA_AUX3     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=FPGA_AUX3
    FPGA_CLK      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=FPGA_CLK
    FPGA_CLK_1    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=FPGA_CLK_1
    FPGA_CLK_2    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=FPGA_CLK_2
    FPGA_DIN      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=FPGA_DIN
    GREEN0        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=GREEN0
    GREEN1        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=GREEN1
    HDRIVE        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=HDRIVE
    INT           : InOut STD_LOGIC_VECTOR(4 DOWNTO 0);      -- ObjectKind=Port|PrimaryId=INT[4..0]
    IO0           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO0
    IO1           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO1
    IO2           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO2
    IO3           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO3
    IO4           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO4
    IO5           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO5
    IO6           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO6
    IO7           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO7
    IO8           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO8
    IO9           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO9
    IO10          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO10
    IO11          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO11
    IO12          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO12
    IO13          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO13
    IO14          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO14
    IO15          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO15
    IO16          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO16
    IO17          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO17
    IO18          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO18
    IO19          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO19
    IO20          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO20
    IO21          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO21
    IO22          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO22
    IO23          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO23
    IO24          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO24
    IO25          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO25
    IO26          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO26
    IO27          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO27
    IO28          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO28
    IO29          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO29
    IO30          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO30
    IO31          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO31
    IO32          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO32
    IO33          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO33
    IO34          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO34
    IO35          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=IO35
    JIOA0         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOA0
    JIOA1         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOA1
    JIOA2         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOA2
    JIOA3         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOA3
    JIOB0         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOB0
    JIOB1         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOB1
    JIOB2         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOB2
    JIOB3         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=JIOB3
    KBCLOCK       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=KBCLOCK
    KBDATA        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=KBDATA
    LCD_BCKL      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LCD_BCKL
    LCD_E         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LCD_E
    LED0          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED0
    LED1          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED1
    LED2          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED2
    LED3          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED3
    LED4          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED4
    LED5          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED5
    LED6          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED6
    LED7          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=LED7
    MOUSECLOCK    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=MOUSECLOCK
    MOUSEDATA     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=MOUSEDATA
    NBLE          : InOut STD_LOGIC_VECTOR(3 DOWNTO 0);      -- ObjectKind=Port|PrimaryId=NBLE[3..0]
    NCS           : InOut STD_LOGIC_VECTOR(5 DOWNTO 0);      -- ObjectKind=Port|PrimaryId=NCS[5..0]
    NDACK0        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NDACK0
    NEXUS_TCK     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NEXUS_TCK
    NEXUS_TDI     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NEXUS_TDI
    NEXUS_TDO_R   : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NEXUS_TDO_R
    NEXUS_TMS     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NEXUS_TMS
    NOE           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NOE
    NRESETIN      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NRESETIN
    NRESETOUT     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NRESETOUT
    NWAIT         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NWAIT
    NWE           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=NWE
    RAM0_DATA0    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA0
    RAM0_DATA1    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA1
    RAM0_DATA2    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA2
    RAM0_DATA3    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA3
    RAM0_DATA4    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA4
    RAM0_DATA5    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA5
    RAM0_DATA6    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA6
    RAM0_DATA7    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_DATA7
    RAM0_OE       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_OE
    RAM0_WE       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM0_WE
    RAM1_DATA0    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA0
    RAM1_DATA1    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA1
    RAM1_DATA2    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA2
    RAM1_DATA3    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA3
    RAM1_DATA4    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA4
    RAM1_DATA5    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA5
    RAM1_DATA6    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA6
    RAM1_DATA7    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_DATA7
    RAM1_OE       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_OE
    RAM1_WE       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM1_WE
    RAM_ADDR0     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR0
    RAM_ADDR1     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR1
    RAM_ADDR2     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR2
    RAM_ADDR3     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR3
    RAM_ADDR4     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR4
    RAM_ADDR5     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR5
    RAM_ADDR6     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR6
    RAM_ADDR7     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR7
    RAM_ADDR8     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR8
    RAM_ADDR9     : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR9
    RAM_ADDR10    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR10
    RAM_ADDR11    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR11
    RAM_ADDR12    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR12
    RAM_ADDR13    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR13
    RAM_ADDR14    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR14
    RAM_ADDR15    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR15
    RAM_ADDR16    : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_ADDR16
    RAM_CS        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RAM_CS
    RED0          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RED0
    RED1          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=RED1
    REF_CLK       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=REF_CLK
    SCL           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SCL
    SDA           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SDA
    SP0           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP0
    SP1           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP1
    SP2           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP2
    SP3           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP3
    SP4           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP4
    SP5           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP5
    SP6           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP6
    SP7           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP7
    SP8           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP8
    SP9           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP9
    SP10          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP10
    SP11          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP11
    SP12          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP12
    SP13          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP13
    SP14          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP14
    SP15          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SP15
    SPI_CLK       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SPI_CLK
    SPI_DIN       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SPI_DIN
    SPI_DOUT      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SPI_DOUT
    SPI_MODE      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SPI_MODE
    SPI_SEL       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SPI_SEL
    SRAM0_A       : InOut STD_LOGIC_VECTOR(18 DOWNTO 0);     -- ObjectKind=Port|PrimaryId=SRAM0_A[18..0]
    SRAM0_D       : InOut STD_LOGIC_VECTOR(15 DOWNTO 0);     -- ObjectKind=Port|PrimaryId=SRAM0_D[15..0]
    SRAM0_E       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM0_E
    SRAM0_LB      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM0_LB
    SRAM0_OE      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM0_OE
    SRAM0_UB      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM0_UB
    SRAM0_W       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM0_W
    SRAM1_A       : InOut STD_LOGIC_VECTOR(18 DOWNTO 0);     -- ObjectKind=Port|PrimaryId=SRAM1_A[18..0]
    SRAM1_D       : InOut STD_LOGIC_VECTOR(15 DOWNTO 0);     -- ObjectKind=Port|PrimaryId=SRAM1_D[15..0]
    SRAM1_E       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM1_E
    SRAM1_LB      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM1_LB
    SRAM1_OE      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM1_OE
    SRAM1_UB      : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM1_UB
    SRAM1_W       : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SRAM1_W
    SSPCLK        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SSPCLK
    SSPEN         : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SSPEN
    SSPFRM        : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SSPFRM
    SSPRX_UART2RX : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SSPRX_UART2RX
    SSPTX_UART2TX : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SSPTX_UART2TX
    SW0           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW0
    SW1           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW1
    SW2           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW2
    SW3           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW3
    SW4           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW4
    SW5           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW5
    SW6           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW6
    SW7           : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SW7
    SWC0          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWC0
    SWC1          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWC1
    SWC2          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWC2
    SWC3          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWC3
    SWR0          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWR0
    SWR1          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWR1
    SWR2          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWR2
    SWR3          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=SWR3
    TEST          : InOut STD_LOGIC;                         -- ObjectKind=Port|PrimaryId=TEST
    VDRIVE        : InOut STD_LOGIC                          -- ObjectKind=Port|PrimaryId=VDRIVE
  );
  attribute MacroCell : boolean;

End IOStandardsTests;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of IOStandardsTests is


begin
end structure;
------------------------------------------------------------

