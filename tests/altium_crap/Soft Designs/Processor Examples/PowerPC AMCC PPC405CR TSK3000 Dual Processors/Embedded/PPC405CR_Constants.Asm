;*******************************************************************************
; * Extrnal Bus Controller Register Constants
;*******************************************************************************
EBC_DCR_BASE              .equ  0x12
PeripheralControl_Address .equ  EBC_DCR_BASE + 0x0   ; External bus controller address reg
PeripheralControl_Data    .equ  EBC_DCR_BASE + 0x1   ; External bus controller data reg

; values for PeripheralControl register - indirect addressing of these registers
EBC0_B0CR   .equ  0x00   ; periph bank 0 config reg
EBC0_B1CR   .equ  0x01   ; periph bank 1 config reg
EBC0_B2CR   .equ  0x02   ; periph bank 2 config reg
EBC0_B3CR   .equ  0x03   ; periph bank 3 config reg
EBC0_B4CR   .equ  0x04   ; periph bank 4 config reg
EBC0_B5CR   .equ  0x05   ; periph bank 5 config reg
EBC0_B6CR   .equ  0x06   ; periph bank 6 config reg
EBC0_B7CR   .equ  0x07   ; periph bank 7 config reg

EBC0_B0AP   .equ  0x10   ; periph bank 0 access parameters
EBC0_B1AP   .equ  0x11   ; periph bank 1 access parameters
EBC0_B2AP   .equ  0x12   ; periph bank 2 access parameters
EBC0_B3AP   .equ  0x13   ; periph bank 3 access parameters
EBC0_B4AP   .equ  0x14   ; periph bank 4 access parameters
EBC0_B5AP   .equ  0x15   ; periph bank 5 access parameters
EBC0_B6AP   .equ  0x16   ; periph bank 6 access parameters
EBC0_B7AP   .equ  0x17   ; periph bank 7 access parameters
EBC0_BEAR   .equ  0x20   ; periph bus error addr reg
EBC0_BESR0  .equ  0x21   ; periph bus error status reg 0
EBC0_BESR1  .equ  0x22   ; periph bus error status reg 1
EBC0_CFG    .equ  0x23   ; external periph control reg
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; EBC Bank Access Parameters for fast BRAM - 2 cycle read, 3 cycle write
;-------------------------------------------------------------------------------
; BME  0--- ---- ---- ---- ---- ---- ---- ---- Burst Mode Enable
; TWT  -000 0000 1--- ---- ---- ---- ---- ---- Transfer Wait                - 1 cycle
; FWT  -000 00-- ---- ---- ---- ---- ---- ---- First Wait                   - Not used
; BWT  ---- --00 1--- ---- ---- ---- ---- ---- Burst Wait                   - Not used
; RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
; CSN  ---- ---- ---- 00-- ---- ---- ---- ---- Chip-Select On Timing        - 0 Cycle
; OEN  ---- ---- ---- --00 ---- ---- ---- ---- Output-Enable On Timing      - 0 Cycle
; WBN  ---- ---- ---- ---- 01-- ---- ---- ---- Write-Byte Enable On Timing  - 1 Cycle
; WBF  ---- ---- ---- ---- --01 ---- ---- ---- Write-Byte Enable Off Timing - 1 Cycle
; TH   ---- ---- ---- ---- ---- 000- ---- ---- Transfer Hold                - 0 Cycle
; RE   ---- ---- ---- ---- ---- ---0 ---- ---- Ready Enable                 - No external wait state control
; SOR  ---- ---- ---- ---- ---- ---- 1--- ---- Sample on Ready
; BEM  ---- ---- ---- ---- ---- ---- -0-- ---- Byte Enable Mode
; PEN  ---- ---- ---- ---- ---- ---- --0- ---- Parity Enable
; RES  ---- ---- ---- ---- ---- ---- ---0 0000 Reserved
;-------------------------------------------------------------------------------
;      0000 0000 1000 0000 0101 0000 1000 0000
;      0    0    8    0    5    0    8    0
EBC_FastBRAM .equ 0x00805080
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; EBC Bank Access Parameters as a Wishbone BUS.
; Use PerReady to control wait-states (device-paced transfers).
;-------------------------------------------------------------------------------
; BME  0--- ---- ---- ---- ---- ---- ---- ---- Burst Mode Enable
; TWT  -000 0001 0--- ---- ---- ---- ---- ---- Transfer Wait                    - 2 cycles
; FWT  -000 00-- ---- ---- ---- ---- ---- ---- First Wait                       - Not used
; BWT  ---- --00 0--- ---- ---- ---- ---- ---- Burst Wait                       - Not used
; RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
; CSN  ---- ---- ---- 00-- ---- ---- ---- ---- Chip-Select On Timing            - 0 Cycle
; OEN  ---- ---- ---- --00 ---- ---- ---- ---- Output-Enable On Timing          - 0 Cycle
; WBN  ---- ---- ---- ---- 00-- ---- ---- ---- Write-Byte Enable On Timing      - 0 Cycle
; WBF  ---- ---- ---- ---- --00 ---- ---- ---- Write-Byte Enable Off Timing     - 0 Cycle
; TH   ---- ---- ---- ---- ---- 000- ---- ---- Transfer Hold                    - 0 Cycle
; RE   ---- ---- ---- ---- ---- ---1 ---- ---- Ready Enable                     - Enable external wait state control
; SOR  ---- ---- ---- ---- ---- ---- 1--- ---- Sample on Ready                  - Sample in the same cycle
; BEM  ---- ---- ---- ---- ---- ---- -0-- ---- Byte Enable Mode
; PEN  ---- ---- ---- ---- ---- ---- --0- ---- Parity Enable
; RES  ---- ---- ---- ---- ---- ---- ---0 0000 Reserved
;-------------------------------------------------------------------------------
;      0000 0001 0000 0000 0000 0001 1000 0000
;      0    1    0    0    0    1    8    0
EBC_WishboneBus .equ 0x01000180
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; EBC Configuration Registers for 16MB of 32-bit RAM at 0x000
;-------------------------------------------------------------------------------
; BAS  0000 0000 0000                           Base Address Select 0x000       - Lowest Memory region
; BS                  100                       Bank Size           0x0         - (16MB)
; BU                     1 1                    Bank Usage          0x3         - (R/W)
; BW                        10                  Bus Width           0x2         - (32-Bits)
; RES                         0 0000 0000 0000  Reserved
;-------------------------------------------------------------------------------
;      1111 1111 1111 1001 1100 0000 0000 0000
;      0    0    0    9    C    0    0    0
EBC_16MB_32Bit_RAM_000 .equ 0x0009C000
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; EBC Bank Configuration Registers for 16MB of 32-bit RAM at 0x010
;-------------------------------------------------------------------------------
; BAS  0000 0001 0000                           Base Address Select 0x010       - 0x0100_0000 To 0x07FF_FFFF
; BS                  100                       Bank Size           0x0         - (16 MB)
; BU                     1 1                    Bank Usage          0x3         - (R/W)
; BW                        10                  Bus Width           0x2         - (32-Bits)
; RES                         0 0000 0000 0000  Reserved
;-------------------------------------------------------------------------------
;      0000 0001 0000 1001 1100 0000 0000 0000
;      0    1    0    9    C    0    0    0
EBC_16MB_32Bit_RAM_010 .equ 0x0109C000
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; EBC Bank Configuration Registers for 32MB of 32-bit RAM at 0x020
;-------------------------------------------------------------------------------
; BAS  0000 0002 0000                           Base Address Select 0x020       - 0x0100_0000 To 0x07FF_FFFF
; BS                  101                       Bank Size           0x0         - (32 MB)
; BU                     1 1                    Bank Usage          0x3         - (R/W)
; BW                        10                  Bus Width           0x2         - (32-Bits)
; RES                         0 0000 0000 0000  Reserved
;-------------------------------------------------------------------------------
;      0000 0002 0000 1011 1100 0000 0000 0000
;      0    2    0    B    C    0    0    0
EBC_32MB_32Bit_RAM_020 .equ 0x020BC000
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; EBC Bank Configuration Registers for 64MB of 32-bit RAM at 0x040
;-------------------------------------------------------------------------------
; BAS  0000 0004 0000                           Base Address Select 0x040       - 0x0100_0000 To 0x07FF_FFFF
; BS                  110                       Bank Size           0x0         - (64 MB)
; BU                     1 1                    Bank Usage          0x3         - (R/W)
; BW                        10                  Bus Width           0x2         - (32-Bits)
; RES                         0 0000 0000 0000  Reserved
;-------------------------------------------------------------------------------
;      0000 0004 0000 1101 1100 0000 0000 0000
;      0    4    0    D    C    0    0    0
EBC_64MB_32Bit_RAM_040 .equ 0x040DC000
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; EBC Bank Configuration Registers for 128MB of 32-bit RAM at 0x080
;-------------------------------------------------------------------------------
; BAS  0000 0008 0000                           Base Address Select 0x080       - 0x0100_0000 To 0x07FF_FFFF
; BS                  111                       Bank Size           0x0         - (128 MB)
; BU                     1 1                    Bank Usage          0x3         - (R/W)
; BW                        10                  Bus Width           0x2         - (32-Bits)
; RES                         0 0000 0000 0000  Reserved
;-------------------------------------------------------------------------------
;      0000 0008 0000 1111 1100 0000 0000 0000
;      0    8    0    F    C    0    0    0
EBC_128MB_32Bit_RAM_080 .equ 0x080FC000
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; EBC Bank Configuration Registers for 16MB of 32-bit RAM at 0xFF0
;-------------------------------------------------------------------------------
; BAS  1111 1111 0000                           Base Address Select 0xFF0       - 0xFF00_0000 To 0xFFFF_FFFF
; BS                  100                       Bank Size           0x0         - (16 MB)
; BU                     1 1                    Bank Usage          0x3         - (R/W)
; BW                        10                  Bus Width           0x2         - (32-Bits)
; RES                         0 0000 0000 0000  Reserved
;-------------------------------------------------------------------------------
;      0000 0001 0000 1001 1100 0000 0000 0000
;      F    F    0    9    C    0    0    0
EBC_16MB_32Bit_RAM_FFF .equ 0xFF09C000
;-------------------------------------------------------------------------------


;*******************************************************************************
; * SDRAM Controller Register Constants
;*******************************************************************************
SC_DCR_BASE               .equ  0x10
SDRAMControl_Address      .equ  SC_DCR_BASE + 0x0   ; External bus controller address reg
SDRAMControl_Data         .equ  SC_DCR_BASE + 0x1   ; External bus controller data reg

; values for SDRAMControl register - indirect addressing of these registers
SDRAM0_BESR0              .equ  0x00 ;R/Clear Bus Error Syndrome Register 0 14-19
SDRAM0_BESR1              .equ  0x08 ;R/Clear Bus Error Syndrome Register 1 14-20
SDRAM0_BEAR               .equ  0x10 ;R Bus Error Address Register 14-18
SDRAM0_CFG                .equ  0x20 ;R/W SDRAM Configuration 14-4
SDRAM0_STATUS             .equ  0x24 ;R SDRAM Controller Status 14-6
SDRAM0_RTR                .equ  0x30 ;R/W Refresh Timer Register 14-15
SDRAM0_PMIT               .equ  0x34 ;R/W Power Management Idle Timer 14-21
SDRAM0_B0CR               .equ  0x40 ;R/W Memory Bank 0 Configuration Register 14-7
SDRAM0_B1CR               .equ  0x44 ;R/W Memory Bank 1 Configuration Register 14-7
SDRAM0_B2CR               .equ  0x48 ;R/W Memory Bank 2 Configuration Register 14-7
SDRAM0_B3CR               .equ  0x4C ;R/W Memory Bank 3 Configuration Register 14-7
SDRAM0_TR                 .equ  0x80 ;R/W SDRAM Timing Register 14-10
SDRAM0_ECCCFG             .equ  0x94 ;R/W ECC Configuration 14-16
SDRAM0_ECCESR             .equ  0x98 ;R/Clear ECC Error Status 14-18
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; SDRAM0_CFG Memory Controller Configuration Register
;-------------------------------------------------------------------------------
; DCE  1--- ---- ---- ---- ---- ---- ---- ---- SDRAM Controller Enable          - Enable
; SRE  -0-- ---- ---- ---- ---- ---- ---- ---- Self-Refresh Enable              - Disable
; PME  --0- ---- ---- ---- ---- ---- ---- ---- Power Management Enable          - Disable
; MCHK ---0 ---- ---- ---- ---- ---- ---- ---- Memory Data Error Checking       - Disable
; REGE ---- 0--- ---- ---- ---- ---- ---- ---- Registered Memory Enable         - Disable
; DRW  ---- -00- ---- ---- ---- ---- ---- ---- SDRAM Width                      - Fixed 00 = 32-bit
; BRPF ---- ---0 1--- ---- ---- ---- ---- ---- Burst Read Prefetch Granularity  - 01 = 16 bytes
; ECCDD---- ---- -1-- ---- ---- ---- ---- ---- ECC Driver Diable                - 1 = Disable
; EMDUL---- ---- --0- ---- ---- ---- ---- ---- Enable Memory Data Unless Read   - 0 = MemData0:31 are placed in high impedance unless a memory write
; RES  ---- ---- ---0 0000 0000 0000 0000 0000 Reserved
;-------------------------------------------------------------------------------
;      1000 0000 1100 0000 0000 0000 0000 0000
;      8    0    C    0    0    0    0    0
SC_SDRAM0_CFG .equ 0x80C00000
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; SDRAM Timing Register
;-------------------------------------------------------------------------------
; RES  0000 00-- ---- ---- ---- ---- ---- ---- Reserved
; CASL ---- ---0 1--- ---- ---- ---- ---- ---- SDRAM ~CAS latency                                      - 01 = 2 MemClkOut cycles
; RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
; PTA  ---- ---- ---- 01-- ---- ---- ---- ---- SDRAM Precharge Command to next Active Command Minimum  - 01 = 2 MemClkOut cycles
; CTP  ---- ---- ---- --01 ---- ---- ---- ---- SDRAM Read/Write Command to Precharge Command Minimum   - 01 = 2 MemClkOut cycles
; LDF  ---- ---- ---- ---- 01-- ---- ---- ---- SDRAM Command Leadoff                                   - 01 = 2 MemClkOut cycles
; RES  ---- ---- ---- ---- --00 0000 000- ---- Reserved
; RFTA ---- ---- ---- ---- ---- ---- ---0 00-- SDRAM ~CAS before ~RAS Refresh Command to next Active Command Mininum - 000 = 4 MemClkOut cycles
; RCD  ---- ---- ---- ---- ---- ---- ---- --01 SDRAM RAS to CAS Delay                                  - 01 = 2 MemClkOut cycles
;-------------------------------------------------------------------------------
;      01000 0000 1000 0101 0100 0000 0000 0001
;      4    0    8    5     4    0    0    1
SC_TR_CFG .equ 0x40854001
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; SDRAM Memory Bank Configuration of 64 MB of SDRAM at 0x800
;-------------------------------------------------------------------------------
; BA   1000 0000 00-- ---- ---- ---- ---- ---- Base Address                     - 0x8000_0000
; RES  ---- ---- --00 ---- ---- ---- ---- ---- Reserved
; SZ   ---- ---- ---- 100- ---- ---- ---- ---- Size                             - 100 = 64M byte
; REZ  ---- ---- ---- ---0 ---- ---- ---- ---- Reserved
; AM   ---- ---- ---- ---- 010- ---- ---- ---- Addressing Mode                  - 010 = Mode 3, 13 (row lines) x 9 (column lines) x 4 (banks)
; RES  ---- ---- ---- ---- ---0 0000 0000 000- Reserved
; BE   ---- ---- ---- ---- ---- ---- ---- ---1 Memory Bank Enable               - Enable
;-------------------------------------------------------------------------------
;      1000 0000 0000 1000 0100 0000 0000 0001
;      8    0    0    8    4    0    0    1
SC_64MB_32bit_SDRAM_800 .equ 0x80084001
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; SDRAM Memory Bank Configuration of 64 MB of SDRAM at 0x600
;-------------------------------------------------------------------------------
; BA   0110 0000 00-- ---- ---- ---- ---- ---- Base Address                     - 0x6000_0000
; RES  ---- ---- --00 ---- ---- ---- ---- ---- Reserved
; SZ   ---- ---- ---- 100- ---- ---- ---- ---- Size                             - 100 = 64M byte
; REZ  ---- ---- ---- ---0 ---- ---- ---- ---- Reserved
; AM   ---- ---- ---- ---- 010- ---- ---- ---- Addressing Mode                  - 010 = Mode 3, 13 (row lines) x 9 (column lines) x 4 (banks)
; RES  ---- ---- ---- ---- ---0 0000 0000 000- Reserved
; BE   ---- ---- ---- ---- ---- ---- ---- ---1 Memory Bank Enable               - Enable
;-------------------------------------------------------------------------------
;      0110 0000 0000 1000 0100 0000 0000 0001
;      6    0    0    8    4    0    0    1
SC_64MB_32bit_SDRAM_600 .equ 0x60084001
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; SDRAM Refresh Time Register value (RTR)
;-------------------------------------------------------------------------------
; 00   00-- ---- ---- ---- ---- ---- ---- ---- 00
; IV   --11 0000 0000 0--- ---- ---- ---- ---- Interval Programmable            - 0x3000 MemClkOut clock cycles
; 000  ---- ---- ---- -000 ---- ---- ---- ---- 000
; RES  ---- ---- ---- ---- 0000 0000 0000 0000 Reserved
;-------------------------------------------------------------------------------
;      0011 0000 0000 0000 0000 0000 0000 0000
;      3    0    0    0    0    0    0    0
SC_RTR_3000 .equ 0x30000000
;-------------------------------------------------------------------------------




;*******************************************************************************
; * For Attached Flash Device
;*******************************************************************************

;-------------------------------------------------------------------------------
; External Module Signals
;-------------------------------------------------------------------------------
; RES  0000 ---- ---- ---- ---- ---- ---- ---- Reserved
; TRE  ---- 0--- ---- ---- ---- ---- ---- ---- GPIO1-9 enabled
; G10E ---- -0-- ---- ---- ---- ---- ---- ---- ~PerCS1 as chip select
; G11E ---- --0- ---- ---- ---- ---- ---- ---- ~PerCS2 as chip select
; G12E ---- ---0 ---- ---- ---- ---- ---- ---- ~PerCS3 as chip select
; G13E ---- ---- 0--- ---- ---- ---- ---- ---- ~PerCS4 as chip select
; G14E ---- ---- -0-- ---- ---- ---- ---- ---- ~PerCS5 as chip select
; G15E ---- ---- --0- ---- ---- ---- ---- ---- ~PerCS6 as chip select
; G16E ---- ---- ---0 ---- ---- ---- ---- ---- ~PerCS7 as chip select
; G17E ---- ---- ---- 0--- ---- ---- ---- ---- IRQ0 as an interrupt
; G18E ---- ---- ---- -0-- ---- ---- ---- ---- Enable interrupt IRQ1 as an interrupt
; G19E ---- ---- ---- --0- ---- ---- ---- ---- Enable interrupt IRQ2 as an interrupt
; G20E ---- ---- ---- ---0 ---- ---- ---- ---- Enable interrupt IRQ3 as an interrupt
; G21E ---- ---- ---- ---- 0--- ---- ---- ---- Enable interrupt IRQ4 as an interrupt
; G22E ---- ---- ---- ---- -0-- ---- ---- ---- Enable interrupt IRQ5 as an interrupt
; G23E ---- ---- ---- ---- --0- ---- ---- ---- Enable interrupt IRQ6 as an interrupt
; RES  ---- ---- ---- ---- ---0 0000 0000 0000 Reserved
;-------------------------------------------------------------------------------
;      0000 0000 0000 0000 0000 0000 0000 0000
;      0    0    0    0    0    0    0    0
EMS_CFG .equ 0x00000000
;-------------------------------------------------------------------------------


GPIO_MASK_9 .equ 0x00400000
GPIO0_IR    .equ 0xEF60071C




;-------------------------------------------------------------------------------
; EBC Bank Access Parameters for AMD 29LV Flash Memory
;-------------------------------------------------------------------------------
; BME  0--- ---- ---- ---- ---- ---- ---- ---- Burst Mode Enable
; BWT  -001 0000 0--- ---- ---- ---- ---- ---- Burst Wait                       - 10 cycles
; RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
; CSN  ---- ---- ---- 01-- ---- ---- ---- ---- Chip-Select On Timing            - 1 Cycle
; OEN  ---- ---- ---- --01 ---- ---- ---- ---- Output-Enable On Timing          - 1 Cycle
; WBN  ---- ---- ---- ---- 01-- ---- ---- ---- Write-Byte Enable On Timing      - 1 Cycle
; WBF  ---- ---- ---- ---- --01 ---- ---- ---- Write-Byte Enable Off Timing     - 1 Cycle
; TH   ---- ---- ---- ---- ---- 010- ---- ---- Transfer Hold                    - 2 Cycle
; RE   ---- ---- ---- ---- ---- ---0 ---- ---- Ready Enable                     - No external wait state control
; SOR  ---- ---- ---- ---- ---- ---- 1--- ---- Sample on Ready
; BEM  ---- ---- ---- ---- ---- ---- -0-- ---- Byte Enable Mode
; PEN  ---- ---- ---- ---- ---- ---- --0- ---- Parity Enable
; RES  ---- ---- ---- ---- ---- ---- ---0 0000 Reserved
;-------------------------------------------------------------------------------
;      1    0    0    5    5    4    8    0
EBC_AMD29_Flash .equ 0x10055480
;-------------------------------------------------------------------------------



;-------------------------------------------------------------------------------
; EBC Bank Configuration Registers for 16MB of 16-bit Flash at 0x070 - UNTESTED
;-------------------------------------------------------------------------------
; BAS  0111 0000 0000                           Base Address Select 0x700   - 0x7000_0000 To 77FF_FFFF
; BS                  111                       Bank Size           0x7     - (128MB)
; BU                     1 1                    Bank Usage          0x3     - (R/W)
; BW                        01                  Bus Width           0x1     - (16-Bits)
; RES                         0 0000 0000 0000  Reserved
;-------------------------------------------------------------------------------
;      0111 0000 0000 1111 1010 0000 0000 0000
;      7    0    0    F    A    0    0    0
EBC_16MB_16Bit_Flash_700 .equ 0x700FA000
;-------------------------------------------------------------------------------
