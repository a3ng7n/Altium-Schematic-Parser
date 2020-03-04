//*******************************************************************************
// * Extrnal Bus Controller Register Constants
//*******************************************************************************
#define EBC_DCR_BASE                0x12
#define PeripheralControl_Address   EBC_DCR_BASE + 0x0   // External bus controller address reg
#define PeripheralControl_Data      EBC_DCR_BASE + 0x1   // External bus controller data reg

// values for PeripheralControl register - indirect addressing of these registers
#define EBC0_B0CR     0x00   // periph bank 0 config reg
#define EBC0_B1CR     0x01   // periph bank 1 config reg
#define EBC0_B2CR     0x02   // periph bank 2 config reg
#define EBC0_B3CR     0x03   // periph bank 3 config reg
#define EBC0_B4CR     0x04   // periph bank 4 config reg
#define EBC0_B5CR     0x05   // periph bank 5 config reg
#define EBC0_B6CR     0x06   // periph bank 6 config reg
#define EBC0_B7CR     0x07   // periph bank 7 config reg

#define EBC0_B0AP     0x10   // periph bank 0 access parameters
#define EBC0_B1AP     0x11   // periph bank 1 access parameters
#define EBC0_B2AP     0x12   // periph bank 2 access parameters
#define EBC0_B3AP     0x13   // periph bank 3 access parameters
#define EBC0_B4AP     0x14   // periph bank 4 access parameters
#define EBC0_B5AP     0x15   // periph bank 5 access parameters
#define EBC0_B6AP     0x16   // periph bank 6 access parameters
#define EBC0_B7AP     0x17   // periph bank 7 access parameters
#define EBC0_BEAR     0x20   // periph bus error addr reg
#define EBC0_BESR0    0x21   // periph bus error status reg 0
#define EBC0_BESR1    0x22   // periph bus error status reg 1
#define EBC0_CFG      0x23   // external periph control reg

#define EBC0_CFGADDR  0x12
#define EBC0_CFGDATA  0x13
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// EBC Bank Access Parameters for fast BRAM - 2 cycle read, 3 cycle write
//-------------------------------------------------------------------------------
// BME  0--- ---- ---- ---- ---- ---- ---- ---- Burst Mode Enable
// TWT  -000 0000 1--- ---- ---- ---- ---- ---- Transfer Wait                - 1 cycle
// FWT  -000 00-- ---- ---- ---- ---- ---- ---- First Wait                   - Not used
// BWT  ---- --00 1--- ---- ---- ---- ---- ---- Burst Wait                   - Not used
// RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
// CSN  ---- ---- ---- 00-- ---- ---- ---- ---- Chip-Select On Timing        - 0 Cycle
// OEN  ---- ---- ---- --00 ---- ---- ---- ---- Output-Enable On Timing      - 0 Cycle
// WBN  ---- ---- ---- ---- 01-- ---- ---- ---- Write-Byte Enable On Timing  - 1 Cycle
// WBF  ---- ---- ---- ---- --01 ---- ---- ---- Write-Byte Enable Off Timing - 1 Cycle
// TH   ---- ---- ---- ---- ---- 000- ---- ---- Transfer Hold                - 0 Cycle
// RE   ---- ---- ---- ---- ---- ---0 ---- ---- Ready Enable                 - No external wait state control
// SOR  ---- ---- ---- ---- ---- ---- 1--- ---- Sample on Ready
// BEM  ---- ---- ---- ---- ---- ---- -0-- ---- Byte Enable Mode
// PEN  ---- ---- ---- ---- ---- ---- --0- ---- Parity Enable
// RES  ---- ---- ---- ---- ---- ---- ---0 0000 Reserved
//-------------------------------------------------------------------------------
//      0000 0000 1000 0000 0101 0000 1000 0000
//      0    0    8    0    5    0    8    0
#define EBC_FastBRAM  0x00805080
//-------------------------------------------------------------------------------


//-------------------------------------------------------------------------------
// EBC Bank Access Parameters as a Wishbone BUS.
// Use PerReady to control wait-states (device-paced transfers).
//-------------------------------------------------------------------------------
// BME  0--- ---- ---- ---- ---- ---- ---- ---- Burst Mode Enable
// TWT  -000 0001 0--- ---- ---- ---- ---- ---- Transfer Wait                    - 2 cycles
// FWT  -000 00-- ---- ---- ---- ---- ---- ---- First Wait                       - Not used
// BWT  ---- --00 0--- ---- ---- ---- ---- ---- Burst Wait                       - Not used
// RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
// CSN  ---- ---- ---- 00-- ---- ---- ---- ---- Chip-Select On Timing            - 0 Cycle
// OEN  ---- ---- ---- --00 ---- ---- ---- ---- Output-Enable On Timing          - 0 Cycle
// WBN  ---- ---- ---- ---- 00-- ---- ---- ---- Write-Byte Enable On Timing      - 0 Cycle
// WBF  ---- ---- ---- ---- --00 ---- ---- ---- Write-Byte Enable Off Timing     - 0 Cycle
// TH   ---- ---- ---- ---- ---- 000- ---- ---- Transfer Hold                    - 0 Cycle
// RE   ---- ---- ---- ---- ---- ---1 ---- ---- Ready Enable                     - Enable external wait state control
// SOR  ---- ---- ---- ---- ---- ---- 1--- ---- Sample on Ready                  - Sample in the same cycle
// BEM  ---- ---- ---- ---- ---- ---- -0-- ---- Byte Enable Mode
// PEN  ---- ---- ---- ---- ---- ---- --0- ---- Parity Enable
// RES  ---- ---- ---- ---- ---- ---- ---0 0000 Reserved
//-------------------------------------------------------------------------------
//      0000 0001 0000 0000 0000 0001 1000 0000
//      0    1    0    0    0    1    8    0
#define EBC_WishboneBus  0x01000180
//-------------------------------------------------------------------------------


//-------------------------------------------------------------------------------
// EBC Bank Access Parameters for AMD 29LV Flash Memory
//-------------------------------------------------------------------------------
// BME  1--- ---- ---- ---- ---- ---- ---- ---- Burst Mode Enable
// TWT  -001 1101 0--- ---- ---- ---- ---- ---- Transfer Wait                    - Not used
// FWT  -001 11-- ---- ---- ---- ---- ---- ---- First Wait                       - 7 Cycles
// BWT  ---- --01 0--- ---- ---- ---- ---- ---- Burst Wait                       - 2 Cycles
// RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
// CSN  ---- ---- ---- 00-- ---- ---- ---- ---- Chip-Select On Timing            - 0 Cycle
// OEN  ---- ---- ---- --01 ---- ---- ---- ---- Output-Enable On Timing          - 1 Cycle
// WBN  ---- ---- ---- ---- 01-- ---- ---- ---- Write-Byte Enable On Timing      - 1 Cycle
// WBF  ---- ---- ---- ---- --01 ---- ---- ---- Write-Byte Enable Off Timing     - 1 Cycle
// TH   ---- ---- ---- ---- ---- 010- ---- ---- Transfer Hold                    - 2 Cycle
// RE   ---- ---- ---- ---- ---- ---0 ---- ---- Ready Enable                     - No external wait state control
// SOR  ---- ---- ---- ---- ---- ---- 1--- ---- Sample on Ready
// BEM  ---- ---- ---- ---- ---- ---- -0-- ---- Byte Enable Mode
// PEN  ---- ---- ---- ---- ---- ---- --0- ---- Parity Enable
// RES  ---- ---- ---- ---- ---- ---- ---0 0000 Reserved
//-------------------------------------------------------------------------------
//      1001 1101 0000 0001 0101 0100 1000 0000
//      9    B    0    1    5    4    8    0
#define EBC_AMD29_Flash  0x9B015480
//-------------------------------------------------------------------------------


//-------------------------------------------------------------------------------
// EBC Configuration Registers for 16MB of 32-bit RAM at 0x000
//-------------------------------------------------------------------------------
// BAS  0000 0000 0000                           Base Address Select 0x000       - Lowest Memory region
// BS                  100                       Bank Size           0x0         - (16MB)
// BU                     1 1                    Bank Usage          0x3         - (R/W)
// BW                        10                  Bus Width           0x2         - (32-Bits)
// RES                         0 0000 0000 0000  Reserved
//-------------------------------------------------------------------------------
//      1111 1111 1111 1001 1100 0000 0000 0000
//      0    0    0    9    C    0    0    0
#define EBC_16MB_32Bit_RAM_000  0x0009C000
//-------------------------------------------------------------------------------


//-------------------------------------------------------------------------------
// EBC Bank Configuration Registers for 16MB of 32-bit RAM at 0x010
//-------------------------------------------------------------------------------
// BAS  0000 0001 0000                           Base Address Select 0x010       - 0x0100_0000 To 0x07FF_FFFF
// BS                  100                       Bank Size           0x0         - (16 MB)
// BU                     1 1                    Bank Usage          0x3         - (R/W)
// BW                        10                  Bus Width           0x2         - (32-Bits)
// RES                         0 0000 0000 0000  Reserved
//-------------------------------------------------------------------------------
//      0000 0001 0000 1001 1100 0000 0000 0000
//      0    1    0    9    C    0    0    0
#define EBC_16MB_32Bit_RAM_010  0x0109C000
//-------------------------------------------------------------------------------


//-------------------------------------------------------------------------------
// EBC Bank Configuration Registers for 16MB of 32-bit RAM at 0xFF0
//-------------------------------------------------------------------------------
// BAS  1111 1111 0000                           Base Address Select 0xFF0       - 0xFF00_0000 To 0xFFFF_FFFF
// BS                  100                       Bank Size           0x0         - (16 MB)
// BU                     1 1                    Bank Usage          0x3         - (R/W)
// BW                        10                  Bus Width           0x2         - (32-Bits)
// RES                         0 0000 0000 0000  Reserved
//-------------------------------------------------------------------------------
//      0000 0001 0000 1001 1100 0000 0000 0000
//      F    F    0    9    C    0    0    0
#define EBC_16MB_32Bit_RAM_FFF  0xFF09C000
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// EBC Bank Configuration Registers for 16MB of 16-bit Flash at 0x070 - UNTESTED
//-------------------------------------------------------------------------------
// BAS  0111 0000 0000                           Base Address Select 0x700   - 0x7000_0000 To 77FF_FFFF
// BS                  111                       Bank Size           0x7     - (128MB)
// BU                     1 1                    Bank Usage          0x3     - (R/W)
// BW                        01                  Bus Width           0x1     - (16-Bits)
// RES                         0 0000 0000 0000  Reserved
//-------------------------------------------------------------------------------
//      0111 0000 0000 1111 1010 0000 0000 0000
//      7    0    0    F    A    0    0    0
#define EBC_16MB_16Bit_Flash_700  0x700FA000
//-------------------------------------------------------------------------------

//*******************************************************************************
// * SDRAM Controller Register Constants
//*******************************************************************************
#define SC_DCR_BASE                0x10
#define SDRAMControl_Address       SC_DCR_BASE + 0x0   // External bus controller address reg
#define SDRAMControl_Data          SC_DCR_BASE + 0x1   // External bus controller data reg

// values for SDRAMControl register - indirect addressing of these registers
#define SDRAM0_BESR0               0x00 //R/Clear Bus Error Syndrome Register 0 14-19
#define SDRAM0_BESR1               0x08 //R/Clear Bus Error Syndrome Register 1 14-20
#define SDRAM0_BEAR                0x10 //R Bus Error Address Register 14-18
#define SDRAM0_CFG                 0x20 //R/W SDRAM Configuration 14-4
#define SDRAM0_STATUS              0x24 //R SDRAM Controller Status 14-6
#define SDRAM0_RTR                 0x30 //R/W Refresh Timer Register 14-15
#define SDRAM0_PMIT                0x34 //R/W Power Management Idle Timer 14-21
#define SDRAM0_B0CR                0x40 //R/W Memory Bank 0 Configuration Register 14-7
#define SDRAM0_B1CR                0x44 //R/W Memory Bank 1 Configuration Register 14-7
#define SDRAM0_B2CR                0x48 //R/W Memory Bank 2 Configuration Register 14-7
#define SDRAM0_B3CR                0x4C //R/W Memory Bank 3 Configuration Register 14-7
#define SDRAM0_TR                  0x80 //R/W SDRAM Timing Register 14-10
#define SDRAM0_ECCCFG              0x94 //R/W ECC Configuration 14-16
#define SDRAM0_ECCESR              0x98 //R/Clear ECC Error Status 14-18
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// SDRAM0_CFG Memory Controller Configuration Register Reset Value
//-------------------------------------------------------------------------------
// DCE  0--- ---- ---- ---- ---- ---- ---- ---- SDRAM Controller Enable          - Enable
// SRE  -0-- ---- ---- ---- ---- ---- ---- ---- Self-Refresh Enable              - Disable
// PME  --0- ---- ---- ---- ---- ---- ---- ---- Power Management Enable          - Disable
// MCHK ---0 ---- ---- ---- ---- ---- ---- ---- Memory Data Error Checking       - Disable
// REGE ---- 0--- ---- ---- ---- ---- ---- ---- Registered Memory Enable         - Disable
// DRW  ---- -00- ---- ---- ---- ---- ---- ---- SDRAM Width                      - Fixed 00 = 32-bit
// BRPF ---- ---0 1--- ---- ---- ---- ---- ---- Burst Read Prefetch Granularity  - 01 = 16 bytes
// ECCDD---- ---- -1-- ---- ---- ---- ---- ---- ECC Driver Diable                - 1 = Disable
// EMDUL---- ---- --0- ---- ---- ---- ---- ---- Enable Memory Data Unless Read   - 0 = MemData0:31 are placed in high impedance unless a memory write
// RES  ---- ---- ---0 0000 0000 0000 0000 0000 Reserved
//-------------------------------------------------------------------------------
//      0000 0000 1100 0000 0000 0000 0000 0000
//      0    0    C    0    0    0    0    0
#define SC_SDRAM0_CFG_RESET  0x00C00000
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// SDRAM0_CFG Memory Controller Configuration Register
//-------------------------------------------------------------------------------
// DCE  1--- ---- ---- ---- ---- ---- ---- ---- SDRAM Controller Enable          - Enable
// SRE  -0-- ---- ---- ---- ---- ---- ---- ---- Self-Refresh Enable              - Disable
// PME  --0- ---- ---- ---- ---- ---- ---- ---- Power Management Enable          - Disable
// MCHK ---0 ---- ---- ---- ---- ---- ---- ---- Memory Data Error Checking       - Disable
// REGE ---- 0--- ---- ---- ---- ---- ---- ---- Registered Memory Enable         - Disable
// DRW  ---- -00- ---- ---- ---- ---- ---- ---- SDRAM Width                      - Fixed 00 = 32-bit
// BRPF ---- ---0 1--- ---- ---- ---- ---- ---- Burst Read Prefetch Granularity  - 01 = 16 bytes
// ECCDD---- ---- -1-- ---- ---- ---- ---- ---- ECC Driver Diable                - 1 = Disable
// EMDUL---- ---- --0- ---- ---- ---- ---- ---- Enable Memory Data Unless Read   - 0 = MemData0:31 are placed in high impedance unless a memory write
// RES  ---- ---- ---0 0000 0000 0000 0000 0000 Reserved
//-------------------------------------------------------------------------------
//      1000 0000 1100 0000 0000 0000 0000 0000
//      8    0    C    0    0    0    0    0
#define SC_SDRAM0_CFG  0x80C00000
//-------------------------------------------------------------------------------


//-------------------------------------------------------------------------------
// SDRAM Timing Register
//-------------------------------------------------------------------------------
// RES  0000 00-- ---- ---- ---- ---- ---- ---- Reserved
// CASL ---- ---0 1--- ---- ---- ---- ---- ---- SDRAM ~CAS latency                                      - 01 = 2 MemClkOut cycles
// RES  ---- ---- -000 ---- ---- ---- ---- ---- Reserved
// PTA  ---- ---- ---- 01-- ---- ---- ---- ---- SDRAM Precharge Command to next Active Command Minimum  - 01 = 2 MemClkOut cycles
// CTP  ---- ---- ---- --01 ---- ---- ---- ---- SDRAM Read/Write Command to Precharge Command Minimum   - 01 = 2 MemClkOut cycles
// LDF  ---- ---- ---- ---- 01-- ---- ---- ---- SDRAM Command Leadoff                                   - 01 = 2 MemClkOut cycles
// RES  ---- ---- ---- ---- --00 0000 000- ---- Reserved
// RFTA ---- ---- ---- ---- ---- ---- ---0 00-- SDRAM ~CAS before ~RAS Refresh Command to next Active Command Mininum - 000 = 4 MemClkOut cycles
// RCD  ---- ---- ---- ---- ---- ---- ---- --01 SDRAM RAS to CAS Delay                                  - 01 = 2 MemClkOut cycles
//-------------------------------------------------------------------------------
//      01000 0000 1000 0101 0100 0000 0000 0001
//      4    0    8    5     4    0    0    1
#define SC_TR_CFG  0x40854001
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// SDRAM Memory Bank Configuration of 64 MB of SDRAM at 0x800
//-------------------------------------------------------------------------------
// BA   1000 0000 00-- ---- ---- ---- ---- ---- Base Address                     - 0x8000_0000
// RES  ---- ---- --00 ---- ---- ---- ---- ---- Reserved
// SZ   ---- ---- ---- 100- ---- ---- ---- ---- Size                             - 100 = 64M byte
// REZ  ---- ---- ---- ---0 ---- ---- ---- ---- Reserved
// AM   ---- ---- ---- ---- 010- ---- ---- ---- Addressing Mode                  - 010 = Mode 3, 13 (row lines) x 9 (column lines) x 4 (banks)
// RES  ---- ---- ---- ---- ---0 0000 0000 000- Reserved
// BE   ---- ---- ---- ---- ---- ---- ---- ---1 Memory Bank Enable               - Enable
//-------------------------------------------------------------------------------
//      1000 0000 0000 1000 0100 0000 0000 0001
//      8    0    0    8    4    0    0    1
#define SC_64MB_32bit_SDRAM_800  0x80084001
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// SDRAM Refresh Time Register value (RTR)
//-------------------------------------------------------------------------------
// 00   00-- ---- ---- ---- ---- ---- ---- ---- 00
// IV   --11 0000 0000 0--- ---- ---- ---- ---- Interval Programmable            - 0x3000 MemClkOut clock cycles
// 000  ---- ---- ---- -000 ---- ---- ---- ---- 000
// RES  ---- ---- ---- ---- 0000 0000 0000 0000 Reserved
//-------------------------------------------------------------------------------
//      0011 0000 0000 0000 0000 0000 0000 0000
//      3    0    0    0    0    0    0    0
#define SC_RTR_3000  0x30000000
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// EBC configuration Register (EBC0_CFG)
//-------------------------------------------------------------------------------
// EBTC 0--- ---- ---- ---- ---- ---- ---- ---- External Bus Three-state Control
// PTD  -0-- ---- ---- ---- ---- ---- ---- ---- Device-Paced Time-out Disable    Enable time-outs
// RTC  --11 1--- ---- ---- ---- ---- ---- ---- Ready Timeout Count              2048 PerClk cycles
// EMPL ---- -00- ---- ---- ---- ---- ---- ---- External Master Priority Low     00 Low
// EMPH ---- ---0 0--- ---- ---- ---- ---- ---- External Master Priority High    00 Low
// CSTC ---- ---- -0-- ---- ---- ---- ---- ---- Chip Select Three-state Control
// BPF  ---- ---- --00 ---- ---- ---- ---- ---- Burst Perfectch                  00 Perfech 1 doubleword
// EMS  ---- ---- ---- 00-- ---- ---- ---- ---- External Master Size             00 8-bit
// PME  ---- ---- ---- --0- ---- ---- ---- ---- Power Management Enable          0 Disable
// PMT  ---- ---- ---- ---0 000- ---- ---- ---- Power Management Timer           0000 
// RES  ---- ---- ---- ---- ---- ---- ---- ---- Reserved
//-------------------------------------------------------------------------------
//      0011 1000 0000 0000 0000 0000 0000 0000
//      3    8    0    0    0    0    0    0
#define EBC0_CFG_Value  0x38000000
//-------------------------------------------------------------------------------


#pragma section "STARTUP"
void startup()
{
    unsigned int mem_access_control_address        = EBC0_B2AP;
    unsigned int mem_access_control_data           = EBC_WishboneBus;
    unsigned int mem_configuration_control_address = EBC0_B2CR;
    unsigned int mem_configuration_control_data    = EBC_16MB_32Bit_RAM_010;

    unsigned int pref_access_control_address        = EBC0_B0AP;
    unsigned int pref_access_control_data           = EBC_WishboneBus;
    unsigned int pref_configuration_control_address = EBC0_B0CR;
    unsigned int pref_configuration_control_data    = EBC_16MB_32Bit_RAM_FFF;

    unsigned int flash_access_control_address        = EBC0_B5AP;
    unsigned int flash_access_control_data           = EBC_AMD29_Flash;
    unsigned int flash_configuration_control_address = EBC0_B5CR;
    unsigned int flash_configuration_control_data    = EBC_16MB_16Bit_Flash_700;

    unsigned int sdram_configuration_address         = SDRAM0_CFG;
    unsigned int sdram_configuration_reset_data      = SC_SDRAM0_CFG_RESET;
    unsigned int sdram_configuration_data            = SC_SDRAM0_CFG;
    unsigned int sdram_timing_register_address       = SDRAM0_TR;
    unsigned int sdram_timing_register_data          = SC_TR_CFG;
    unsigned int sdram_refresh_time_register_address = SDRAM0_RTR;
    unsigned int sdram_refresh_time_register_data    = SC_RTR_3000;
    unsigned int sdram_memory_bank_address           = SDRAM0_B0CR;
    unsigned int sdram_memory_bank_data              = SC_64MB_32bit_SDRAM_800;

    unsigned int ebc0_config_register_address        = EBC0_CFGADDR;
    unsigned int ebc0_config_register                = EBC0_CFG;
    unsigned int ebc0_config_register_data           = EBC0_CFGDATA;
    unsigned int ebc0_config_value                   = EBC0_CFG_Value;

// 0x0100_0000 16MB ram
    __asm(
"    mtdcr   %0,%2        \n"
"    mtdcr   %1,%3        \n"
"    mtdcr   %0,%4        \n"
"    mtdcr   %1,%5        \n"
    :
    :"i"(PeripheralControl_Address),"i"(PeripheralControl_Data),"r"(mem_access_control_address),"r"(mem_access_control_data),"r"(mem_configuration_control_address),"r"(mem_configuration_control_data)
    :
    );

// 0xFF00_0000 peripheral
    __asm(
"    mtdcr   %0,%2        \n"
"    mtdcr   %1,%3        \n"
"    mtdcr   %0,%4        \n"
"    mtdcr   %1,%5        \n"
    :
    :"i"(PeripheralControl_Address),"i"(PeripheralControl_Data),"r"(pref_access_control_address),"r"(pref_access_control_data),"r"(pref_configuration_control_address),"r"(pref_configuration_control_data)
    :
    );

//stop sdram
    __asm(
"    mtdcr   %0,%2        \n"
"    mtdcr   %1,%3        \n"
    :
    :"i"(SDRAMControl_Address),"i"(SDRAMControl_Data),"r"(sdram_configuration_address),"r"(sdram_configuration_reset_data)
    :
    );

// 0x8000_0000 sdram
    __asm(
"    mtdcr   %0,%2        \n"
"    mtdcr   %1,%3        \n"
"    mtdcr   %0,%4        \n"
"    mtdcr   %1,%5        \n"
"    mtdcr   %0,%6        \n"
"    mtdcr   %1,%7        \n"
    :
    :"i"(SDRAMControl_Address),"i"(SDRAMControl_Data),"r"(sdram_timing_register_address),"r"(sdram_timing_register_data),"r"(sdram_refresh_time_register_address),"r"(sdram_refresh_time_register_data),"r"(sdram_memory_bank_address),"r"(sdram_memory_bank_data)
    :
    );

//start sdram
    __asm(
"    mtdcr   %0,%2        \n"
"    mtdcr   %1,%3        \n"
    :
    :"i"(SDRAMControl_Address),"i"(SDRAMControl_Data),"r"(sdram_configuration_address),"r"(sdram_configuration_data)
    :
    );

// 0x7000_0000 flash
    __asm(
"    mtdcr   %0,%2        \n"
"    mtdcr   %1,%3        \n"
"    mtdcr   %0,%4        \n"
"    mtdcr   %1,%5        \n"
    :
    :"i"(PeripheralControl_Address),"i"(PeripheralControl_Data),"r"(flash_access_control_address),"r"(flash_access_control_data),"r"(flash_configuration_control_address),"r"(flash_configuration_control_data)
    :
    );


    __asm(
"    mtdcr   %0,%2        \n"
"    mtdcr   %1,%3        \n"
    :
    :"i"(PeripheralControl_Address),"i"(PeripheralControl_Data),"r"(ebc0_config_register),"r"(ebc0_config_value)
    :
    );


}
#pragma endsection
