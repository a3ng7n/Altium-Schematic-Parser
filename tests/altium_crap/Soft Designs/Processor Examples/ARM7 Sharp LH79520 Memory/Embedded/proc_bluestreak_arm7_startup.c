#include "proc_bluestreak_arm7_startup.h"
#include "util_timing.h"

extern void init_clock( void );     // Should be in a arm7_util_timing.h or something

/*******************************************************************************
 * External Static Memory Controller (SMC) Configuration Register Summary
 ******************************************************************************/

#define SMCRegBase      0xFFFF1000
#define SMCBCR0         (SMCRegBase + 0x000)
#define SMCBCR1         (SMCRegBase + 0x004)
#define SMCBCR2         (SMCRegBase + 0x008)
#define SMCBCR3         (SMCRegBase + 0x00C)
#define SMCBCR4         (SMCRegBase + 0x010)
#define SMCBCR5         (SMCRegBase + 0x014)
#define SMCBCR6         (SMCRegBase + 0x018)


/*******************************************************************************
 * Reset, Clock Generation and Power Control (RCPC) Register Summary
 ******************************************************************************/

#define RCPCBase        0xFFFE2000
#define RCPCCtrl        (RCPCBase + 0x000)
#define IDString        (RCPCBase + 0x004)
#define RCPCRemapCtrl   (RCPCBase + 0x008)
#define SoftReset       (RCPCBase + 0x00C)
#define ResetStatus     (RCPCBase + 0x010)
#define ResetStatusClr  (RCPCBase + 0x014)
#define HCLKPrescale    (RCPCBase + 0x018)
#define CpuClkPrescale  (RCPCBase + 0x01C)
#define PeriphClkCtrl   (RCPCBase + 0x024)
#define PeriphClkCtrl2  (RCPCBase + 0x028)
#define AHBClkCtrl      (RCPCBase + 0x02C)
#define PeriphClkSel    (RCPCBase + 0x030)
#define PeriphClkSel2   (RCPCBase + 0x034)
#define PWM0Prescale    (RCPCBase + 0x038)
#define PWM1Prescale    (RCPCBase + 0x03C)
#define LCDClkPrescale  (RCPCBase + 0x040)
#define SSPClkPrescale  (RCPCBase + 0x044)
#define IntConfig       (RCPCBase + 0x080)
#define IntClear        (RCPCBase + 0x084)
#define CoreClkConfig   (RCPCBase + 0x088)

/*******************************************************************************
 * IO Configuration (IOCON) Register Summary
 ******************************************************************************/
#define IOCONBase       0xFFFE5000
#define MemMux          (IOCONBase + 0x000)
#define LCDMux          (IOCONBase + 0x004)
#define MiscMux         (IOCONBase + 0x008)
#define DMAMux          (IOCONBase + 0x00C)
#define UARTMux         (IOCONBase + 0x010)
#define SSPMux          (IOCONBase + 0x014)

/*******************************************************************************
 * SDRAM Configuration (SDRC) Register Summary
 ******************************************************************************/
#define SDRCBase        0xFFFF2000
#define SDRCConfig0     *((volatile unsigned *)(SDRCBase + 0x000))
#define SDRCConfig1     *((volatile unsigned *)(SDRCBase + 0x004))
#define SDRCRefTimer    *((volatile unsigned *)(SDRCBase + 0x008))
#define SDRCWBTimeout   *((volatile unsigned *)(SDRCBase + 0x00C))

#define SDRAM_MEM_BASE              (0x20000000)

/**********************************************************************
 * SDRAMC Memory Bank Address Space Bases
 *********************************************************************/

#define SDRAM_BANK0_BASE        (SDRAM_MEM_BASE + 0x00000000)
#define SDRAM_BANK1_BASE        (SDRAM_MEM_BASE + 0x08000000)

/**********************************************************************/
/* _BIT(n) sets the bit at position "n"
 * _BIT(n) is intended to be used in "OR" and "AND" expressions:
 * e.g., "(_BIT(3) | _BIT(7))".
 */

#define _BIT(n) (((unsigned int)(1)) << (n))

/* _SBF(f,v) sets the bit field starting at position "f" to value "v".
 * _SBF(f,v) is intended to be used in "OR" and "AND" expressions:
 * e.g., "((_SBF(5,7) | _SBF(12,0xF)) & 0xFFFF)"
 */
#define _SBF(f,v) (((unsigned int)(v)) << (f))

/* _BITMASK constructs a symbol with 'field_width' least significant
 * bits set.
 * e.g., _BITMASK(5) constructs '0x1F', _BITMASK(16) == 0xFFFF
 * The symbol is intended to be used to limit the bit field width
 * thusly:
 * <a_register> = (any_expression) & _BITMASK(x), where 0 < x <= 32.
 * If "any_expression" results in a value that is larger than can be
 * contained in 'x' bits, the bits above 'x - 1' are masked off.  When
 * used with the _SBF example above, the example would be written:
 * a_reg = ((_SBF(5,7) | _SBF(12,0xF)) & _BITMASK(16))
 * This ensures that the value written to a_reg is no wider than
 * 16 bits, and makes the code easier to read and understand.
 */
#define _BITMASK(field_width) ( _BIT(field_width) - 1)

/***********************************************************************
 * SDRAM Controller Configuration Register 0 Bit Field constants
 **********************************************************************/
#define SDRAM_A_AUTO    _SBF(24,1)  /* Auto Pre-charge */
#define SDRAM_A_NOAUTO  _SBF(24,0)  /* No Auto Pre-charge */
#define SDRAM_C_CONT    _SBF(18,1)  /* Clock Enable Continuous */
#define SDRAM_C_IDLE    _SBF(18,0)  /* Clock Enable Idle deasserted */
#define SDRAM_CLAT1     _SBF(20,1)  /* CAS latency 1 */
#define SDRAM_CLAT2     _SBF(20,2)  /* CAS latency 2 */
#define SDRAM_CLAT3     _SBF(20,3)  /* CAS latency 3 */
#define SDRAM_CS0_256M  _BIT(1)     /* nCSOut[0] is 256MBit device */
#define SDRAM_CS0_2BANK _SBF(3,0)   /* nCSOut[0] is 2 bank device*/
#define SDRAM_CS0_4BANK _SBF(3,1)   /* nCSOut[0] is 4 bank device*/
#define SDRAM_CS0_X16   _SBF(2,0)   /* nCSOut[0] is x16 device*/
#define SDRAM_CS0_X32   _SBF(2,0)   /* nCSOut[0] is x32 device*/
#define SDRAM_CS0_X8    _SBF(2,1)   /* nCSOut[0] is x8 device*/
#define SDRAM_CS1_256M  _BIT(5)     /* nCSOut[1] is 256MBit device */
#define SDRAM_CS1_2BANK _SBF(7,0)   /* nCSOut[1] is 2 bank device*/
#define SDRAM_CS1_4BANK _SBF(7,1)   /* nCSOut[1] is 4 bank device*/
#define SDRAM_CS1_X16   _SBF(6,0)   /* nCSOut[1] is x16 device*/
#define SDRAM_CS1_X32   _SBF(6,0)   /* nCSOut[1] is x32 device*/
#define SDRAM_CS1_X8    _SBF(6,1)   /* nCSOut[1] is x8 device*/
#define SDRAM_CS2_256M  _BIT(9)     /* nCSOut[2] is 256MBit device */
#define SDRAM_CS2_2BANK _SBF(11,0)  /* nCSOut[2] is 2 bank device*/
#define SDRAM_CS2_4BANK _SBF(11,1)  /* nCSOut[2] is 4 bank device*/
#define SDRAM_CS2_X16   _SBF(10,0)  /* nCSOut[2] is x16 device*/
#define SDRAM_CS2_X32   _SBF(10,0)  /* nCSOut[2] is x32 device*/
#define SDRAM_CS2_X8    _SBF(10,1)  /* nCSOut[2] is x8 device*/
#define SDRAM_CS3_256M  _BIT(13)    /* nCSOut[3] is 256MBit device */
#define SDRAM_CS3_2BANK _SBF(15,0)  /* nCSOut[3] is 2 bank device*/
#define SDRAM_CS3_4BANK _SBF(15,1)  /* nCSOut[3] is 4 bank device*/
#define SDRAM_CS3_X16   _SBF(14,0)  /* nCSOut[3] is x16 device*/
#define SDRAM_CS3_X32   _SBF(14,0)  /* nCSOut[3] is x32 device*/
#define SDRAM_CS3_X8    _SBF(14,1)  /* nCSOut[3] is x8 device*/
#define SDRAM_E_CONT    _SBF(17,0)  /* Clockout Continuous */
#define SDRAM_E_IDLE    _SBF(17,1)  /* Clockout stops on Idle */
#define SDRAM_RCLAT1    _SBF(22,1)  /* RAS to CAS latency 1 */
#define SDRAM_RCLAT2    _SBF(22,2)  /* RAS to CAS latency 2 */
#define SDRAM_RCLAT3    _SBF(22,3)  /* RAS to CAS latency 3 */
#define SDRAM_EXTBUS16  _SBF(19,1)  /* External Bus Width 16 */
#define SDRAM_EXTBUS32  _SBF(19,0)  /* External Bus Width 32 */

/***********************************************************************
 * SDRAM Controller Configuration Register 1 Bit Field constants
 **********************************************************************/
#define SDRAM_INIT_NORMAL   _SBF(0,0)   /* Normal Operation */
#define SDRAM_INIT_PALL     _SBF(0,1)   /* Init Control PALL */
#define SDRAM_INIT_MODE     _SBF(0,2)   /* Init Control MODE */
#define SDRAM_INIT_NOP      _SBF(0,3)   /* Init Control NOP */
#define SDRAM_STATUS        _BIT(5)     /* SDRAM Engine Status */
#define SDRAM_WB_DISABLE    _SBF(3,0)   /* Write Buffer Disable */
#define SDRAM_WB_ENABLE     _SBF(3,1)   /* Write Buffer Enable */
#define SDRAM_RB_DISABLE    _SBF(2,0)   /* Read Buffer Disable */
#define SDRAM_RB_ENABLE     _SBF(2,1)   /* Read Buffer Enable */

/***********************************************************************
 * SDRAM Controller Refresh Timer Register Bit Field
 **********************************************************************/
/* Set Refresh Timer */
#define SDRAM_SET_REFRESH(n)    ((n)&_BITMASK(16))

/***********************************************************************
 * SDRAM Controller Write Buffer Timeout Register Bit Field
 **********************************************************************/
/* Set Write Buffer Timeout */
#define SDRAM_SET_WBTIMEOUT(n)  ((n)&_BITMASK(16))

/***********************************************************************
 * Refresh timer values for various Clock Indexes
 **********************************************************************/

#define REFTIMER_78     0x480
#define REFTIMER_52     0x320
#define REFTIMER_39     0x270
#define REFTIMER_10     0x80



//-------------------------------------------------------------------------------
// Reset Clock Generation and Power Control (RCPC) Control - Enable Write
//-------------------------------------------------------------------------------
// RES       0000 0000 0000 0000 0000 00-- ---- ---- Reserved
// WRTLOCK   ---- ---- ---- ---- ---- --1- ---- ---- Write Lock Enable
// RES       ---- ---- ---- ---- ---- ---0 ---- ---- Reserved
// CLKSEL    ---- ---- ---- ---- ---- ---- 1--- ---- 0 = HCLK is derived from the 309.6576 MHz output of the PLL, 1 = HCLK is derived from the signal at the CLKIN input pin
// OUTSEL    ---- ---- ---- ---- ---- ---- -11- ---- CLKOUT Source Select, 0b00 = 14.7456 MHz signal from the internal oscillator, 0b01 = 309.6576 MHz signal from the PLL, 0b10 = FCLK, 0b11 = HCLK
// PWRDWNSEL ---- ---- ---- ---- ---- ---- ---0 00-- Power Down Mode Select, 0b000 = Active Mode, 0b001 = Standby Mode, 0b010 = Sleep Mode, 0b011 = Stop1 Mode, 0b100 = Stop2 Mode
// EX*       ---- ---- ---- ---- ---- ---- ---- --1- 0 = Disable the 14.7456 MHz internal crystal oscillator, 1 = Enable the 14.7456 MHz internal crystal oscillator (writable only if the CLKSEL bit field (bit 7 of this register) is ‘1’.)
// EP*       ---- ---- ---- ---- ---- ---- ---- ---1 0 = Disable PLL, 1 = Enable PLL (writable only if the CLKSEL bit field (bit 7 of this register) is ‘1’.)
//-------------------------------------------------------------------------------
//           0000 0000 0000 0000 0000 0010 1110 0011
//           0    0    0    0    0    2    E    3
#define RCPCCtrl_EnableWrite 0x000002E3
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// SMC Bank Access Parameters for fast BRAM -  3 cycle write, 3 cycle read
//-------------------------------------------------------------------------------
// RES    00-- ---- ---- ---- ---- ---- ---- ---- Reserved
// MW     --10 ---- ---- ---- ---- ---- ---- ---- External Static Memory Width    0b00 = 8-bit, 0b01 = 16-bit, 0b10 = 32-bit, 0b11 = Reserved — do not write
// BM     ---- 0--- ---- ---- ---- ---- ---- ---- Burst Mode
// WP     ---- -0-- ---- ---- ---- ---- ---- ---- Write Protect
// WPERR  ---- --0- ---- ---- ---- ---- ---- ---- Write Protect Error Status Flag
// BUSERR ---- ---0 ---- ---- ---- ---- ---- ---- Bus Transfer Error Status Flag
// RES    ---- ---- 0000 0000 ---- ---- ---- ---- Reserved
// WST2   ---- ---- ---- ---- 0010 0--- ---- ---- Wait State2 (IMPORTANT: at least 4 to enable the nWait signal to be recognised by the processor) This is the write access time for SRAM, the burst access time for burst ROM, and does not apply to ROM devices. This wait state time is: (WST2 + 1) × tHCLK in the case of SRAM,or (WST2) × tHCLK in the case of burst ROM.
// RBLE   ---- ---- ---- ---- ---- -1-- ---- ---- Read Byte Lane Enable
// WST1   ---- ---- ---- ---- ---- --00 010- ---- Wait State1 (IMPORTANT: at least 2 to enable the nWait signal to be recognised by the processor) This is the read access time for SRAM and ROM, or the initial access time for burst ROM. This wait state time is (WST1 + 1) × tHCLK.
// RES    ---- ---- ---- ---- ---- ---- ---0 ---- Reserved
// IDCY   ---- ---- ---- ---- ---- ---- ---- 0000 Idle cycle memory data bus turn-around time. The turn around time is (IDCY + 1) × (the Period of the AHB clock).
//-------------------------------------------------------------------------------
//        0010 0000 0000 0000 0010 0100 0100 0000
//        2    0    0    0    2    4    4    0
#define SMC_WB32Bit  0x20002440
#define SMC_WB16Bit  0x10002440
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// SMC Bank Access Parameters for fast 32bit BRAM - 2 cycle read, 2 cycle write
//-------------------------------------------------------------------------------
// RES    00-- ---- ---- ---- ---- ---- ---- ---- Reserved
// MW     --10 ---- ---- ---- ---- ---- ---- ---- External Static Memory Width    0b00 = 8-bit, 0b01 = 16-bit, 0b10 = 32-bit, 0b11 = Reserved — do not write
// BM     ---- 0--- ---- ---- ---- ---- ---- ---- Burst Mode
// WP     ---- -0-- ---- ---- ---- ---- ---- ---- Write Protect
// WPERR  ---- --0- ---- ---- ---- ---- ---- ---- Write Protect Error Status Flag
// BUSERR ---- ---0 ---- ---- ---- ---- ---- ---- Bus Transfer Error Status Flag
// RES    ---- ---- 0000 0000 ---- ---- ---- ---- Reserved
// WST2   ---- ---- ---- ---- 0000 0--- ---- ---- Wait State2  This is the write access time for SRAM, the burst access time for burst ROM, and does not apply to ROM devices. This wait state time is: (WST2 + 1) × tHCLK in the case of SRAM,or (WST2) × tHCLK in the case of burst ROM.
// RBLE   ---- ---- ---- ---- ---- -1-- ---- ---- Read Byte Lane Enable
// WST1   ---- ---- ---- ---- ---- --00 000- ---- Wait State1  This is the read access time for SRAM and ROM, or the initial access time for burst ROM. This wait state time is (WST1 + 1) × tHCLK.
// RES    ---- ---- ---- ---- ---- ---- ---0 ---- Reserved
// IDCY   ---- ---- ---- ---- ---- ---- ---- 0000 Idle cycle memory data bus turn-around time. The turn around time is (IDCY + 1) × (the Period of the AHB clock).
//-------------------------------------------------------------------------------
//        0010 0000 0000 0000 0000 0100 0000 0000
//        2    0    0    0    0    4    0    0
#define SMC_Fast32Bit  0x20000400
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// SMC Bank Access Parameters for fast 16bit BRAM - 2 cycle read, 2 cycle write
//-------------------------------------------------------------------------------
// RES    00-- ---- ---- ---- ---- ---- ---- ---- Reserved
// MW     --01 ---- ---- ---- ---- ---- ---- ---- External Static Memory Width    0b00 = 8-bit, 0b01 = 16-bit, 0b10 = 32-bit, 0b11 = Reserved — do not write
// BM     ---- 0--- ---- ---- ---- ---- ---- ---- Burst Mode
// WP     ---- -0-- ---- ---- ---- ---- ---- ---- Write Protect
// WPERR  ---- --0- ---- ---- ---- ---- ---- ---- Write Protect Error Status Flag
// BUSERR ---- ---0 ---- ---- ---- ---- ---- ---- Bus Transfer Error Status Flag
// RES    ---- ---- 0000 0000 ---- ---- ---- ---- Reserved
// WST2   ---- ---- ---- ---- 0000 0--- ---- ---- Wait State2  This is the write access time for SRAM, the burst access time for burst ROM, and does not apply to ROM devices. This wait state time is: (WST2 + 1) × tHCLK in the case of SRAM,or (WST2) × tHCLK in the case of burst ROM.
// RBLE   ---- ---- ---- ---- ---- -1-- ---- ---- Read Byte Lane Enable
// WST1   ---- ---- ---- ---- ---- --00 000- ---- Wait State1  This is the read access time for SRAM and ROM, or the initial access time for burst ROM. This wait state time is (WST1 + 1) × tHCLK.
// RES    ---- ---- ---- ---- ---- ---- ---0 ---- Reserved
// IDCY   ---- ---- ---- ---- ---- ---- ---- 0000 Idle cycle memory data bus turn-around time. The turn around time is (IDCY + 1) × (the Period of the AHB clock).
//-------------------------------------------------------------------------------
//        0001 0000 0000 0000 0000 0100 0000 0000
//        1    0    0    0    0    4    0    0
#define SMC_Fast16Bit  0x10000400
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// HCLK Prescale Register for 50 MHz
//-------------------------------------------------------------------------------
// RES     0000 0000 0000 0000 0000 0000 0000 ---- Reserved
// HCPSVAL ---- ---- ---- ---- ---- ---- ---- 0001 Divider      0b0011 = Diverder Value = 6, 0b0010 = Diverder Value = 4, 0b0001 = Divider value = 2
//-------------------------------------------------------------------------------
//         0000 0000 0000 0000 0000 0000 0000 0001
//         0    0    0    0    0    0    0    1
#define HCLKPrescale_50Mhz 0x00000001
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// Memory Interface Multiplexing Register, configured to enable nCS3,4,5,6 and nBLE2,3
//-------------------------------------------------------------------------------
// RES            0000 0000 0000 0000 00-- ---- ---- ---- Reserved
// PIN34          ---- ---- ---- ---- --1- ---- ---- ---- nBLE3
// PIN35          ---- ---- ---- ---- ---1 ---- ---- ---- nBLE2
// PIN41          ---- ---- ---- ---- ---- 1--- ---- ---- nCS6
// PIN42          ---- ---- ---- ---- ---- -1-- ---- ---- nCS5
// PIN43          ---- ---- ---- ---- ---- --1- ---- ---- nCS4
// PIN44          ---- ---- ---- ---- ---- ---1 ---- ---- nCS3
// UPPER16        ---- ---- ---- ---- ---- ---- 1--- ---- Enable Upper 16 Data Bus bits
// PIN101         ---- ---- ---- ---- ---- ---- -0-- ---- 1 = SDCLK 0 = PF0 (reset)
// PIN102         ---- ---- ---- ---- ---- ---- --0- ---- 1 = SDCKE 0 = PE7 (reset)
// PIN104         ---- ---- ---- ---- ---- ---- ---0 ---- 1 = nDCS1 0 = PE6 (reset)
// PIN105         ---- ---- ---- ---- ---- ---- ---- 0--- 1 = nDCS0 0 = PE5 (reset)
// PIN106         ---- ---- ---- ---- ---- ---- ---- -0-- 1 = nSDWE 0 = PE4 (reset)
// PIN112:PIN109  ---- ---- ---- ---- ---- ---- ---- --00 Configure pins 112 through 109.
//-------------------------------------------------------------------------------
//                0000 0000 0000 0000 0011 1111 1000 0000
//                0    0    0    0    3    F    8    0
//#define MemMux_EnableAllChipSelect 0x00003F80
#define MemMux_EnableAllChipSelect 0x00003FFF
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// Miscellaneous Pin Multiplexing Register, Enable nWait, CLKOUT and INT0 - INT4
//-------------------------------------------------------------------------------
// RES            0000 0000 0000 0000 0000 0--- ---- ---- Reserved
// PIN98          ---- ---- ---- ---- ---- -0-- ---- ---- 0 = CLKIN (reset)
// PIN99          ---- ---- ---- ---- ---- --0- ---- ---- 0 = PF1(reset)
// PIN144         ---- ---- ---- ---- ---- ---1 ---- ---- 1 = nWAIT
// PIN145         ---- ---- ---- ---- ---- ---- 0--- ---- 0 = CTOUT1B (reset)
// PIN150         ---- ---- ---- ---- ---- ---- -0-- ---- 0 = INT4 (reset)
// PIN151         ---- ---- ---- ---- ---- ---- --0- ---- 0 = INT3 (reset)
// PIN152         ---- ---- ---- ---- ---- ---- ---1 ---- 1 = INT2
// PIN153         ---- ---- ---- ---- ---- ---- ---- 1--- 1 = INT1
// PIN155         ---- ---- ---- ---- ---- ---- ---- -1-- 1 = INT0
// PIN156         ---- ---- ---- ---- ---- ---- ---- --1- 1 = CLKOUT
// PIN157         ---- ---- ---- ---- ---- ---- ---- ---0 0 = PWM1 (reset)
//-------------------------------------------------------------------------------
//                0000 0000 0000 0000 0000 0001 0001 1110
//                0    0    0    0    0    1    1    E
#define MiscMux_EnableAllIntAndnWait 0x0000011E
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// Core Clock Configuration Register
//-------------------------------------------------------------------------------
// RES            0000 0000 0000 0000 0000 0000 0000 00-- Reserved
// CFGVAL         ---- ---- ---- ---- ---- ---- ---- --11 0b00 = Standard Mode, asynchronous operation (the value at reset), 0b10 = Standard Mode, synchronous operation, 0b01 = FastBus Extension Mode, 0b11 = FastBus Extension Mode
//-------------------------------------------------------------------------------
//                0000 0000 0000 0000 0000 0000 0000 0011
//                0    0    0    0    0    0    0    3
#define CoreClkConfig_FastBus 0x3
//-------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// AHB Clock Control Register
//-------------------------------------------------------------------------------
// RES            0000 0000 0000 0000 0000 0000 0000 01-- Reserved
// SDC            ---- ---- ---- ---- ---- ---- ---- --0- 1 = Disable the HCLK signal fed to the SDRC (reset), 0 = Enable the HCLK signal fed to the SDRC
// DMA            ---- ---- ---- ---- ---- ---- ---- ---1 1 = Disable the HCLK signal fed to the DMAC (reset), 0 = Enable the HCLK signal fed to the DMAC
//-------------------------------------------------------------------------------
//                0000 0000 0000 0000 0000 0000 0000 0101
//                0    0    0    0    0    0    0    5
#define AHBClkCtrl_Configuration 0x5
//-------------------------------------------------------------------------------

static void init_sdram ()
{
   volatile unsigned int tmp;

    /* New pseudo-code for Micron MC48LC8M16A2TG-8EL SDRAM's
     * Issue two NOP commands
     * Delay at least 100 usecs.
     * Issue precharge to all banks
     * Once in idle state, issue two auto refresh cycles to each bank
     * Program the Mode register when all banks are idle
     * Wait the "specified time" before initiating subsequent operation
     * */

    /*      DELAY   to allow power and clocks to stabilize */
    /* load ~100us value to timer1 */
   timing_get_ticks_us (200);

    SDRCConfig1 = SDRAM_INIT_NOP;
    SDRCConfig1 = SDRAM_INIT_NOP;

    /*      DELAY   to allow sdram clocks to settle */
    /* load ~200us value to timer1 */
   timing_get_ticks_us (200);

    /* issue a "pre-charge all" command */
    SDRCConfig1 = SDRAM_INIT_PALL;

    /* load ~250us value to timer1 */
   timing_get_ticks_us (250);

    /* refresh every 16 clock cycles */
   //SDRCRefTimer = SDRAM_SET_REFRESH(16);
   SDRCRefTimer = SDRAM_SET_REFRESH(16);
    /*      DELAY   for at least two auto refresh cycles */
    /* load ~250us value to timer1 */
   timing_get_ticks_us (250);

    /* set the refresh timer */
    /* Assumes 52 MHz default clock */
    SDRCRefTimer = SDRAM_SET_REFRESH(REFTIMER_52);

    /* load ~250us value to timer1 */
   timing_get_ticks_us (250);

    /* Program the SDRAM internal mode registers on bank nSDCS0-1
     * Burst Length - 4  (A2:A0 = 0b010)
     * Burst Type - Sequential (A3 = 0)
     * CAS Latency - 3 (A6:A4 = 0x011)
     * Operating Mode - Standard (A8:A7 = 0)
     * Write Burst Mode - Programmed Burst Length (A9 = 0)
     */

    /* Select mode register update mode */
    SDRCConfig1 = SDRAM_INIT_MODE;

   tmp = *((volatile int *)(SDRAM_BANK0_BASE | (0x32 << 12)));
   tmp = *((volatile int *)(SDRAM_BANK1_BASE | (0x32 << 12)));

    /* Wait until idle */
   while (SDRCConfig1 & SDRAM_STATUS);

    /*  Configure SDRAM Controller Configuration Register 0 */
    unsigned int cfg_value = 0;
    cfg_value =         SDRAM_A_AUTO |
                        SDRAM_C_IDLE |
                        SDRAM_E_IDLE |
                        SDRAM_CLAT3 |
                        SDRAM_CS0_4BANK |
                        SDRAM_CS1_4BANK |
                        SDRAM_CS0_X32 |
                        SDRAM_CS1_X32 |
                        SDRAM_RCLAT3 |
                        SDRAM_CS0_256M |
                        SDRAM_CS1_256M |
                        SDRAM_EXTBUS32;

    SDRCConfig0 |=  cfg_value;
    /* Wait until idle */
   while (SDRCConfig1 & SDRAM_STATUS);

    /* select normal operating mode */
   SDRCConfig1 = SDRAM_INIT_NORMAL;

    /* Wait until idle */
   while (SDRCConfig1 & SDRAM_STATUS);

    /* Wait ~100 us before using */
   timing_get_ticks_us (128);
}

void startup(void)
{
    *((unsigned int*) RCPCCtrl)     = RCPCCtrl_EnableWrite;
    *((unsigned int*) MemMux)       = MemMux_EnableAllChipSelect;
    *((unsigned int*) MiscMux)      = MiscMux_EnableAllIntAndnWait;

    *((unsigned int*) SMCBCR0)      = SMC_Fast16Bit; // Wish bone wrapper internal mem
    *((unsigned int*) SMCBCR1)      = SMC_WB32Bit;
    *((unsigned int*) SMCBCR2)      = SMC_WB32Bit;
    *((unsigned int*) SMCBCR3)      = SMC_WB32Bit;
    *((unsigned int*) SMCBCR4)      = SMC_WB32Bit;
    *((unsigned int*) SMCBCR5)      = SMC_WB32Bit;
    *((unsigned int*) SMCBCR6)      = SMC_WB16Bit;

    *((unsigned int*) HCLKPrescale)   = HCLKPrescale_50Mhz;
    *((unsigned int*) CpuClkPrescale) = HCLKPrescale_50Mhz;
    *((unsigned int*) AHBClkCtrl)     = AHBClkCtrl_Configuration;
    *((unsigned int*) CoreClkConfig)  = CoreClkConfig_FastBus;

    init_clock();
    init_sdram();
}




