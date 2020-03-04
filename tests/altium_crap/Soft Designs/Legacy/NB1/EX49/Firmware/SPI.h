//-------------------------------------------------------------
//     ____   ____  ___
//    / ___| |  _ \|_ _|
//    \___ \ | |_) || |
//     ___) ||  __/ | |
//    |____/ |_|   |___|
//
//-------------------------------------------------------------------------

#ifndef __NBT_SPI_H__
#define __NBT_SPI_H__

//--------------------------------------------------
// SPI device nubers for SPI_open function
//--------------------------------------------------
#define DEVICE_FLASH_CONFIGURATION 1
#define DEVICE_FLASH_EMBEDDED 2
#define DEVICE_TESTER 3
#define DEVICE_CLOCK  4
#define DEVICE_AUDIO_CODEC 0xAA                            // special case: on-board audio codec via FPGA SPI multiplexer

// -------------------------------------------------
//    __  __  ____   ____   ____   _  _     ___
//   |  \/  ||___ \ | ___| |  _ \ | || |   / _ \
//   | |\/| |  __) ||___ \ | |_) || || |_ | | | |
//   | |  | | / __/  ___) ||  __/ |__   _|| |_| |
//   |_|  |_||_____||____/ |_|       |_|   \___/
//
// -------------------------------------------------
//   M25P40 4MB Flash Memory constants
//---------------------------------------------------

//Instructions
#define M25P40_WREN      0x06                              // Write Enable Instruction
#define M25P40_WRDI      0x04                              // Write Disable Instruction
#define M25P40_RDSR      0x05                              // Read Status Register Instruction
#define M25P40_WDSR      0x01                              // Write Status Register Instruction
#define M25P40_READ      0x03                              // Read Data Bytes Instruction
#define M25P40_FAST_READ 0x0B                              // Read Data Bytes at Higher Speed Instruction
#define M25P40_PP        0x02                              // Page Program Instruction
#define M25P40_SE        0xD8                              // Sector Erase Instruction
#define M25P40_BE        0xC7                              // Bulk Erase Instruction
#define M25P40_DP        0xB9                              // Deep Powerdown
#define M25P40_RES       0xAB                              // Release from Deep Powerdown/Read Electronic Signature

//       Status Register Format 
//
//           b7                                   B0
//       +------+---+---+-----+-----+-----+-----+-----+
//       | SRWD | 0 | 0 | BP2 | BP1 | BP0 | WEL | WIP |
//       +---+--+---+---+--+--+--+--+--+--+--+--+--+--+
//           |             |     |     |     |     |
//        Status Register  +-----+-----+     |     |
//        Write Protect          |           |     |
//                       Block Protect Bits  |     |
//                                           |     |
//                                  Write Enable   |
//                                  Latch Bit      |
//                                                 |
//                                            Write In
//                                            Progress Bit
//

#define M25P40_SRWD   0x80                                 // Mask for Status Register Write Protect
#define M25P40_BP0    0x04                                 // Mask for Block Protect Bit 0
#define M25P40_BP1    0x08                                 // Mask for Block Protect Bit 1
#define M25P40_BP2    0x10                                 // Mask for Block Protect Bit 2
#define M25P40_WEL    0x02                                 // Mask for Write Enable Latch Bit
#define M25P40_WIP    0x01                                 // Mask for Write in Progress Bit

// Masks for Status Register Block Protection Bits BP[0..2]
#define M25P40_PROTECT_NONE           0x00                 // Mask for no write protection
#define M25P40_PROTECT_7              0x04                 // Mask for write protection sector 7 only
#define M25P40_PROTECT_6_7            0x08                 // Mask for write protection sector 6 and 7
#define M25P40_PROTECT_4_7            0x0C                 // Mask for write protection sector 4 to 7
#define M25P40_PROTECT_ALL            0x10                 // Mask for write protection all sectors

// RES Response
#define M25P40_RES_RESPONSE     0x12                       // expected response to RES command

// -------------------------------------------------
//    ___ ____ ____ _____  ___ _____    ____
//   |_ _/ ___/ ___|___ / / _ \___  |  |___ \
//    | | |   \___ \ |_ \| | | | / /____ __) |
//    | | |___ ___) |__) | |_| |/ /_____/ __/
//   |___\____|____/____/ \___//_/     |_____|
//
//   ICS302-2 Serially Programmable Clock Source
// -------------------------------------------------

// Bit Masks for CLK1 output divider Setting  S[0..2]
#define ICS307_OD_10   0x00                                // BitMask for CLK1 Output Divide by 10
#define ICS307_OD_2    0x01                                // BitMask for CLK1 Output Divide by 2
#define ICS307_OD_8    0x02                                // BitMask for CLK1 Output Divide by 8
#define ICS307_OD_4    0x03                                // BitMask for CLK1 Output Divide by 4 (default)
#define ICS307_OD_5    0x04                                // BitMask for CLK1 Output Divide by 5
#define ICS307_OD_7    0x05                                // BitMask for CLK1 Output Divide by 7
#define ICS307_OD_3    0x06                                // BitMask for CLK1 Output Divide by 3
#define ICS307_OD_6    0x07                                // BitMask for CLK1 Output Divide by 6

// Bit Masks for Clock 2 Output MUX bits (F[0..1])
#define ICS307_CLK2_REF        0x00                        // Clk2 output = REF    (default)
#define ICS307_CLK2_REF_DIV2   0x08                        // Clk2 output = REF/2
#define ICS307_CLK2_OFF        0x10                        // Clk2 output = 0
#define ICS307_CLK2_CLK1_DIV2  0x18                        // Clk2 output = CLK1/2

// Bit Masks for TTL Bit (Output Duty Cycle Configuration)
#define ICS307_TTL_5V          0x00                        // optimised Dyty Cycle for VDD = 5V
#define ICS307_TTL_3V3         0x20                        // optimised Dyty Cycle for VDD = 3.3V (default)

// Bit Mask for internal Crystal Load Capacitance
#define ICS307_XTAL_LOAD_0     0x00                        // Lowest Range (default)
#define ICS307_XTAL_LOAD_1     0x40                        // ...
#define ICS307_XTAL_LOAD_2     0x80                        // ...
#define ICS307_XTAL_LOAD_3     0xC0                        // Highest Range

#define ICS307_Default_Config  (ICS307_XTAL_LOAD_0 | ICS307_TTL_3V3 | ICS307_CLK2_REF | ICS307_OD_4)
#define ICS307_Default_VCO_DIV 8                           // default VCO divider
#define ICS307_Default_REF_DIV 6                           // default Reference divider

#define ICS307_100MHZ 0x230B01L                            // command word for 100MHz
#define ICS307_90MHZ  0x230981L                            // command word for  90MHz
#define ICS307_80MHZ  0x210201L                            // command word for  80MHz
#define ICS307_70MHZ  0x230681L                            // command word for  70MHz
#define ICS307_60MHZ  0x230501L                            // command word for  60MHz
#define ICS307_50MHZ  0x220B01L                            // command word for  50MHz
#define ICS307_40MHZ  0x220801L                            // command word for  40MHz
#define ICS307_30MHZ  0x220501L                            // command word for  30MHz
#define ICS307_25MHZ  0x220381L                            // command word for  25MHz
#define ICS307_20MHZ  0x230406L                            // command word for  20MHz

//------------------------------------------------------------------------------
// Programs the ICS307 to generate a new output frequency
// Output Frequency for CLK1 = Fref * 2 * (VDW + 8) / ((RDW + 2) * OD)
// Parameters:
//   Config: Configuration word :
//
//         MSB                                 LSB
//        +----+----+-----+----+----+----+----+----+
//        | C1   C0 | TTL | F1   F0 | S2   S1   S0 |
//        +----+----+--+--+--+-+-+--+-+--+----+--+-+
//           |   |     |     |   |    |          |
//           +-+-+     |     +-+-+    +-----+----+
//             |       |       |            |
//        Crystal    Output  CLK2      Output Divide
//        Load       Duty    Output
//        Impedance  Cycle   Select
//
//  VDW    : VCO Divider Word  (9 significant bits)
//  RDW    : Reference Divider Word  (7 significant bits)
//------------------------------------------------------------------------------
void ICS307_Program(unsigned char Config, unsigned int VDW, unsigned char RDW);

//-------------------------------------------------------------
// Programs command word 'data' into the clock generator chip
//-------------------------------------------------------------
void ICS307_ProgramW(unsigned long data);

//-----------------------------------------------------------------------------------
//   __  __     _    __  __ _  _   ___   _  _       ____  ___   ____   _____  ____
//  |  \/  |   / \   \ \/ // |/ | / _ \ | || |     / ___|/ _ \ |  _ \ | ____|/ ___|
//  | |\/| |  / _ \   \  / | || || | | || || |_   | |   | | | || | | ||  _| | |
//  | |  | | / ___ \  /  \ | || || |_| ||__   _|  | |___| |_| || |_| || |___| |___
//  |_|  |_|/_/   \_\/_/\_\|_||_| \___/    |_|     \____|\___/ |____/ |_____|\____|
//
//-----------------------------------------------------------------------------------

//   Control Byte Format
//   +------+-------+-------------------------------------------------------------+
//   | BIT# | NAME  |  DESCRIPTION                                                |
//   +------+-------+-------------------------------------------------------------+
//   |7(MSB)| START |  1=New Control Word,                                        |
//   |      |       |  0=Control word ignored                                     |
//   +------+-------+-------------------------------------------------------------+
//   |  6   |  A1   |  1=DAC addressed Current byte configures DAC, then DAC Data |
//   |      |       |  0=DAC not addressed                                        |
//   +------+-------+-------------------------------------------------------------+
//   |  5   |  A0   |  1=ADC Current byte configuration, next byte returns Data   |
//   |      |       |  0=ADC not addressed                                        |
//   +------+-------+-------------------------------------------------------------+
//   |  4   |  C1   |  1=ADC input to Vdd/2   (not recommended for MAX1104)       |
//   |      |       |  0=ADC input to AIN                                         |
//   +------+-------+-------------------------------------------------------------+
//   |  3   |  C0   |  1=Continuous conversion until reconfigured                 |
//   |      |       |  0=Single conversion, controlword reqd. for next conversion |
//   +------+-------+-------------------------------------------------------------+
//   |  2   |  E2   |  1=reference enabled    (don't care for MAX1104)            |
//   |      |       |  0=reference disabled                                       |
//   +------+-------+-------------------------------------------------------------+
//   |  1   |  E1   |  1=ADC enabled                                              |
//   |      |       |  0=ADC disabled                                             |
//   +------+-------+-------------------------------------------------------------+
//   |  0   |  E0   |  1=DAC enabled                                              |
//   |      |       |  0=DAC disabled                                             |
//   +------+-------+-------------------------------------------------------------+

#define MAX1104_START 0x80                                 // Start bit (always set for command mode)
#define MAX1104_A1    0x40
#define MAX1104_A0    0x20
#define MAX1104_C1    0x10
#define MAX1104_C0    0x08
#define MAX1104_E2    0x04
#define MAX1104_E1    0x02
#define MAX1104_E0    0x01


//--------------------------------------------------------------------------------
//  This writes to the SPI bus multiplexer on the Nanobord's FPGA and selects the
//  SPI device.
//--------------------------------------------------------------------------------
void SPI_open(unsigned char device);

//--------------------------------------------------------------------------------
// Close SPI channel and clear the address register
//--------------------------------------------------------------------------------
void SPI_close(void);

//----------------------------------------------------------------------------------//
// Open a flash file for reading starting at 'address'
// subsequent SPI_sendReceiveByte commands will autoincrement the address
// leaves chip enable active
//----------------------------------------------------------------------------------//
void Flash_openRead(unsigned long address);

//--------------------------------------------------------------------------------
//  Send and receive a byte to the SPI device
//  'in' is the byte to write (dummy byte in case of a read)
//  returns: Byte read
//--------------------------------------------------------------------------------
unsigned char SPI_sendReceiveByte(unsigned char in);

//-------------------------------------------------------------------------------
// Test data is exchanged with the Nanoboard controller by sending and receiving
// a 32 bit (4 byte) command to SPI address 3.
// Currently only the first and last bytes are used to exchange data.
// The last byte sent contains the bits to set the LEDs and the last byte received
// contains the state of the jumpers. ('1' = open '0' = inserted)
// The first byte sent (0xc0) places the Nanoboard controller into test mode.
// If the first byte is not 0xC0 (eg. 0x00) the Nanoboard controller reverts back
// to non-test mode
//-------------------------------------------------------------------------------
unsigned char Nanoboard_TestModeOn(unsigned char leds);

//-----------------------------------------------------------------------------------
// turns NanoBoard Test Mode off, LEDs and Jumpers revert to their normal functions
//-----------------------------------------------------------------------------------
void Nanoboard_TestModeOff(void);

//----------------------------------------------------------------------------------//
// read the electronic signature to wake the device
// also takes chip out of deep powerdown
//----------------------------------------------------------------------------------//
unsigned char Flash_readElectronicSignature(void);
          
#endif // __NBT_SPI_H__

