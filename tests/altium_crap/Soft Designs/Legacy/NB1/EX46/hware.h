//       __  __ _       __ ___     ____   ______    __  __
//      / / / /| |     / //   |   / __ \ / ____/   / / / /
//     / /_/ / | | /| / // /| |  / /_/ // __/     / /_/ /
//    / __  /  | |/ |/ // ___ | / _, _// /___ _  / __  /
//   /_/ /_/   |__/|__//_/  |_|/_/ |_|/_____/(_)/_/ /_/
// 
// (c) 2003 Altium
// Started: 06.11.2003 Ch.W.
// Defines hardware constants for project

#ifndef __HWARE_H__
#define __HWARE_H__ 

#include <regtsk51a.sfr>

// ------------------------------------------------------------------------------------------
//   Oscillator Frequency
// ------------------------------------------------------------------------------------------

#define FOSC 30000000L  // Oscillator frequency

// ------------------------------------------------------------------------------------------
//   Interrupt Vectors
// ------------------------------------------------------------------------------------------

#define __INTNO(nr) ((8*nr)+3)

#define INTVEC_EXT0 (0x03)  // interrupt number 0, vector address 0x03
#define INTVEC_T0   (0x0B)
#define INTVEC_EXT1 (0x13)
#define INTVEC_T1   (0x1B)
#define INTVEC_SIO  (0x23)

// ------------------------------------------------------------------------------------------
//   SFR Port expansions
// ------------------------------------------------------------------------------------------

#define SFR_PORT0 (*(__bsfr volatile unsigned char *) 0x86 )   // I/O Port on SFR bus @ 0x86
#define SFR_PORT1 (*(__bsfr volatile unsigned char *) 0x8E )   // I/O Port on SFR bus @ 0x8E
#define SFR_PORT2 (*(__bsfr volatile unsigned char *) 0x96 )   // I/O Port on SFR bus @ 0x96
#define SFR_PORT3 (*(__bsfr volatile unsigned char *) 0x9E )   // I/O Port on SFR bus @ 0x9E
#define SFR_PORT4 (*(__bsfr volatile unsigned char *) 0xA6 )   // I/O Port on SFR bus @ 0xA6
#define SFR_PORT5 (*(__bsfr volatile unsigned char *) 0xAE )   // I/O Port on SFR bus @ 0xAE
#define SFR_PORT6 (*(__bsfr volatile unsigned char *) 0xB6 )   // I/O Port on SFR bus @ 0xB6
#define SFR_PORT7 (*(__bsfr volatile unsigned char *) 0xBE )   // I/O Port on SFR bus @ 0xBE
#define SFR_PORT8 (*(__bsfr volatile unsigned char *) 0xC6 )   // I/O Port on SFR bus @ 0xC6
#define SFR_PORT9 (*(__bsfr volatile unsigned char *) 0xCE )   // I/O Port on SFR bus @ 0xCE

#define KEY_PORT      SFR_PORT0        // port for Keyboard
#define LED_PORT      SFR_PORT1        // output port for LEDs
#define DIP_PORT      SFR_PORT1        // input port for DIP-Switches
#define U1H0_PORT     SFR_PORT2        // Port for lowest byte  of User I/O port
#define U1H1_PORT     SFR_PORT3        // Port for middle byte  of User I/O Port
#define U1H2_PORT     SFR_PORT4        // Port for top two bits of User I/O Port
#define SPK_PORT      SFR_PORT5        // Port for Speaker
#define FREQ0_PORT    SFR_PORT6        // Port for Frequency Counter LSN
#define FREQ1_PORT    SFR_PORT7        // Port for Frequency Counter MSN
#define FREQMODE_PORT SFR_PORT7        // Mode for Frequency counter
#define MASTERIO_PORT SFR_PORT8        // Port for Master I/O Port Header
#define SLAVEIO_PORT  SFR_PORT8        // Port for Slave I/O Port Header

#define FREQ_MODE_RESET 0     // modes for frequency counter
#define FREQ_MODE_COUNT 1
#define FREQ_MODE_STOP  2
#define FREQ_MODE_AUTO  3

//-------------------------------------------------------------------------------------------
//   Port 0
// ------------------------------------------------------------------------------------------

#define LCD_RAM         P0_0      // 0 = LCD mapped to XDATA , 1 = RAM mapped to XDATA
#define RAM_BANK        P0_1      // A16 of RAM chip
#define RAM_CHIP        P0_2      // 0 = RAM0                , 1 = RAM1
#define PS2_SEL         P0_3      // 0 = select Port A, 1 = select Port B
#define LCD_BACKLIGHT   P0_4      // 0 = Backlight Off       , 1 = Backlight On
#define SPEAKER_ENABLE  P0_5      // 0 = Speaker disabled    , 1 = Speaker enabled
#define SER_SEL0        P0_6      // 00 = RXD -> TXD
#define SER_SEL1        P0_7      // 01 = RTS -> CTS
                                  // 02 = CAN
#define SER_SEL_RXDTXD  0x00      // mask for SER_SEL[0..1]
#define SER_SEL_RTSCTS  0x40
#define SER_SEL_CAN     0x80

//-------------------------------------------------------------------------------------------
//   Port 1
// ------------------------------------------------------------------------------------------

//---------------- I2C Control Lines -------------------------------
#define SCL       P1_3    // pin for SCL
#define SDA       P1_2    // pin for SDA
#define SCL_CTRL  P1_3    // Control Pin for SCL
#define SDA_CTRL  P1_2    // Control Pin for SDA

#define CTRL_HIZ   (1)     // Control definition for high impedance
#define CTRL_OUT   (0)     // for output
#define CTRL_IN    (1)     // for input

//---------------- SPI Control Lines -------------------------------
#define SPI_CLK   P1_7    // SPI Clock Line
#define SPI_DIN   P1_4    // SPI Data In Line
#define SPI_DOUT  P1_4    // SPI Data Out Line
#define SPI_SEL   P1_5    // SPI Select Line
#define SPI_MODE  P1_6    // SPI Mode Line

//---------------- PS2KBD Control Lines ----------------------------
#define PS2_CLK_RD P1_0  // PS2 Port Clock Read Line
#define PS2_DTA_RD P1_1  // PS2 Port Clock Read Line
#define PS2_CLK_WR P1_0  // PS2 Port Clock Write Line
#define PS2_DTA_WR P1_1  // PS2 Port Clock Write Line

//-------------------------------------------------------------------------------------------
//   Port 2
// ------------------------------------------------------------------------------------------

#define SPI_SEL_AUDIO  P2_0  // 0 -> SPI on normal SPI bus, 1 -> SPI on Max1104 Audio Codec


//-------------------------------------------------------------------------------------------
//   XDATA
// ------------------------------------------------------------------------------------------
/*

XDATA is mapped into 8 banks of 64k
Bank Select is controlled via Port0, Bits 0..2
P0.0 : Selects lower (0) or upper (1) half of each 128k RAM chip
P0.1 : Selects RAM chip 0 or 1
P0.2 : Selects LCD (0) or RAM (1)
 
*/

#define BANK_LCD   0x00
#define BANK_RAM0  0x04
#define BANK_RAM1  0x05
#define BANK_RAM2  0x06
#define BANK_RAM3  0x07

// LCD
// LCD is mapped to XDATA 0..3  when BS2 = 0;
#define LCD_BASE  0xFFFC   // BASE Address for LCD Display

#define LCD_CTRL_W      (*(__xdata volatile unsigned char *) (LCD_BASE + 0) )   // I/O Port on xdata bus @ 0 
#define LCD_DATA_W      (*(__xdata volatile unsigned char *) (LCD_BASE + 1) )   // I/O Port on xdata bus @ 1 
#define LCD_CTRL_R      (*(__xdata volatile unsigned char *) (LCD_BASE + 2) )   // I/O Port on xdata bus @ 2 
#define LCD_DATA_R      (*(__xdata volatile unsigned char *) (LCD_BASE + 3) )   // I/O Port on xdata bus @ 3  


/*------------------------------------------------------------------------------------------*/

#endif //ndef __HWARE_H__  
