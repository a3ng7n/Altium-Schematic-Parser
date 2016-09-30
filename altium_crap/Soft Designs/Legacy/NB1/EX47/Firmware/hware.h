#ifndef __HWARE_H__
#define __HWARE_H__ 

#include <regtsk51a.sfr>

// ------------------------------------------------------------------------------------------
//   Oscillator Frequency
// ------------------------------------------------------------------------------------------

#define FOSC 80000000L                                          // Oscillator frequency

// ------------------------------------------------------------------------------------------
//   Interrupt Vectors
// ------------------------------------------------------------------------------------------

#define __INTNO(nr) ((8*nr)+3)

#define INTVEC_EXT0 (0x03)                                      // interrupt number 0, vector address 0x03
#define INTVEC_T0   (0x0B)
#define INTVEC_EXT1 (0x13)
#define INTVEC_T1   (0x1B)
#define INTVEC_SIO  (0x23)

// ------------------------------------------------------------------------------------------
//   SFR Port expansions
// ------------------------------------------------------------------------------------------

#define SFR_PORT0  (*(__bsfr volatile unsigned char *) 0x86 )   // I/O Port on SFR bus @ 0x86
#define SFR_PORT1  (*(__bsfr volatile unsigned char *) 0x8E )   // I/O Port on SFR bus @ 0x8E
#define SFR_PORT2  (*(__bsfr volatile unsigned char *) 0x96 )   // I/O Port on SFR bus @ 0x96
#define SFR_PORT3  (*(__bsfr volatile unsigned char *) 0x9E )   // I/O Port on SFR bus @ 0x9E
#define SFR_PORT4  (*(__bsfr volatile unsigned char *) 0xA6 )   // I/O Port on SFR bus @ 0xA6
#define SFR_PORT5  (*(__bsfr volatile unsigned char *) 0xAE )   // I/O Port on SFR bus @ 0xAE
#define SFR_PORT6  (*(__bsfr volatile unsigned char *) 0xB6 )   // I/O Port on SFR bus @ 0xB6
#define SFR_PORT7  (*(__bsfr volatile unsigned char *) 0xBE )   // I/O Port on SFR bus @ 0xBE
#define SFR_PORT8  (*(__bsfr volatile unsigned char *) 0xC6 )   // I/O Port on SFR bus @ 0xC6
#define SFR_PORT9  (*(__bsfr volatile unsigned char *) 0xCE )   // I/O Port on SFR bus @ 0xCE
#define SFR_PORT10 (*(__bsfr volatile unsigned char *) 0xD6 )   // I/O Port on SFR bus @ 0xD6


#define KEY_PORT          SFR_PORT0                             // port for Keyboard
#define LED_PORT          SFR_PORT1                             // output port for LEDs
#define DIP_PORT          SFR_PORT1                             // input port for DIP-Switches
#define SPK_PORT          SFR_PORT2                             // Port for Speaker

//-------------------------------------------------------------------------------------------
//   Port 0
// ------------------------------------------------------------------------------------------

#define SELECT_LCD 1
#define SELECT_RAM 0

#define RAM_LCD         P0_0                                    // 1 = LCD mapped to XDATA , 0 = RAM mapped to XDATA
#define RAM_BANK        P0_1                                    // A16 of RAM chip
#define RAM_CHIP        P0_2                                    // 0 = RAM0                , 1 = RAM1
#define LCD_BACKLIGHT   P0_4                                    // 0 = Backlight Off       , 1 = Backlight On
#define SPEAKER_ENABLE  P0_5                                    // 0 = Speaker disabled    , 1 = Speaker enabled
#define SER_SEL0        P0_6                                    // 00 = RXD -> TXD
#define SER_SEL1        P0_7                                    // 01 = RTS -> CTS
                                                                // 02 = CAN
#define SER_SEL_RXDTXD  0x00                                    // mask for SER_SEL[0..1]
#define SER_SEL_RTSCTS  0x40
#define SER_SEL_CAN     0x80

//-------------------------------------------------------------------------------------------
//   Port 1
// ------------------------------------------------------------------------------------------

//---------------- I2C Control Lines -------------------------------
#define SCL       P1_1                                          // pin for SCL
#define SDA       P1_2                                          // pin for SDA
#define SCL_CTRL  P1_1                                          // Control Pin for SCL
#define SDA_CTRL  P1_2                                          // Control Pin for SDA

#define CTRL_HIZ   (1)                                          // Control definition for high impedance
#define CTRL_OUT   (0)                                          // for output
#define CTRL_IN    (1)                                          // for input

//---------------- SPI Control Lines -------------------------------
#define SPI_CLK   P1_5                                          // SPI Clock Line
#define SPI_DIN   P1_7                                          // SPI Data In Line
#define SPI_DOUT  P1_7                                          // SPI Data Out Line
#define SPI_SEL   P1_5                                          // SPI Select Line
#define SPI_MODE  P1_3                                          // SPI Mode Line

//---------------- PS2KBD Control Lines ----------------------------
#define PS2_CLK_RD P1_0                                         // PS2 Port Clock Read Line
#define PS2_DTA_RD P1_1                                         // PS2 Port Clock Read Line
#define PS2_CLK_WR P1_0                                         // PS2 Port Clock Write Line
#define PS2_DTA_WR P1_1                                         // PS2 Port Clock Write Line

//-------------------------------------------------------------------------------------------
//   Port 2
// ------------------------------------------------------------------------------------------

#define OB_RAM_BANK  P2                                         // Bank select switch for onboard memory

//-------------------------------------------------------------------------------------------
//   XDATA
// ------------------------------------------------------------------------------------------

#define BANK_LCD   0x00
#define BANK_RAM0  0x04
#define BANK_RAM1  0x05
#define BANK_RAM2  0x06
#define BANK_RAM3  0x07

// LCD
// LCD is mapped to XDATA 0..3  when BS2 = 0;
#define LCD_BASE  0x0000                                        // BASE Address for LCD Display

#define LCD_CTRL_W      (*(__xdata volatile unsigned char *) (LCD_BASE + 0) )   // I/O Port on xdata bus @ 0 
#define LCD_DATA_W      (*(__xdata volatile unsigned char *) (LCD_BASE + 1) )   // I/O Port on xdata bus @ 1 
#define LCD_CTRL_R      (*(__xdata volatile unsigned char *) (LCD_BASE + 2) )   // I/O Port on xdata bus @ 2 
#define LCD_DATA_R      (*(__xdata volatile unsigned char *) (LCD_BASE + 3) )   // I/O Port on xdata bus @ 3  


/*------------------------------------------------------------------------------------------*/

#endif //ndef __HWARE_H__  
