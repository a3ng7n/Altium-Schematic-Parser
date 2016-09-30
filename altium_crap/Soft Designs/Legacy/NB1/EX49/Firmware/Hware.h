//       __  __ _       __ ___     ____   ______    __  __
//      / / / /| |     / //   |   / __ \ / ____/   / / / /
//     / /_/ / | | /| / // /| |  / /_/ // __/     / /_/ /
//    / __  /  | |/ |/ // ___ | / _, _// /___ _  / __  /
//   /_/ /_/   |__/|__//_/  |_|/_/ |_|/_____/(_)/_/ /_/
//

#ifndef __HWARE_H__
#define __HWARE_H__

#include <regtsk51a.sfr>

// ------------------------------------------------------------------------------------------
//   Oscillator Frequency
// ------------------------------------------------------------------------------------------
#define FOSC 20000000L                                          // Oscillator frequency

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

//-------------------------------------------------------------------------------------------
//   Port 0
// ------------------------------------------------------------------------------------------
#define SELECT_LCD 1
#define SELECT_RAM 0
#define RAM_LCD         P0_0                                    // 1 = LCD mapped to XDATA , 0 = RAM mapped to XDATA

#define MODE_16BIT       1
#define MODE_8BIT        0
#define RAM_MODE16      P0_1                                    // 1 = 16 bit mode, RAM accessed from Receiver
                                                                // 0 =  8 bit mode, RAM accessed from 8051 as XDATA
#define TX_START        P0_2                                    // 0 = Transmit Comma Characters
                                                                // 1 = Start State Machine to transmit test sequence
#define RX_START        P0_3                                    // 1 = Start waiting for Comma Characters

#define LCD_BACKLIGHT   P0_4                                    // 0 = Backlight Off       , 1 = Backlight On
#define RIO_SELECT      P0_5                                    // 0 = RIO0, 1 = RIO1   Receiver select

//-------------------------------------------------------------------------------------------
//   Port 1
// ------------------------------------------------------------------------------------------
// inputs
#define RX0_DONE       P1_0                                     // 0=Receiver 0 busy,     1=Receiver 0 done
#define TX_DONE        P1_1                                     // 0=Transmitter busy,    1=Transmitter ready to start transmit sequence

#define BUTTON_DOWN    0
#define BUTTON_UP      1
#define TEST_BUTTON    P1_2                                     // 0=Test button pressed, 1=Test button released

#define SPI_DIN        P1_3                                     // input data from SPI bus

// loopback inputs from dip-switch
#define LOOP_0_PAR      P1_4                                    // 1 : parallel loopback enabled transceiver 0
#define LOOP_0_SER      P1_5                                    // 1 : serial loopback enabled transceiver 0
#define LOOP_1_PAR      P1_6                                    // 1 : parallel loopback enabled transceiver 1
#define LOOP_1_SER      P1_7                                    // 1 : serial loopback enabled transceiver 1

// outputs
#define SPI_SEL        P1_0                                     // SPI select output
#define SPI_MODE       P1_1
#define SPI_CLK        P1_2
#define SPI_DOUT       P1_3

#define SPKR_OUT       P1_4                                     // Speaker Output

#define RIO_RESET      P1_5                                     // Active high reset for rocket I/O transceivers

//-------------------------------------------------------------------------------------------
//   Port 2
// ------------------------------------------------------------------------------------------
#define LED_PORT        P2                                      // LED output

//-------------------------------------------------------------------------------------------
//   XDATA
// ------------------------------------------------------------------------------------------

// LCD
// LCD is mapped to XDATA 0..3 when RAM_LCD is 1;
#define LCD_BASE  0x0000                                        // BASE Address for LCD Display

#define LCD_CTRL_W      (*(__xdata volatile unsigned char *) (LCD_BASE + 0) )   // I/O Port on xdata bus @ 0
#define LCD_DATA_W      (*(__xdata volatile unsigned char *) (LCD_BASE + 1) )   // I/O Port on xdata bus @ 1
#define LCD_CTRL_R      (*(__xdata volatile unsigned char *) (LCD_BASE + 2) )   // I/O Port on xdata bus @ 2
#define LCD_DATA_R      (*(__xdata volatile unsigned char *) (LCD_BASE + 3) )   // I/O Port on xdata bus @ 3

/*------------------------------------------------------------------------------------------*/

#endif //ndef __HWARE_H__





