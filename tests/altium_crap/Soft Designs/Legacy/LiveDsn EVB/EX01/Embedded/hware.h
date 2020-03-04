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
//   Internal ROM Size
// ------------------------------------------------------------------------------------------

#define ROM_SIZE 0x4000  // 16k


// ------------------------------------------------------------------------------------------
//   Oscillator Frequency
// ------------------------------------------------------------------------------------------

#define FOSC 50000000L  // Oscillator frequency

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
#define SFR_PORT11 (*(__bsfr volatile unsigned char *) 0xDE )   // I/O Port on SFR bus @ 0xDE
#define SFR_PORT12 (*(__bsfr volatile unsigned char *) 0xE6 )   // I/O Port on SFR bus @ 0xE6
#define SFR_PORT13 (*(__bsfr volatile unsigned char *) 0xEE )   // I/O Port on SFR bus @ 0xEE
#define SFR_PORT14 (*(__bsfr volatile unsigned char *) 0xF6 )   // I/O Port on SFR bus @ 0xF6
#define SFR_PORT15 (*(__bsfr volatile unsigned char *) 0xFE )   // I/O Port on SFR bus @ 0xFE

#define NOTEL_PORT        SFR_PORT0        // Port for Left  Audio Note
#define NOTER_PORT        SFR_PORT1        // Port for Right Audio Note
#define VOLUME_PORT       SFR_PORT14       // port for Audio Volume
#define U1H0_PORT         SFR_PORT2        // Port for lowest byte  of User I/O port
#define U1H1_PORT         SFR_PORT3        // Port for middle byte  of User I/O Port
#define U1H2_PORT         SFR_PORT4        // Port for top two bits of User I/O Port
#define LED_PORT          SFR_PORT5        // output port for LEDs
#define DIP_PORT          SFR_PORT5        // input port for DIP-Switches
#define KEY_PORT          SFR_PORT0        // input for Keys

#define M_KEY_TEST 0x80   // mask for Test/Reset Key
#define M_KEY_SW1  0x01
#define M_KEY_SW2  0x02
#define M_KEY_SW3  0x04
#define M_KEY_SW4  0x08
#define M_KEY_SW5  0x10
#define M_KEY_SW6  0x20

#define Seg7_START_ADDR   0xB6             // SFR address for segment 0
#define SEG7A_PORT        SFR_PORT6        // 7 segment display digit A
#define SEG7B_PORT        SFR_PORT7        // 7 segment display digit A
#define SEG7C_PORT        SFR_PORT8        // 7 segment display digit A
#define SEG7D_PORT        SFR_PORT9        // 7 segment display digit A
#define SEG7E_PORT        SFR_PORT10       // 7 segment display digit A
#define SEG7F_PORT        SFR_PORT11       // 7 segment display digit A

#define VLCDCTRL_PORT     SFR_PORT12
#define VLCDDATA_PORT     SFR_PORT13
#define VLCD_MASK_WR      0x20             // Mask for write signal  (high active)
#define VLCD_MASK_BL      0x80             // Mask for backlight bit


//-------------------------------------------------------------------------------------------
//   Port 0
// ------------------------------------------------------------------------------------------

#define SELECT_LCD 1
#define SELECT_RAM 0

#define RAM_LCD         P0_0      // 1 = LCD mapped to XDATA , 0 = RAM mapped to XDATA
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

//---------------- PS2KBD Control Lines ----------------------------
#define PS2_CLK_RD P1_0  // PS2 Port Clock Read Line
#define PS2_DTA_RD P1_1  // PS2 Port Clock Read Line
#define PS2_CLK_WR P1_0  // PS2 Port Clock Write Line
#define PS2_DTA_WR P1_1  // PS2 Port Clock Write Line

//-------------------------------------------------------------------------------------------
//   Port 2
// ------------------------------------------------------------------------------------------

#define OB_RAM_BANK  P2  // Bank select switch for onboard memory


//-------------------------------------------------------------------------------------------
//   XDATA
// ------------------------------------------------------------------------------------------
/*

XDATA is mapped into 8 banks of 64k
Bank Select is controlled via Port0, Bits 0..2
P0.0 : Selects lower (0) or upper (1) half of each 128k RAM chip
P0.1 : Selects RAM chip 0 or 1
P0.2 : Selects LCD (0) or RAM (1)  // not implemented in EB tester, must be set to 1

*/

#define BANK_LCD   0x00
#define BANK_RAM0  0x04
#define BANK_RAM1  0x05
#define BANK_RAM2  0x06
#define BANK_RAM3  0x07

/*------------------------------------------------------------------------------------------*/

#endif //ndef __HWARE_H__
