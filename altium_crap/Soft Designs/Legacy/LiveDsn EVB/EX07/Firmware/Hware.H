#ifndef __HWARE_H__
#define __HWARE_H__

#include <regtsk51a.sfr>         // for register names

#define FCLK (50000000.0)
#define FOSC (33333333.0)          // oscillator frequency

#define MAX_MEMORY (1024l * 1024)  // maximum memory size

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

#define SFR_BASE  0x86
#define SFR_PORT0  (*(__bsfr volatile unsigned char *) SFR_BASE )       // I/O Port on SFR bus @ 0x86
#define SFR_PORT1  (*(__bsfr volatile unsigned char *) SFR_BASE + 8 )   // I/O Port on SFR bus @ 0x8E
#define SFR_PORT2  (*(__bsfr volatile unsigned char *) 0x96 )   // I/O Port on SFR bus @ 0x96
#define SFR_PORT3  (*(__bsfr volatile unsigned char *) 0x9E )   // I/O Port on SFR bus @ 0x9E
#define SFR_PORT4  (*(__bsfr volatile unsigned char *) 0xA6 )   // I/O Port on SFR bus @ 0xA6
#define SFR_PORT5  (*(__bsfr volatile unsigned char *) 0xAE )   // I/O Port on SFR bus @ 0xAE
#define SFR_PORT6  (*(__bsfr volatile unsigned char *) 0xB6 )   // I/O Port on SFR bus @ 0xB6
#define SFR_PORT7  (*(__bsfr volatile unsigned char *) 0xBE )   // I/O Port on SFR bus @ 0xBE
#define SFR_PORT8  (*(__bsfr volatile unsigned char *) 0xC6 )   // I/O Port on SFR bus @ 0xC6
#define SFR_PORT9  (*(__bsfr volatile unsigned char *) 0xCE )   // I/O Port on SFR bus @ 0xCE
#define SFR_PORT10 (*(__bsfr volatile unsigned char *) 0xD6 )   // I/O Port on SFR bus @ 0xD6

// ------------------------------------------------------------------------------------------
//   SFR 32bit Ports
// ------------------------------------------------------------------------------------------

#define SFR32_SAMPLE_RATE 0       // 32bit wide port for sample rate divider ratio
#define SFR32_END_ADDRESS 1       // register for end address of playback counter


#define CHANNEL_SELECT  P1_0      // bit for chanel select
#define CHANNEL_LEFT    1
#define CHANNEL_RIGHT   0


#endif
