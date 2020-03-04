/* sys_8051fpga.h */

#ifndef _SYS_8051FPGA_H_
#define _SYS_8051FPGA_H_

#ifdef IP
// defined in 8051fpga compiler headers
#undef IP
#endif

#ifdef TCPIPDEBUG
#include <stdio.h>
#endif


#define BIG_ENDIAN

typedef unsigned char Uint1;    /* 1 bit quantity         */
typedef unsigned char Uint8;    /* 8 bit quantity         */
typedef unsigned short Uint16;  /* 16 bit quantity        */
typedef unsigned long Uint32;   /* 32 bit quantity        */
typedef char Sint8; /* 8 bit quantity         */
typedef short Sint16;   /* 16 bit quantity        */
typedef long Sint32;    /* 32 bit quantity        */

// swap-macros
#define swapb(a, b) swap(a, b)
#define swapw(a, b) swap(a, b)
#define swapdw(a, b) swap(a, b)
#define swap(a, b) { (a) ^= (b); (b) ^= (a); (a) ^= (b); }

#ifdef ROMSTR
#define ROMMEMSPEC __rom
#ifdef TCPIPDEBUG
#define s(x) srom2ram(x)
char *srom2ram(ROMMEMSPEC char *srom);
#endif
#else
#define ROMMEMSPEC const
#endif

//**************************************************************************

// net-to-host-Uint16-pointer
// (convert net-order Uint16 at Uint8-pointer address to host-order Uint16)
#define ntohwp(bp) (*(Uint16*)(bp))
// net-to-host-Uint16
#define ntohw(w) (w)
// host-to-net-Uint16
#define htonw(w) (ntohw(w))

// net-to-host-Uint32
Uint32 ntohdw(Uint32 dw);
// host-to-net-Uint32
#define htondw(dw) (ntohdw(dw))

// net-Uint32-add-host-Uint16
// (add host-ordered Uint16 to a net-ordered Uint32)
#define ndwaddhw(dw, w) ((dw) + (w))

#define highbyte(w) ((Uint8)((w) >> 8))
#define lowbyte(w) ((Uint8)((w) & 0xFF))

//**************************************************************************

void sys_init(void);
void sys_sleep(Uint16 ms);

//**************************************************************************

#endif
