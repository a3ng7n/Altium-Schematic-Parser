/**************************************************************************
|*    Startup code automatically generated and updated by:
|*    TASKING 1.149r0 (build version: 1.0)
|*    Do not edit unless switching off the automatic generation
|*    checkbox in Project | Project Options | Processor | Startup Code
|*
|*    Copyright 2001-2004 Altium BV
*/

#ifndef         P0
# define        P0	(*(__bsfr volatile unsigned char *)0x80)
#endif
#ifndef         SP
# define        SP	(*(__sfr  volatile unsigned char *)0x81)
#endif
#ifndef         ROMSIZE
# define        ROMSIZE (*(__sfr  volatile unsigned char *)0x8F)
#endif
#ifndef         P1
# define        P1	(*(__bsfr volatile unsigned char *)0x90)
#endif
#ifndef         XP
# define        XP      (*(__sfr  volatile unsigned char *)0x9F)
#endif
#ifndef         P2
# define        P2	(*(__bsfr volatile unsigned char *)0xA0)
#endif
#ifndef         P3
# define        P3	(*(__bsfr volatile unsigned char *)0xB0)
#endif
#ifndef         PSW
# define        PSW	(*(__bsfr volatile unsigned char *)0xD0)
#endif

extern void _init( void );
extern int   main( int argc );
extern void  exit( int );
#pragma weak    exit
#pragma extern _Exit

extern char __idata _lc_bs[];		/* system stack begin label */


extern char __xdata * __data _SP;
extern char __xdata _lc_ue_vstack_xdata[];



/* Force this section to be named '_start', it then will be located
 * in the first block of 256 bytes in code space by using LSL.
 */
#pragma section code=-f
/* Disabled waring, because the interrupt interrupt function _start does
 * not need to save register R0..7,A,B,DPL,DPH and PSW.
 */
#pragma warning 765
__interrupt(0x0) __frame() void _start( void )
{
	PSW = 0;								/* select register bank 0 */

	XP = 0x00;							/* needed when using __pdata */


	ROMSIZE = 0x10;					/* size internal Program memory */


	SP = (unsigned char)_lc_bs;		/* initialize system stack pointer */


	_SP = _lc_ue_vstack_xdata-1;		/* initialize 'virtual stack' pointer */

	_init();								/* initialize C variables */
	exit( main(0) );
}

