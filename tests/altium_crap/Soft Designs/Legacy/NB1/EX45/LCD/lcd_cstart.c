/**************************************************************************
|*    Startup code automatically generated and updated by:
|*    TASKING 1.88r1 (build version: 1.37)
|*    Do not edit unless switching off the automatic generation
|*    checkbox in Project | Project Options | Processor | Startup Code
|*
|*    Copyright 2001-2003 Altium BV
 */

#define P0      (*(__bsfr volatile unsigned char *)0x80)
#define SP      (*(__sfr  volatile unsigned char *)0x81)
#define ROMSIZE (*(__sfr  volatile unsigned char *)0x8F)
#define P1      (*(__bsfr volatile unsigned char *)0x90)
#define XP      (*(__sfr  volatile unsigned char *)0x9F)
#define P2      (*(__bsfr volatile unsigned char *)0xA0)
#define P3      (*(__bsfr volatile unsigned char *)0xB0)
#define PSW     (*(__bsfr volatile unsigned char *)0xD0)

extern void _init( void );
extern int   main( int argc );
extern void _exit( void );
#pragma weak    _Exit
#pragma alias   _Exit = _exit

extern char __idata _lc_bs[];		/* system stack begin label */


__interrupt(0x0) __frame() void _start( void ) __at(0x2b)
{
	PSW = 0;								/* select register bank 0 */

	XP = 0x00;							/* needed when using __pdata */

	ROMSIZE = 0x10;					/* size internal Program memory */

	SP = (unsigned char)_lc_bs;		/* initialize system stack pointer */

	_init();								/* initialize C variables */
	main(0);
	_exit();
}

/* The exit() function jumps to _exit. When it is required to restart
 * the program, SP and _SP should be initialized again (if applicable).
 */
void _exit( void )
{
	while(1);								/* loop infinite */
}
