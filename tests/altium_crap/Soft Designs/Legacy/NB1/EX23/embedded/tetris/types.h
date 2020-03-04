/*****************************************************************
 * VERSION CONTROL:	@(#)types.h	1.14 04/09/16
 * DESCRIPTION:	
 * 	Specific 'osek' definitions for 8051
 ****************************************************************/
#ifndef _H_OS_TYPES
#define _H_OS_TYPES

/* Implementation compulsory parameters */
#define         OSTICKSPERBASE                  1
#define         OSMINCYCLE                      1
#define         OSTICKDURATION                  10000000
#define         OSTICKDURATIONINMSCS            10

/* memory qualifier for fast access variables */
#define _os_FASTALLOC				__idata

/**************************************************************** 
 * MACRO:	_os_USETRAPS
 * 
 * DESCRIPTION:	This target macro is defined:
 * 	- 'non-zero' in targets where support for system 
 * 	traps/interrupts exists (and the mechanism is used to 
 * 	implement the OSEK system services).
 * 	- 'zero' otherwise
 * 	- Defined in target file: types.h
 ***************************************************************/
#define		_os_USETRAPS	0

/* see 'osek.h' */
#define  	_os_SYSINLINE	

#endif


