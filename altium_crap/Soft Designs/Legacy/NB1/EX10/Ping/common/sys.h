/*************************************************************************
**
**  VERSION CONTROL:	@(#)sys.h	1.1	03/04/15
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	Common system routines header,
**                      every platform should implement it's own
**                      platformheader and source.
**                      The global settingsfile tcpipset.h for the project
** 			should include both sys.h and the platform specific
**			file.
**
**************************************************************************/

#ifndef _SYS_H_
#define _SYS_H_

#include "stddef.h"

//**************************************************************************

#ifdef TCPIPDEBUG
#include <stdio.h>
#define debug_global(x) (x)
#else
#define debug_global(x) {}
#endif

//**************************************************************************

// memory space definition to indicate callback functions
#ifndef CALLBACKMEMSPEC
#define CALLBACKMEMSPEC	// no specific memory space needed
#endif

// memory space definition to allocate constants for some compilers
#ifndef ROMMEMSPEC
#define ROMMEMSPEC const	// no specific memory space needed
#define ROMMEMSPEC_AUTOCAST	// compiler can autocast from ROM to default memory
#endif

// special version of some functions needed to have one of the parameters pointing to rom space
#ifdef ROMMEMSPEC_AUTOCAST
#define strcpy_rom(to, from_rom) strcpy(to, from_rom)
#define strcat_rom(to, from_rom) strcat(to, from_rom)
#define strlen_rom(s_rom) strlen(s_rom)
#define strcmp_rom(s1, s2_rom) strcmp(s1, s2_rom)
#define strncmp_rom(s1, s2_rom, len) strncmp(s1, s2_rom, len)
#define strstr_rom(s1, s2_rom) strstr(s1, s2_rom)
#define strpbrk_rom(s1, s2_rom) strpbrk(s1, s2_rom)
#else
extern char *strcpy_rom(register char *s1, register ROMMEMSPEC char *s2);
extern char *strcat_rom(register char *s1, register ROMMEMSPEC char *s2);
extern size_t strlen_rom(register ROMMEMSPEC char *s);
extern Sint16 strcmp_rom(register const char *s1, register ROMMEMSPEC char *s2);
extern Sint16 strncmp_rom(register const char *s1, register ROMMEMSPEC char *s2, size_t len);
extern char *strpbrk_rom(register const char *cs, register ROMMEMSPEC char *ct);
extern char *strstr_rom(register const char *cs, register ROMMEMSPEC char *ct);
#endif

#ifdef TCPIPDEBUG
// All constant strings for debugging are contained in a s() macro.
// This way we can do a just-in-time move of ROM data to RAM for
// processors who need this, if we don't want the startup code
// to allocate RAM for all the constant strings we need for debugging
// the program.
#ifndef s
#define s(x) (x)	// constant strings are directly accessible at runtime
#endif
#endif

//**************************************************************************

#ifdef TCPIPDEBUG
// source in tcpip_debug.c
extern Sint16 printbytes(char *pdata, Sint16 length);
extern Sint16 printchars(char *cdata, Sint16 length);
#endif

//**************************************************************************

#endif
