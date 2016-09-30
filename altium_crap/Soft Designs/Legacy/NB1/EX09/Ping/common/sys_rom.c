/*************************************************************************
**
**  VERSION CONTROL:	@(#)sys_rom.c	1.1	03/04/15
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	Duplicates of common ANSI C-lib string functions
**       		but with one of the stringpointer parameters pointing
** 			to ROMMEMSPEC
**
**************************************************************************/

#include <tcpipset.h>

#ifdef ROMMEMSPEC_AUTOCAST
#ifndef TCPIPDEBUG
#error ROMMEMSPEC_AUTOCAST is defined, but sys_rom.c is included in project
#endif
#endif

#ifndef ROMMEMSPEC_AUTOCAST

//**************************************************************************

char *strcat_rom(register char *s1, register ROMMEMSPEC char *s2)
{
    register char *os1;

    os1 = s1;
    while (*s1++)
	;
    --s1;
    while ((*s1++ = *s2++))
	;
    return (os1);
}


char *strcpy_rom(register char *s1, register ROMMEMSPEC char *s2)
{
    register char *os1;

    os1 = s1;
    while ((*s1++ = *s2++))
	;
    return (os1);
}




Sint16 strcmp_rom(register const char *s1, register ROMMEMSPEC char *s2)
{
    Uint8 c1, c2;
    do
    {
	c2 = *s2++;
	c1 = *s1++;
    }
    while (c1 != '\0' && c1 == c2);
    return ((Sint16) c1 - (Sint16) c2);
}

size_t strlen_rom(register ROMMEMSPEC char *s)
{
    ROMMEMSPEC char *p;

    for (p = s; *s++;)
    {
    }
    return (s - p) - 1;
}


Sint16 strncmp_rom(register const char *s1, register ROMMEMSPEC char *s2, size_t n)
{
    Uint8 c1, c2;
    while (n--)
    {
	c2 = *s2++;
	c1 = *s1++;
	if (c1 == '\0' || c1 != c2)
	    return ((Sint16) c1 - (Sint16) c2);
    }
    return (0);
}


char *strpbrk_rom(register const char *cs, register ROMMEMSPEC char *ct)
{
    register ROMMEMSPEC char *search;

    do
    {
	for (search = ct; *search && *search != *cs; search++)
	    ;	/* look if character from cs is also in ct */

	if (*search)	/* if found the character in ct, */
	    return ((char *) cs);	/* return pointer        */
    }
    while (*cs++);

    return (NULL);	/* no matching character found     */
}


char *strstr_rom(register const char *cs, register ROMMEMSPEC char *ct)
{
    register ROMMEMSPEC char *search;
    register const char *search2;

    if (*ct)
    {
	for (; *cs; cs++)
	{
	    for (search = ct, search2 = cs; *search && *search == *search2; ++search, ++search2)
		;	/* look if string ct is in cs   */

	    if (!*search)	/* if found the string in ct,   */
		return ((char *) cs);	/* return pointer       */
	}
    }
    else
	return ((char *) cs);

    return (NULL);	/* no matching string found     */
}


//**************************************************************************

#endif // ROMMEMSPEC_AUTOCAST
