/**************************************************************************
**                                                                        *
**  FILE        :  _write.c                                               *
**                                                                        *
**  DESCRIPTION :  Source file for _write() routine                       *
**                 Write a block of characters to a file and return the   *
**                 the number of characters written.                      *
**                                                                        *
**  This routine is customised to connect STDOUT to the LCD.              *
**                                                                        *
**  COPYRIGHT   :  2001-2004 Altium BV                                    *
**                                                                        *
**************************************************************************/

#include <stdio.h>
#include <io.h>
#include <ctype.h>
#include "lcd.h"

#define STDIN  0
#define STDOUT 1
#define STDERR 2

int
_write(_fd_t fd, const void *buffer, size_t nbytes)
{
    const char *cbuf = (const char *)buffer;
    if ( fd == STDOUT )
    {
        size_t nr = nbytes;
        while ( nr > 0 )
        {
           lcd_putc( *cbuf );
           ++cbuf;
           --nr;
        }
        return nbytes;
    }
    else
    {
        return _host_write( fd, buffer, nbytes );
    }
}
