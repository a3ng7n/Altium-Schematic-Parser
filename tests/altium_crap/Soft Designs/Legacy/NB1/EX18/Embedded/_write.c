/**************************************************************************
**                                                                        *
**  FILE        :  _write.c                                               *
**                                                                        *
**  DESCRIPTION :  Source file for _write() routine                       *
**                 Write a block of characters to a file and return the   *
**                 the number of characters written.                      *
**                                                                        *
**  This routine is customised to connect STDOUT to the Serial port.      *
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

#define BASE_ADDRESS 0xfff8
#define XSBUF   (*(volatile unsigned char *)(BASE_ADDRESS+0))
#define XSCON   (*(volatile unsigned char *)(BASE_ADDRESS+1))

void serial_wait( void )
{
    while ( !(XSCON & 0x2) )
    {
        __nop();
    }
    XSCON &= ~0x2;
}

int
_write(_fd_t fd, const void *buffer, size_t nbytes)
{
    const char *cbuf = (const char *)buffer;
    if ( fd == STDOUT )
    {
        /* stdout is redirected to serial out */
        size_t nr = nbytes;
        while ( nr > 0 )
        {
           XSBUF = *cbuf;
           serial_wait();
           ++cbuf;
           --nr;
        }
        return nbytes;
    }
    else if ( fd == STDERR )
    {
        /* stderr is redirected to the LCD */
        size_t nr = nbytes;
        while ( nr > 0 )
        {
           if ( isprint(*cbuf) )
           {
               lcd_putc( *cbuf );
           }
           else if ( *cbuf == '\n' )
           {
               lcd_gotoxy(0,0);
           }
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
