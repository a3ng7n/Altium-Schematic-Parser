/**************************************************************************
**                                                                        *
**  FILE        :  _read.c                                                *
**                                                                        *
**  DESCRIPTION :  Source file for _read() routine                        *
**                 Read a block of characters from a file and return the  *
**                 the number of characters read.                         *
**                                                                        *
**  This routine is customised to connect STDIN to the Serial port.       *
**                                                                        *
**  COPYRIGHT   :  2001-2004 Altium BV                                    *
**                                                                        *
**************************************************************************/

#include        <stdio.h>
#include        <io.h>

#define STDIN  0
#define STDOUT 1
#define STDERR 2

#define BASE_ADDRESS 0xfff8
#define XSBUF   (*(volatile unsigned char *)(BASE_ADDRESS+0))
#define XSCON   (*(volatile unsigned char *)(BASE_ADDRESS+1))

int
_read(_fd_t fd, void *buffer, size_t nbytes)
{
    char *cbuf = (char *)buffer;
    if ( fd == STDIN )
    {
        size_t nr = nbytes;
        while ( nr > 0 )
        {
            while ( !(XSCON & 0x1) )
            {
                __nop();
            }
            XSCON &= ~0x1;
            *cbuf = XSBUF;
            /*
             * HyperTerminal by default only sends a carriage-return(='\r')
             * when pressing the 'Enter' key, to workaround this convert it to
             * a proper linefeed(='\n') character.
             * For terminal programs sending a '\r\n' combination this may
             * result in an extra (emtpy) line being returned. in that case
             * remove the if{} statement below.
             */
            if ( *cbuf == '\r' )  
            {
                putchar( '\n' );
                *cbuf = '\n';
            }
            ++cbuf;
            --nr;
        }
        return nbytes;
    }
    else
    {
        return _host_read( fd, buffer, nbytes );
    }
}
