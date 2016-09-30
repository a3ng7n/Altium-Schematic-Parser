/**************************************************************************
**                                                                        *
**  FILE        :  _read.c                                                *
**                                                                        *
**  DESCRIPTION :  Source file for _read() routine                        *
**                 Read a block of characters from a file and return the  *
**                 the number of characters read.                         *
**                                                                        *
**  This routine is customised to connect STDIN to the KeyPad.            *
**                                                                        *
**  COPYRIGHT   :  2001-2004 Altium BV                                    *
**                                                                        *
**************************************************************************/

#include        <stdio.h>
#include        <io.h>

#define STDIN  0
#define STDOUT 1
#define STDERR 2

#define KEY      (*(volatile unsigned char *)(0xfffa))
#define KEYVALID (*(volatile unsigned char *)(0xfffb))
#define KEYRESET (*(volatile unsigned char *)(0xfffb))

const char key2ascii[16] = "123C456D789EA0BF";

int
_read(_fd_t fd, void *buffer, size_t nbytes)
{
    char *cbuf = (char *)buffer;
    if ( fd == STDIN )
    {
        size_t nr = nbytes;
        while ( nr > 0 )
        {
            while ( !KEYVALID )
            {
                __nop();
            }
            *cbuf = key2ascii[KEY];
            KEYRESET = 1;
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
