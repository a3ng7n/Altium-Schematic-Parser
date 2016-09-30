/**************************************************************************
**                                                                        *
**  FILE        :  main.c                                                 *
**                                                                        *
**  DESCRIPTION :  IO Redirection example.                                *
**                 This example shows how to redirect standard IO to in-  *
**                 and output devices. All standard IO functions like     *
**                 printf, fscanf, putchar, fgets map on the low level    *
**                 functions _write() for output and _read() for input    *
**                 So to redirect IO to a specific device all that is     *
**                 required is to customize these functions for the       *
**                 specific input/output device. This example uses the    *
**                 following mappings:                                    *
**                    0 - STDIN - KeyPad                                  *
**                    1 - STDOUT - LCD                                    *
**                 See also the _read.c and _write.c modules in this      *
**                 example.                                               *
**                                                                        *
**  COPYRIGHT   :  2004 Altium BV                                         *
**                                                                        *
**************************************************************************/

#include <stdio.h>
#include "lcd.h"

void main( void )
{
    char ch;

    lcd_init();
    /* message printed to stdout = LCD */
    printf( "Press any key: " );
    while ( 1 )
    {
        /* Read a character from stdin = KeyPad */
        ch = getchar();
        /* and write it to stdout = LCD */
        putchar( ch );
    }
}
