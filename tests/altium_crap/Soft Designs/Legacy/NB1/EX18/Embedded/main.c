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
**                    0 - STDIN - RS232 Serial in                         *
**                    1 - STDOUT - RS232 Serial out                       *
**                    2 - STDERR - LCD                                    *
**                 See also the _read.c and _write.c modules in this      *
**                 example.                                               *
**                                                                        *
**  COPYRIGHT   :  2004 Altium BV                                         *
**                                                                        *
**************************************************************************/

#include <stdio.h>
#include <string.h>
#include "lcd.h"

void cmd_version( void )
{
    printf( "IO Redirection example v1.0\r\n" );
}

void cmd_help( void )
{
    printf( "Valid commands:\r\n" );
    printf( "  help\r\n" );
    printf( "  version\r\n" );
}

void
handle_command( char *cmd )
{
    char validcmd = 0;
    size_t len = strlen( cmd );

    // check for and remove ending newline
    if ( cmd[len-1] == '\n' )
    {
       cmd[len-1] = '\0';
       --len;
    }

    switch ( len )
    {
    case 4:
        if ( !strcmp(cmd,"help") )
        {
            validcmd = 1;
            cmd_help();
        }
        break;
    case 7:
        if ( !strcmp(cmd,"version") )
        {
            validcmd = 1;
            cmd_version();
        }
        break;
    }
    if ( !validcmd )
    {
        printf( "Invalid command: \"%s\"\r\n", cmd );
    }
}

void main( void )
{
    char line[100];

    lcd_init();
    while ( 1 )
    {
        do
        {
            /* message printed to stdout = serial out */
            printf( "Enter command: " );
            /* Read a line from stdin = serial in */
            fgets( line, sizeof(line), stdin );
        } while ( strlen(line) <= 1 );
        /* and write it to stdout = LCD */
        fprintf( stderr, "Command entered:%s\n", line );

        handle_command( line );
    }
}
