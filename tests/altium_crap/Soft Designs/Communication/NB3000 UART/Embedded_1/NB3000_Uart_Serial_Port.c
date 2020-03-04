/*****************************************************************************\
|*
|*  IN PACKAGE:         Multicore Uart Echo Example
|*
|*  COPYRIGHT:          Copyright (c) 2010, Altium
|*
|*  DESCRIPTION:
|*
 */

#include <unistd.h>
#include <string.h>
#include "swplatform.h"

#define BUFSIZE     32

void main( void )
{
    uint8_t buf[BUFSIZE];
    int i;

    // For terminal output we use stdout, which has automatically been opened before main()
    puts( "UART example, " __FILE__ " compiled " __DATE__ ", " __TIME__ );

    // Generated initialization code (see swplatform.h)
    swplatform_init_stacks();

    if ( serial_port == NULL )
    {
        puts( "Fatal: unable to initialize UART" );
        abort();
    }
    puts( "UART intialization done." );

    // Generate data
    printf( "Fill buf:" );
    srand( 0x12345678 );
    for ( i = 0; i < sizeof( buf ); i++ )
    {
        buf[i] = rand() & 0xFF;
        printf( " %02X", buf[i] );
    }
    putchar( '\n' );

    // Write buffer over serial port:
    for ( i = 0; i < sizeof( buf ); i++ )
    {
        uart8_putchar( serial_port, buf[i] );
    }
    if ( uart8_transmit_idle( serial_port ) )
    {
        puts( "ERROR: No transmission" );
    }

    // Transmission should be finished within transmission time for 10 x bytes x bittime.
    // To make sure, we calculate with 11 bits per byte

    delay_ms( 1000 * 12 * sizeof( buf ) / uart8_get_baudrate( serial_port ) );

    if ( uart8_transmit_idle( serial_port ) )
    {
        printf( "Done transmitting %d bytes...\n", i );
    }
    else
    {
        puts( "WARNING: transmission takes longer than expected..." );
    }

    // Are there any characters for me?

    puts( uart8_receive_buf_available( serial_port ) ? "Remote answered" : "ERROR: No answer" );
    memset( buf, 0, sizeof( buf ));

    // Read it back
    i = uart8_read( serial_port, (void *)buf, sizeof( buf ) );
    printf( "Read returned %d bytes\nReceived:", i );
    for ( int x = 0; x < i; x++ )
    {
        printf( " %02X", buf[x] );
    }
    putchar( '\n' );

    if ( uart8_receive_buf_available( serial_port ) )
    {
        puts( "Extraneous bytes sent by echo!" );
    }

    // Send a break to have remote empty it's buffers
    puts( "Clearing remote..." );
    uart8_putbreak( serial_port, 20 );
    delay_ms( (50 * 1000) / uart8_get_baudrate( serial_port ));

   // Signal to the remote not to send me anything back
   puts( "Stopping remote..." );
   uart8_rts( serial_port, true );

   // Clear our own buffers by writing baudrate
   uart8_set_baudrate( serial_port, uart8_get_baudrate( serial_port ));

    // Write buffer over serial port:
    for ( i = 0; i < sizeof( buf ); i++ )
    {
        uart8_putchar( serial_port, buf[i] );
    }
    while( !uart8_transmit_idle( serial_port ) ) __nop();  // Wait until transmission is actually done
    printf( "Transmitted %d bytes\n", i );

    // Are there any characters are there for me?
    if( uart8_receive_buf_available( serial_port ) )
    {
        puts( "ERROR: Remote ignored RTS and answered!" );

        memset( buf, 0xFF, sizeof( buf ));
        i = uart8_read( serial_port, (void *)buf, sizeof( buf ) );
        printf( "Read returned %d bytes\nReceived:", i );
        for ( int x = 0; x < i; x++ )
        {
            printf( " %02X", buf[x] );
        }
        putchar( '\n' );
    }
    else
    {
        puts( "OK: No answer" );
    }
    puts( "Ready" );
}

