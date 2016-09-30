/*****************************************************************************\
|*
|*  IN PACKAGE:
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:
|*
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <timing.h>

#include <devices.h>
#include <drv_uart8.h>
#include <per_ioport.h>

static void init( void );

uart8_t * uart;
volatile uint8_t * leds;

/**********************************************************************
|*
|*  FUNCTION    :
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION :
 */

void main( void )
{
    int c;
    uint32_t baudrate;

    init();

    puts("Serial mode:");
    printf("- databits %i\n", uart8_get_databits(uart));
    switch (uart8_get_parity(uart))
    {
    case UART8_ODD_PARITY : puts("- odd parity"); break;
    case UART8_EVEN_PARITY : puts("- even parity"); break;
    case UART8_USER_PARITY : puts("- user controlled parity"); break;
    default: puts("- no parity"); break;
    }
    printf("- %i stopbits\n\n", uart8_get_stopbits(uart));

    puts("Echoing: ");
    while( 1 )
    {
        c = uart8_getchar( uart );

        if (c & UART8_BREAK )
        {
            // BREAK condition found, reset LED counter, reset input buffer

            // The UART buffers are cleared by setting the baudrate
            puts("\nbreak");
            baudrate = uart8_get_baudrate( uart );
            uart8_set_baudrate( uart, baudrate );
            *leds = 0;
        }
        else
        {
            // Normal character found, show and increase LED counter
            printf("%02X%c ", c & 0xFF, (c & UART8_PARERR) ? '+' : '-');
            uart8_set_user_parity( uart, (c & UART8_PARERR) ? 1 : 0);
            uart8_putchar( uart, c );
            (*leds)++;
        }
    }
}

/**********************************************************************
|*
|*  FUNCTION    : init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize hardware & device drivers
 */

static void init( void )
{
    puts( "UART example echo, " __FILE__ " compiled " __DATE__ ", " __TIME__ );

    if ( leds = ( void *)per_ioport_get_base_address( PRTIO ), !leds )
    {
        abort();
    }

    if ( uart = uart8_open( DRV_UART8_ECHO ), !uart )
    {
        puts( "Fatal: unable to initialize UART" );

        for (;;)
        {
            // Flash LEDs to indicate there's something wrong

            *leds = 0xFF;
            delay_ms( 250 );
            *leds = 0x00;
            delay_ms( 250 );
        }
    }
    puts( "UART intialization done\n" );

   *leds = 0x00;
}
