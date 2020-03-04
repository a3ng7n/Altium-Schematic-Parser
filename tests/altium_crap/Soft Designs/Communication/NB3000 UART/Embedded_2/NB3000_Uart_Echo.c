/*****************************************************************************\
|*
|*  IN PACKAGE:         Multicore Uart Echo Example
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:
|*
 */

#include "swplatform.h"

#define DELAY_FLASH         100
#define DELAY_ECHO          25

#define VAL_FLASH_BREAK     0xAA
#define VAL_FLASH_ERROR     0xFF

void main( void )
{
    int val;
    uint32_t baudrate;

    // Generated initialization code
    swplatform_init_stacks();

    if (led_port == NULL)
    {
        abort();
    }
    if ( echo_uart == NULL )
    {
        for (;;)
        {
            // Flash LEDs to indicate there's something wrong

            ioport_set_value( led_port, PRTIO_PA, VAL_FLASH_ERROR );
            delay_ms( DELAY_FLASH );
            ioport_set_value( led_port, PRTIO_PA, 0x00 );
            delay_ms( DELAY_FLASH );
        }
    }
    ioport_set_value( led_port, PRTIO_PA, 0x00 );

    while( 1 )
    {
        if ( val = uart8_getchar( echo_uart ), val == UART8_BREAK )
        {
            // BREAK condition found, reset LED counter, reset input buffer

            // The UART buffers are cleared by setting the baudrate
            baudrate = uart8_get_baudrate( echo_uart );
            uart8_set_baudrate( echo_uart, baudrate );

            // flash LED's and reset counter
            for (int i = 0; i < 2; i++)
            {
                ioport_set_value( led_port, PRTIO_PA, 0x00 );
                delay_ms( DELAY_FLASH );
                ioport_set_value( led_port, PRTIO_PA, VAL_FLASH_BREAK );
                delay_ms( DELAY_FLASH );
            }
            ioport_set_value( led_port, PRTIO_PA, 0x00 );
        }
        else
        {
            // Normal character found, increase LED counter
            uart8_putchar( echo_uart, val );
            val = ioport_get_value( led_port, PRTIO_PA );
            ioport_set_value( led_port, PRTIO_PA, val + 1 );
            // delay to make counter visible
            delay_ms( DELAY_ECHO );
        }
    }
}

