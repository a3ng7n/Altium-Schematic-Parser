/********************************************************************\
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    Show how to use the adc084s021 device driver.
|*
\********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <devices.h>

#include <timing.h>
#include <drv_adc084s021.h>
#include <drv_ioport.h>


static void init( void );
static int next_channel(uint8_t channels, uint8_t current_channel, uint8_t max_channels);

adc084s021_t    *adc;
ioport_t        *ioport;

/**********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Continuously read and display the contents of the ADC
 */

void main( void )
{
    clock_t t;
    int d1, d2, d3, d4;
    int d1_prev = -2, d2_prev = -2, d3_prev = -2, d4_prev = -2;
    uint8_t channels;

    init();

    for (;;)
    {
        t = clock() + CLOCKS_PER_SEC / 10;

        /* read dip switches */
        channels = ioport_get_value(ioport, 0) & 0x0f;
        ioport_set_value(ioport, 1, channels);

        if (channels & (1 << 0))
        {
            d1 = adc084s021_read( adc, next_channel(channels, 0, 4) );
        }
        else
        {
            d1 = -1;
        }

        if (channels & (1 << 1))
        {
            d2 = adc084s021_read( adc, next_channel(channels, 1, 4) );
        }
        else
        {
            d2 = -1;
        }

        if (channels & (1 << 2))
        {
            d3 = adc084s021_read( adc, next_channel(channels, 2, 4) );
        }
        else
        {
            d3 = -1;
        }

        if (channels & (1 << 3))
        {
            d4 = adc084s021_read( adc, next_channel(channels, 3, 4) );
        }
        else
        {
            d4 = -1;
        }

        if (d1 != d1_prev || d2 != d2_prev || d3 != d3_prev || d4 != d4_prev)
        {
            printf( "%4d: Read = %d %d %d %d\n", (int)(t / CLOCKS_PER_SEC), d1, d2, d3, d4 );
        }
        d1_prev = d1;
        d2_prev = d2;
        d3_prev = d3;
        d4_prev = d4;

        while( clock() < t ) __nop();
    }

}

/**********************************************************************
|*
|*  Function    : init
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Initialize the hardware
 */

static void init( void )
{
    // Say hello to the user
    puts( "File '" __FILE__ "' compiled " __DATE__ ", " __TIME__  );

    printf( "Press any key to continue: ");
    while( getchar() == -1 ) ;

    // ...and finally open the device driver
    printf( "Opening ADC:  ");
    adc = adc084s021_open( DRV_ADC084S021_1 );
    puts ( adc ? "OK" : "Fail" );
    ioport = ioport_open( DRV_IOPORT_1 );
}

static int next_channel(uint8_t channels, uint8_t current_channel, uint8_t max_channels)
{
    uint8_t mask;
    int     i;

    /* check if bit is set in bits for higher channel numbers */
    mask = ~( ((int)1 << (current_channel + 1)) -1 );
    if (mask & channels)
    {
        for (i = current_channel + 1; i < max_channels; i++)
        {
            if (channels & (1 << i))
            {
                return i;
            }
        }
    }

    /* check if bit is set in bits for lower channel numbers */
    mask = (1 << current_channel) - 1;
    if (mask & channels)
    {
        for (i = 0; i < current_channel; i++)
        {
            if (channels & (1 << i))
            {
                return i;
            }
        }
    }

    return current_channel;
}

