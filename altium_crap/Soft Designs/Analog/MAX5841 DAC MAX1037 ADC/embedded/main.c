/********************************************************************\
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    Show how to use the MAX1037 device driver.
|*
\********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

// Include the device driver interfaces
#include <devices.h>
#include <timing.h>
#include <devices.h>
#include <drv_max5841.h>
#include <drv_max1037.h>
#include <per_ioport.h>

static void init( void );

max5841_t * dac;
max1037_t * adc;
volatile uint8_t   * dipswitches;

/**********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Once per second, write the DAC
 */

void main( void )
{
    clock_t t;
    uint16_t x = 0;
    uint8_t  select;

    init();

    puts( "Time  Ch#0 Ch#1 Ch#2 Ch#3" );
    for (;;)
    {
        t = clock() + CLOCKS_PER_SEC;
        select = ~*dipswitches;
        *dipswitches = select;

        printf( "%4d:", (int)(t / CLOCKS_PER_SEC) );
        if ( select & 0x80 ) max5841_out( dac, 0, x, true );
        if ( select & 0x40 ) max5841_out( dac, 1, x, true );
        if ( select & 0x20 ) max5841_out( dac, 2, x, true );
        if ( select & 0x10 ) max5841_out( dac, 3, x, true );


        if ( select & 0x08 )
        {
            max1037_select_channel( adc, 0 );
            printf( " 0:%02X", max1037_read( adc ) );
        }
        else
        {
            printf( "     " );
        }

        if ( select & 0x04 )
        {
            max1037_select_channel( adc, 1 );
            printf( " 1:%02X", max1037_read( adc ) );
        }
        else
        {
            printf( "     " );
        }

        if ( select & 0x02 )
        {
            max1037_select_channel( adc, 2 );
            printf( " 2:%02X", max1037_read( adc ) );
        }
        else
        {
            printf( "     " );
        }

        if ( select & 0x01 )
        {
            max1037_select_channel( adc, 3 );
            printf( " 3:%02X", max1037_read( adc ) );
        }
        else
        {
            printf( "     " );
        }

        putchar('\n');
        x = (x + 100) & 0x3FF;
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
    puts( "Use switches to select in- and output channels" );

    // Get address of DIP switches:
    dipswitches = (void *)per_ioport_get_base_address( GPIO );

    // Open the ADC device driver
    printf( "Opening ADC: " );
    adc = max1037_open( DRV_MAX1037_0 );
    puts( adc ? "OK" : "Fail" );
    max1037_select_channel( adc, 0 );

    // And finally, open the DAC device driver
    printf( "Opening DAC:  ");
    dac = max5841_open( DRV_MAX5841_0 );
    puts ( dac ? "OK" : "Fail" );


}

