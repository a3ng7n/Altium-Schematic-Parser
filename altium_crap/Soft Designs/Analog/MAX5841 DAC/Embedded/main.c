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
#include <devices.h>

// Include the device driver interfaces
#include <timing.h>
#include <drv_max5841.h>

static void init( void );

max5841_t * dac;

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

    init();

    for (;;)
    {
        t = clock() + CLOCKS_PER_SEC;
        printf( "%4d: Write %d = %d\n", (int)(t / CLOCKS_PER_SEC), (int)x, max5841_out( dac, 0, x, true ) );
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
    printf( "Press any key to continue: ");
    while( getchar() == -1 ) ;
    puts( "\bthank you" );

    // And finally, open the DAC device driver
    printf( "Opening DAC:  ");
    dac = max5841_open( DRV_MAX5841_0 );
    puts ( dac ? "OK" : "Fail" );
}

