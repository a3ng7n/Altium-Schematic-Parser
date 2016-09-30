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

// Include device driver interfaces
#include <timing.h>
#include <drv_max1037.h>
#include <drv_max1037_cfg_instance.h>   // Needed to display bus sharing & blocking modes only!


static void init( void );

max1037_t * adc;

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

    init();

    for (;;)
    {
        t = clock() + CLOCKS_PER_SEC;
        printf( "%4d: Read = %d\n", (int)(t / CLOCKS_PER_SEC), max1037_read( adc ) );
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

    // Display information on the configuration of the MAX1037 device driver
#if DRV_MAX1037_INSTANCE_BUS_SHARING_TRUE_USED
    puts( "MAX1037: Bus sharing ON" );
#else
    puts( "MAX1037: Bus sharing OFF" );
#endif
#if DRV_MAX1037_INSTANCE_BLOCKING_TRUE_USED
    puts( "MAX1037: Driver is in blocking mode" );
#else
    puts( "MAX1037: Driver is in non-blocking mode" );
#endif
    printf( "Press any key to continue: ");
    while( getchar() == -1 ) ;
    puts( "okidokie" );

    // ...and finally open the device driver
    printf( "Opening ADC:  ");
    adc = max1037_open( DRV_MAX1037_0 );
    puts ( adc ? "OK" : "Fail" );
}

