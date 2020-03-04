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
#include <drv_dac084s085.h>
#include <drv_ioport.h>

static void init( void );

dac084s085_t    *dac;
ioport_t        *ioport;
volatile bool   done;

/**********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Continuously write data to the selected DAC ports
 */

void main( void )
{
    clock_t t;
    uint8_t channels;
    int     val = 0, direction = 1;
    int     va, vb, vc, vd;
    int     va_prev = -2, vb_prev = -2, vc_prev = -2, vd_prev = -2;
    int     more;

    init();

    while (!done)
    {
        t = clock() + CLOCKS_PER_SEC / 10;

        /* read dip switches */
        channels = (uint8_t)ioport_get_value(ioport, 0);
        ioport_set_value(ioport, 1, channels);

        if (channels & (1 << 0))
        {
            more = channels & 0x0e;
            dac084s085_write( dac, DAC084S085_OUTA, (uint8_t)val, !more );
            va = val;
        }
        else
        {
            va = -1;
        }

        if (channels & (1 << 1))
        {
            more = channels & 0x0C;
            dac084s085_write( dac, DAC084S085_OUTB, (uint8_t)val, !more );
            vb = val;
        }
        else
        {
            vb = -1;
        }

        if (channels & (1 << 2))
        {
            more = channels & 0x08;
            dac084s085_write( dac, DAC084S085_OUTC, (uint8_t)val, !more );
            vc = val;
        }
        else
        {
            vc = -1;
        }

        if (channels & (1 << 3))
        {
            dac084s085_write( dac, DAC084S085_OUTD, (uint8_t)val, true );
            vd = val;
        }
        else
        {
            vd = -1;
        }

        if (va != va_prev || vb != vb_prev || vc != vc_prev || vd != vd_prev)
        {
            printf( "%4d: Write = %d %d %d %d\n", (int)(t / CLOCKS_PER_SEC), va, vb, vc, vd );
            va_prev = va;
            vb_prev = vb;
            vc_prev = vc;
            vd_prev = vd;
        }

        while( clock() < t ) __nop();

        val = val + direction;
        if (val > 255)
        {
            val = 255;
            direction = -1;
        }
        if (val < 0)
        {
            val = 0;
            direction = 1;
        }
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
    printf( "Opening DAC:  ");
    dac = dac084s085_open( DRV_DAC084S085_1 );
    puts ( dac ? "OK" : "Fail" );
    ioport = ioport_open( DRV_IOPORT_1 );
}

