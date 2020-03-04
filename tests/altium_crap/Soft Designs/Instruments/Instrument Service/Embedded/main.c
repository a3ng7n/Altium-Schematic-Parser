#include "devices.h"
#include "instruments.h"

#include <stdint.h>
#include <drv_instrument.h>
#include <drv_ioport.h>


#define MAX_COUNTER 65536
#define MAX_SPEED   256

/* Configuration data for IO Port and Custom Instrument drivers */
instrument_t * control;
ioport_t *     io;


/* The main function */
int main(void)
{
    uint32_t counter = 0;
    uint32_t speed = 0;
    uint32_t timer = 0;
    uint32_t value;
    uint32_t i;

    control = instrument_open(DRV_CONTROL);
    io = ioport_open(DRV_IO);

    while(1)
    {
        /* set the value of the dipswitch */
        value = ioport_get_value(io, 0);
        instrument_set_value(control, CONTROL_DIPSWITCH, value);

        /* set the value of the LEDs */
        value = instrument_get_value(control, CONTROL_LED);
        ioport_set_value(io, 0, value);

        /* read the speed setting from the instrument */
        speed = MAX_SPEED - instrument_get_value(control, CONTROL_SPEED);

        /* update the value of the counter, depending on the speed */
        timer = timer + 1;
        if ((timer % speed) == 0)
        {
            counter = (counter + 1) % MAX_COUNTER;
        }
        instrument_set_value(control, CONTROL_COUNTER, counter);

        for (i = 0; i < 500; ++i);
        {
            __nop();
        }
    }

    return 0;
}
