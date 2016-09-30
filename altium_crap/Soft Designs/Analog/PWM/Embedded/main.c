/********************************************************************\
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    Demonstrate the use of the PWMX driver with an
|*                  RC servo device.
|*
\********************************************************************/

#include <timing.h>
#include <drv_pwmx.h>
#include "devices.h"

int main(void)
{
    int direction;
    unsigned short pwmrg, begin, end, delay;
    int period_ms = 15;
    volatile int forever = 1;

    pwmx_t * driver;

    driver = pwmx_open(DRV_PWMX_1);

    pwmx_set_frequency(driver, 1000 / period_ms);

    switch (pwmx_get_resolution_mode(driver))
    {
        case PWMX_MODE_8BIT:
             delay = 128;
             break;
        case PWMX_MODE_10BIT:
             delay = 32;
             break;
        case PWMX_MODE_12BIT:
             delay = 8;
             break;
        case PWMX_MODE_14BIT:
             delay = 2;
             break;
        default:
             delay = 128;
             break;
    }

    pwmx_enable_controller(driver);

    direction = 1;
    begin = 800 * 1000 / (period_ms * 1000 * 1000 / pwmx_max_pulsewidth(driver));
    end = 2000 * 1000 / (period_ms * 1000 *  1000 / pwmx_max_pulsewidth(driver));
    pwmrg = begin;
    /* sweep to right and back to left in loop */
    /* for rc servo this is between 1 and 2 ms */
    do
    {
        pwmx_set_pulsewidth(driver, pwmrg);
        delay_ms(delay);
        pwmrg += direction;
        if (pwmrg <= begin)
        {
            direction = 1;
        }
        if (pwmrg >= end)
        {
            direction = -1;
        }
    } while (forever);

    pwmx_disable_controller(driver);

    return 0;
}
