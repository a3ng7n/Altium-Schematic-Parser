/*
 * TMR3 Dual Timer Unit example
 *      Timer A and B are configured in 8 bit autoreload mode.
 *      This application runs until both have triggered 100 times.
 *      The interval for one timer is configured twice as long as
 *      the other, so number of calls should be 100 and 200.
 */

#include <drv_tmr3.h>
#include <interrupts.h>

#define ERR_MAX 16
char *errors[ERR_MAX];
int num_errors;

static int num_calls_handler_a;
static int num_calls_handler_b;
static volatile bool hundred_a;
static volatile bool hundred_b;

void handler_timer_a(tmr3_t *drv, void *data)
{
    if (++num_calls_handler_a == 100)
    {
        hundred_a = true;
    }
}

void handler_timer_b(tmr3_t *drv, void *data)
{
    if (++num_calls_handler_b == 100)
    {
        hundred_b = true;
    }
}

int main(void)
{
    tmr3_t  *drv;
    int     err;

    interrupts_enable();

    drv = tmr3_open(0);

    if (drv != NULL)
    {
        err = tmr3_timer_a_set_handler(drv, handler_timer_a, NULL);
        if (err != 0)
        {
            errors[num_errors++] = "error tmr3 timer a set handler";
        }
        err = tmr3_timer_a_start(drv);
        if (err != 0)
        {
            errors[num_errors++] = "error tmr3 timer a start";
        }
        err = tmr3_timer_b_set_handler(drv, handler_timer_b, NULL);
        if (err != 0)
        {
            errors[num_errors++] = "error tmr3 timer b set handler";
        }
        err = tmr3_timer_b_start(drv);
        if (err != 0)
        {
            errors[num_errors++] = "error tmr3 timer b start";
        }

        while(!hundred_a || !hundred_b);

        err = tmr3_timer_a_stop(drv);
        if (err != 0)
        {
            errors[num_errors++] = "error tmr3 timer a stop";
        }
        err = tmr3_timer_b_stop(drv);
        if (err != 0)
        {
            errors[num_errors++] = "error tmr3 timer b stop";
        }
        err = tmr3_close(drv);
        if (err != 0)
        {
            errors[num_errors++] = "error tmr3 close";
        }
    }
    else
    {
        errors[num_errors++] = "error tmr3 open";
    }

    interrupts_disable();

    return 0;
}
