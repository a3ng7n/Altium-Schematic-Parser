/*
 * ICS307 SERIALLY PROGRAMMABLE CLOCK SOURCE
 *
 *  This example program runs at 20MHz (CLK_REF) and programs the ICS307 produce
 *  a CLK_BRD frequency form 6MHz to 200MHz. It uses the drivers best match routine
 *  to obtain the programming values for the ICS307. It will determine the time it
 *  takes for each best match calculation. And prints the statistics and programming
 *  values to the terminal instrument.
 */

#include <stdio.h>
#include <string.h>
#include <drv_ics307.h>
#include <timing.h>

char *errors[1024];
int num_errors;

int main(void)
{
    ics307_t *drv;
    unsigned int val = -1;
    unsigned int i, od = -1, vdw = -1, rdw = -1;
    uint64_t start, total, sum = 0;
    uint32_t usec, minimum = UINT32_MAX, maximum=0;
    int number_of_samples = 0;
    char buf[100];
    int write_cnt = 0;
    int err;

    total = clock_us();
    drv = drv_ics307_open(0);
    if (drv != NULL)
    {
        /* read the current configuration */
        if (drv_ics307_read_word(drv, &val) != 0)
        {
            errors[num_errors++] = "error drv_ics307_read_word()";
        }

        /* write a predetermined value */
        drv_ics307_program_word(drv, 0x220204);  // 10  Mhz from 20 Mhz

        for (i = 6; i <= 200; i++)
        {
            start = clock_us();
            err = drv_ics307_best_match(i * 1 * 1000 * 1000, &od, &vdw, &rdw);
            usec = (uint32_t)elapsed_time_us(start);
            sum += usec;
            number_of_samples++;
            if (usec < minimum)
            {
                minimum = usec;
            }
            if (usec > maximum)
            {
                maximum = usec;
            }
            if (err != 1)
            {
                errors[num_errors++] = "incorrect match";
            }
            else
            {
                /*
                 * write values to the uart8
                 */
                sprintf(buf, "clk %d, od %d, vwd %d rdw %d (%d usec)\n\r", i, od, vdw, rdw, (int)usec);
                printf("%s", buf);
            }
            drv_ics307_program(drv, 0x20 | od, vdw, rdw);
            delay_ms(2000);
        }
        sprintf(buf, "total %d usec\n\r", (int)elapsed_time_us(total));
        err = 0;
        printf("%s", buf);
        sprintf(buf, "average %d usec\n\r", (int)(sum/number_of_samples));
        printf("%s", buf);
        sprintf(buf, "minimum %d usec\n\r", (int)minimum);
        printf("%s", buf);
        sprintf(buf, "maximum %d usec\n\r", (int)maximum);
        printf("%s", buf);
    }

    return -1;
}


