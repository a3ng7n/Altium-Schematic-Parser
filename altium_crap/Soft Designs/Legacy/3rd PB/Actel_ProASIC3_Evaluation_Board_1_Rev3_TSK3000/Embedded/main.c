#include "hardware.h"

void main()
{
    int j, i = 0;
    int wait_mult;
    while (1)
    {
        *((volatile unsigned char *) Base_LED) = (unsigned char) i++;
        wait_mult = (int) *((volatile unsigned char *) Base_LED);

        for (j = 0; j < (wait_mult + 1) * 10000; j++)
            ;
    }
}
