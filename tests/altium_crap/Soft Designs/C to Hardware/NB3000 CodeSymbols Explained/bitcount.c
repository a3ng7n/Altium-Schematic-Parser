#include <stdint.h>
#include <stdbool.h>


uint32_t countbits(uint32_t DAT, uint1_t ADR, uint4_t SEL, uint1_t WE)
{
    static uint32_t     n;

    // To enable debugging check the "Enable Printf-Style Debugging" checkbox on the
    // "Options" pane of the "C Code Symbol" dialog and rebuild the project.
    // Debug logic is not generated unless this option is checked.

    __debug_printf("countbits: DAT=%d ADR=%d SEL=%d WE=%d\n", DAT, ADR, SEL, WE);

    if (WE)
    {
        n = DAT;
        unsigned int i;
        unsigned int masks[] = {0x55555555, 0x33333333, 0x0f0f0f0f, 0x00ff00ff, 0x0000ffff};

        for (i = 0; i < 5; ++i)
        {
            n = ((n & ~masks[i]) >> (1<<i)) + (n & masks[i]);
        }
        return 0;
    }
    else
    {
        return n;
    }
}
