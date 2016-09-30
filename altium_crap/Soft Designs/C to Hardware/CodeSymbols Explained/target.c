#include <stdint.h>
#include <stdbool.h>

static volatile uint8_t value;
__output bool LED0;
__output bool LED1;
__output bool LED2;
__output bool LED3;

void target(void)
{
    __debug_printf("started!\n");
    value = 0;
    LED0 = 0;
    LED1 = 0;
    LED2 = 0;
    LED3 = 0;

    while(true)
    {
        __debug_printf("next pos\n");
        __wait(5000000);
        LED0 = (value == 0);
        LED1 = (value == 1);
        LED2 = (value == 2);
        LED3 = (value == 3);
        value = (value + 1) % 4;
    }
}

