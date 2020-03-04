#include <stdint.h>
#include <stdbool.h>

static volatile uint8_t value;
__output bool LED0;                     // __output qualified variables appear as ports on the code symbol
__output bool LED1;                     // variables of type bool arwe 1-bit wide
__output bool LED2;
__output bool LED3;

void shot(void)
{
    for (value = 0; value < 5; ++value)
    {
        __wait(5000000);               // Wait given number of clock cycles
        LED0 = (value == 0);
        LED1 = (value == 1);
        LED2 = (value == 2);
        LED3 = (value == 3);
    }
}

