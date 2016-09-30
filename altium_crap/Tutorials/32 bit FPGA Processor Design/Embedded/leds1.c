#include <stdint.h>
#include "hardware.h"

volatile uint8_t * const leds = (void *)Base_GPIO;

void main( void )
{
    *leds = 0;  // Initialize the LEDs to all OFF
    for ( ;; )
    {
        (*leds)++;
        for ( int delay = 0; delay < 5000000; delay++ )
        {
            __nop();  // Two underscores
        }
    }
}












