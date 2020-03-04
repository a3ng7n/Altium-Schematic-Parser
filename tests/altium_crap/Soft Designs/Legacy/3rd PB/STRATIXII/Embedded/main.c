#include "hardware.h"

#define US_COUNT  495
#define ONE_MS_COUNT  10

#define IO_BASE(base) ((volatile unsigned char *) base)
#define IO_LEDS(base)     IO_BASE(base)[0]
#define IO_DIPS(base)     IO_BASE(base)[1]

#define SW    IO_DIPS(Base_LEDS)
#define LEDS  IO_LEDS(Base_LEDS)

void wait_100us(int Time)
{
    int i, j;
    for (j = Time; j > 0; j--)
    {
        for (i = US_COUNT; i > 0; i--)
        {
            __asm("nop");
        }
    }
}

void wait_1ms(int Time)
{
    int i;
    for (i = Time; i > 0; i--)
    {
        wait_100us(ONE_MS_COUNT);
    }
}

typedef enum
{
    blink,
    shift_right,
    shift_left
} mode;

void main(void)
{
    /*
    while(1)
    {
        LEDS = ~LEDS;
        //wait_100us(1);
        wait_1ms(1);
    }
    */

    static unsigned char leds;

    leds = blink;
    while (1)
    {
        switch ( SW )
        {
            case 0x01 : leds = blink       ; break;
            case 0x02 : leds = shift_right ; break;
            case 0x04 : leds = shift_left  ; break;
        }
        wait_1ms(100);

        switch ( leds )
        {
            case blink       : if ( (LEDS != 0x00) & (LEDS != 0xFF) )
                                   LEDS = 0xFF;
                               else
                                   LEDS =~ LEDS;
                               break;
            case shift_right : if ( (LEDS == 0x00) | (LEDS == 0xFF) )
                                   LEDS = 0x80;
                               else
                                   LEDS >>= 1;
                               break;
            case shift_left  : if ( (LEDS == 0x00) | (LEDS == 0xFF) )
                                   LEDS = 0x01;
                               else
                                   LEDS <<= 1;
                               break;
        }
        wait_1ms(100);
    }
}
