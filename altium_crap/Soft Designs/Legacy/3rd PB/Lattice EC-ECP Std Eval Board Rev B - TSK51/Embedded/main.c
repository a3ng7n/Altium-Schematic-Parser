#define US_COUNT  13
#define ONE_MS_COUNT  11

#define LEDS  P1
#define SW    P0

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
        wait_1ms(20);

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
