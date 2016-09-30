/********************************************************************\
|*
|* Version : 1.0
|*
|* Copyright : Copyright (C) 2006, Altium
|*
|* Description : Simple Leds Flasher example using the TSK3000A soft processor
|*
\********************************************************************/

#include "hardware.h"

#define IO_BASE(base) ((volatile unsigned char *) base)
#define IO_DIPS(base)     IO_BASE(base)[2]
#define IO_LEDS(base)     IO_BASE(base)[3]

#define SW    IO_DIPS(Base_IO)
#define LEDS  IO_LEDS(Base_IO)

#define US_COUNT  53
#define ONE_MS_COUNT  21

typedef enum
{
    blink,
    shift_right,
    shift_left
} mode;

/***********************************************************************
|*
|* Function : wait_100us
|*
|* Parameters : Time = delay * 100us
|*
|* Returns : none
|*
|* Description : Generates a delay in 100us units.
*/
void wait_100us( int Time )
{
    int i, j;
    for ( j = Time; j > 0; j-- )
    {
        for (i = US_COUNT; i > 0; i--)
        {
            __asm("nop");
        }
    }
}

/***********************************************************************
|*
|* Function : wait_1ms
|*
|* Parameters : Time = delay * 1ms
|*
|* Returns : none
|*
|* Description : Generates a delay in 1ms units.
*/
void wait_1ms( int Time )
{
    int i;
    for ( i = Time; i > 0; i-- )
    {
        wait_100us(ONE_MS_COUNT);
    }
}

/***********************************************************************
|*
|* Function : main
|*
|* Parameters : none
|*
|* Returns : none
|*
|* Description : Flashes leds
*/
void main ( void )
{
    unsigned char leds;

    leds = blink;
    while(1)
    {
        switch(SW)
        {
            case 0x01 : leds = blink       ; break;
            case 0x02 : leds = shift_right ; break;
            case 0x04 : leds = shift_left  ; break;
        }
        wait_1ms(20);

        switch(leds)
        {
            case blink       : if ( (LEDS != 0x00) & (LEDS != 0x0F) )
                                   LEDS = 0x0F;
                               else
                                   LEDS =~ LEDS;
                               break;
            case shift_right : if ( (LEDS == 0x00) | (LEDS == 0x0F) )
                                   LEDS = 0x80;
                               else
                                   LEDS >>= 1;
                               break;
            case shift_left  : if ( (LEDS == 0x00) | (LEDS == 0x0F) )
                                   LEDS = 0x01;
                               else
                                   LEDS <<= 1;
                               break;
        }
        wait_1ms(100);
    }

}
