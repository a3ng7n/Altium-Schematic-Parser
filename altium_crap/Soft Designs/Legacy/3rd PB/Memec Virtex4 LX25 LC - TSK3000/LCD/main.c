/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Simple LCD example using the TSK3000A soft processor
|*
\*****************************************************************************/

#include "hardware.h"
#include "LcdControl.h"
#include "MsgPatterns.h"

#define IO_BASE(base) ((volatile unsigned char *) base)   
#define IO_DIPS(base)     IO_BASE(base)[2]
#define IO_LEDS(base)     IO_BASE(base)[3]

#define SW    IO_DIPS(Base_IO)
#define LEDS  IO_LEDS(Base_IO)

typedef enum
{
    blink,
    shift_right,
    shift_left
} mode;

void main (void)
{
    unsigned char leds;

    InitLCD();

    LCD_ClearDisplay();

    LCD_Set_IconPattern(0, 0);
    LCD_Set_IconPattern(1, 1);
    LCD_Set_IconPattern(2, 2);
    LCD_Set_IconPattern(3, 3);
    LCD_Set_IconPattern(4, 4);
    LCD_Set_IconPattern(5, 5);
    LCD_Set_IconPattern(6, 6);
    LCD_Set_IconPattern(7, 7);

    LCD_GotoXY(0, 0);
    Write_DR_LCD(0);
    Write_DR_LCD(1);
    Write_DR_LCD(2);
    Write_DR_LCD(3);
    LCD_WriteString(" Live Design");
    LCD_GotoXY(0, 1);
    Write_DR_LCD(4);
    Write_DR_LCD(5);
    Write_DR_LCD(6);
    Write_DR_LCD(7);
    LCD_WriteString("   enabled  ");

    leds = blink;
    while (1)
    {
        switch ( SW )
        {
            case 0x01 : leds = blink       ; break;
            case 0x02 : leds = shift_right ; break;
            case 0x04 : leds = shift_left  ; break;
        }
        Wait_1ms(20);

        switch ( leds )
        {
            case blink       : if ( (LEDS != 0x00) & (LEDS != 0x0F) )
                                   LEDS = 0x0F;
                               else
                                   LEDS =~ LEDS;
                               break;
            case shift_right : if ( (LEDS == 0x00) | (LEDS == 0x0F) )
                                   LEDS = 0x08;
                               else
                                   LEDS >>= 1;
                               break;
            case shift_left  : if ( (LEDS == 0x00) | (LEDS == 0x0F) )
                                   LEDS = 0x01;
                               else
                                   LEDS <<= 1;
                               break;
        }
        Wait_1ms(100);
    }

}
