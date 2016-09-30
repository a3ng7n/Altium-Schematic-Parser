/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Simple routines to drive the LCD
|*
\*****************************************************************************/

#include "hardware.h"
#include "MsgPatterns.h"

#define US_COUNT      497
#define ONE_MS_COUNT  10

#define IO_BASE(base) ((volatile unsigned char *) base)
#define IO_LCD_CTRL(base) IO_BASE(base)[0]
#define IO_LCD_DATA(base) IO_BASE(base)[1]

#define IO_LCD_RS_HIGH(base) (IO_LCD_CTRL(base) = (IO_LCD_CTRL(base) | 0x01))
#define IO_LCD_RS_LOW(base)  (IO_LCD_CTRL(base) = (IO_LCD_CTRL(base) & 0xFE))
#define IO_LCD_E_HIGH(base)  (IO_LCD_CTRL(base) = (IO_LCD_CTRL(base) | 0x02))
#define IO_LCD_E_LOW(base)   (IO_LCD_CTRL(base) = (IO_LCD_CTRL(base) & 0xFD))
#define IO_LCD_T_HIGH(base)  (IO_LCD_CTRL(base) = (IO_LCD_CTRL(base) | 0x80))
#define IO_LCD_T_LOW(base)   (IO_LCD_CTRL(base) = (IO_LCD_CTRL(base) & 0x7F))

#define LCD_Putch(ch) Write_DR_LCD(ch)
#define LCD_Set_CGRAM_Address(Address) Wait_1ms(3); Write_IR_LCD((Address & 0x3F) | 0x40)
#define LCD_Set_DDRAM_Address(Address) Wait_1ms(3); Write_IR_LCD(Address | 0x80)

void Wait_100us(int Time)
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

void Wait_1ms(int Time)
{
    int i;
    for (i = Time; i > 0; i--)
    {
        Wait_100us(ONE_MS_COUNT);
    }
}

void inline Write_LCD(unsigned char Data)
{
    IO_LCD_DATA(Base_IO) = Data;
    IO_LCD_E_HIGH(Base_IO);
    Wait_100us(1);
    IO_LCD_E_LOW(Base_IO);
}

void inline Write_IR_LCD(unsigned char Data)
{
    IO_LCD_RS_LOW(Base_IO);
    Wait_100us(1);
    Write_LCD(Data);
}

void Write_DR_LCD(unsigned char Data)
{
    IO_LCD_RS_HIGH(Base_IO);
    Wait_1ms(2);
    Write_LCD(Data);
}

void InitLCD(void)
{
    IO_LCD_E_LOW(Base_IO);
    IO_LCD_RS_LOW(Base_IO);
    IO_LCD_DATA(Base_IO) = 0x00;

    Wait_1ms(40);         // wait for more than 15ms after powerup
    Write_IR_LCD(0x38);
    Wait_1ms(8);          // wait for another 8 ms
    Write_IR_LCD(0x38);
    Wait_100us(1);        // Wait more than 100us
    Write_IR_LCD(0x38);   // Function Set, 8-bit, 2 line
    Wait_1ms(1);
    Write_IR_LCD(0x01);   //Clear Display;
    Wait_1ms(3);
    Write_IR_LCD(0x0E);   // Display on, Cursor On
    Wait_1ms(3);
    Write_IR_LCD(0x06);   //Increment address counter and move cursor by 1
    Wait_1ms(3);
}

void LCD_GotoXY(unsigned char X, unsigned char Y)
{
    Wait_1ms(2);
    Write_IR_LCD(0x80 | (X + 0x40* Y));
}

void LCD_WriteString(const char * str)
{
    while (* str)
    {
        LCD_Putch(* str++);
    }
}

void LCD_ClearDisplay(void)
{
    Wait_1ms(3);
    Write_IR_LCD(0x01);
}

/*The following routine sets one of 8 available Icons (contained in CGRAM) to a pattern defined by Patterns[][] */
void LCD_Set_IconPattern(int Icon, int Pattern)
{                             
    int idx = Pattern << 3;
    LCD_Set_CGRAM_Address(Icon << 3);
    for (int i = 0; i < 8; i++)
    {
        Write_DR_LCD(Patterns[idx++]);
    }
}
