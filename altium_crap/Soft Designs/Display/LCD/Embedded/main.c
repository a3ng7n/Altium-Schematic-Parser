#include "chars.h"

#include <timing.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <devices.h>
#include <drv_lcd.h>
#include <drv_lcd_util.h>


#define MSG_SIZE 128

drv_lcd_t *display;

const uint32_t delay = 1500;

void init(void)
{
    uint8_t *cdata = chardata;

    display = lcd_open(DRV_LCD_1);

    for (uint32_t i = 0; i < charmax; i++)
    {
        lcd_set_custom_char(display, i, cdata + i * 8);
    }

}


void lcd_print(const char *msg)
{
    uint8_t size;

    for (size = 0; size < MSG_SIZE; size++)
    {
        if (msg[size] == '\0') break;
    }

    lcd_write(display, msg, size);
}


void main (void)
{
    char Msg[MSG_SIZE];
    uint32_t count = 0;
    static uint64_t start_tick;
    uint32_t elapsed;

    init();

    lcd_set_cursor(display, 0, 0);
    start_tick = clock_ms();
    lcd_print("LCD Driver\nVersion 1.0");

    delay_ms(delay);

    while (1)
    {
        lcd_print("\f\r-=Introducing=-\n");
        delay_ms(delay * 2);
        lcd_goto_xy(display, 0, 0);
        for (uint32_t i = 0; i < 16; i++)
        {
            lcd_write_char(display, ctop[i]);
        }
        lcd_goto_xy(display, 0, 1);
         for (uint32_t i = 0; i < 16; i++)
         {
            lcd_write_char(display, cbot[i]);
         }

       delay_ms(delay * 3);
       lcd_print("\fUsing printf() &\n");
       delay_ms(delay);
       lcd_print("I/O re-direction\n");
       delay_ms(delay);
       lcd_print("for easy output\n");
       delay_ms(delay);
       lcd_print("using format \n");
       delay_ms(delay);
       lcd_print("operators\n");
       delay_ms(delay);
       lcd_print("such as %%x, \n");
       delay_ms(delay);
       sprintf(Msg,"e.g. count: 0x%02x\n", count);
       lcd_print(Msg);
       delay_ms(delay);
       lcd_print("Time since reset\n");
       delay_ms(delay);
       elapsed = elapsed_time_ms(start_tick);
       sprintf(Msg, "%02d days\n", elapsed / ((60 * 60 * 1000 * 24)));
       lcd_print(Msg);
       delay_ms(delay);
       sprintf(Msg, "%02d hours\n", (elapsed / (60 * 60 * 1000)) % 24);
       lcd_print(Msg);
       delay_ms(delay);
       sprintf(Msg, "%02d minutes\n", (elapsed / (60 * 1000)) % 60);
       lcd_print(Msg);
       delay_ms(delay);
       sprintf(Msg, "%02d seconds\n", (elapsed / 1000) % 60);
       lcd_print(Msg);
       delay_ms(delay);
       lcd_print("Form Feed will\nclear the LCD\n");
       delay_ms(delay);
       lcd_print("\f\n");
       lcd_print("3 Backspaces:");
       delay_ms(delay);
       lcd_print("\b\b\bHere.\n");
       delay_ms(delay);
       lcd_print("\rCarriage Return.\n");
       delay_ms(delay);
       lcd_print("\t\tTwo Tabs\n");
       lcd_print("\t\t^\n");
       delay_ms(delay);
       lcd_print("\f\n");
       lcd_shift(display, -16);
       lcd_print("Shift Left\n     Shift Left\n");
       for(uint32_t i = 0 ; i < 32 ; i++ )
       {
          lcd_shift(display, 1 );
          delay_ms(200);
       }
       delay_ms(delay);
       lcd_print("Shift Right\n     Shift Right\n");
       for(uint32_t i = 0 ; i < 32 ; i++ )
       {
          lcd_shift(display, -1 );
          delay_ms(200);
       }
       lcd_shift(display, 16 );
       count++;
       count &= 0xFF;
    }

 while(1);
}


