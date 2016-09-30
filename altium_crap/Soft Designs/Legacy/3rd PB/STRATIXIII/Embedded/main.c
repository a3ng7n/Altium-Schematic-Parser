#include "chars.h"
#include "Common.h"

#include <timing.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <devices.h>
#include <drv_lcd.h>
#include <drv_lcd_util.h>


#define MSG_SIZE 128

typedef enum {eLeds,eLcd,eButtons, eSelect} mode_t;
mode_t mode = eSelect;

unsigned int count = 0;
unsigned long long start_tick;
unsigned int elapsed;

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

static bool button_check(void)
{
    LEDS = 0;

    lcd_output_string(display,0,"Press Button 1");
    while(!PUSH_BUTTON_1_DOWN)
    {
       LEDS = ~PUSH_BUTTONS;
    }

    lcd_output_string(display,0,"Press Button 2");
    while(!PUSH_BUTTON_2_DOWN)
    {
       LEDS = ~PUSH_BUTTONS;
    }

    lcd_output_string(display,0,"Press Button 3");
    while(!PUSH_BUTTON_3_DOWN)
    {
       LEDS = ~PUSH_BUTTONS;
    }

    lcd_output_string(display,0,"Press Button 4");
    while(!PUSH_BUTTON_4_DOWN)
    {
       LEDS = ~PUSH_BUTTONS;
    }


   LEDS = 0;
   return true;
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


void tsk3k_promo (void)
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
}

//..............................................................................
void KnightRider(unsigned int OnTimeMs, unsigned char count)
{
  _Bool direction = 0;
  register unsigned char pattern = 1;
  while(count--)
  {
    LEDS = pattern;              // set LED[0..7] to pattern

     delay_ms(OnTimeMs);
    if(0 == direction)  // move right
    {
       pattern <<= 1;
       if(0==pattern)
       {
          direction = 1;
          pattern = 0x40;
       }
    }
    else                // move left
    {
       pattern >>= 1;
       if(0==pattern)
       {
          direction = 0;
          pattern = 0x02;
       }
    }
  }
  LEDS = 0;               // turn all LEDs off when exiting
}
//..............................................................................


//..............................................................................
static void show_select_screen(void)
{
    lcd_clear_screen(display);
    switch (mode)
    {
      case eSelect:    lcd_output_string(display,0,"1-Leds 2-Lcd");
                       lcd_output_string(display,1,"3-Push Buttons");
                       delay_ms(2000);
                       break;

      case eLeds:      lcd_output_string(display,0,"Testing Leds...");
                       delay_ms(2000);
                       KnightRider(30,100);
                       mode=eSelect;
                       break;

      case eLcd:       lcd_output_string(display,0,"Testing LCD");
                       lcd_output_string(display,1,"TSK3k Promotion");
                       delay_ms(2000);
                       tsk3k_promo();
                       mode = eSelect;
                       break;

     case eButtons:    button_check();
                       mode = eSelect;
                       break;

    }

}
//..............................................................................


//..............................................................................
void main (void)
{

    setbuf( stdout, NULL ); // Disables line buffering on stdout
    setbuf( stdin, NULL );  // Disables line buffering on stdin

    init();
    lcd_set_cursor(display, 0, 0);
    start_tick = clock_ms();
    lcd_print("LCD Driver\nVersion 1.0");
    fflush(stdout);

    delay_ms(delay);
    lcd_clear_screen (display);
    lcd_output_string(display,0,"Stratix3");
    lcd_output_string(display,1,"Evaluation Board");

    delay_ms(delay);
    lcd_clear_screen(display);
    mode = eSelect;

    while (1)
    {
        lcd_output_string(display,0,"Select following");
        lcd_output_string(display,1,"to begin test:");
        delay_ms(delay);

        if (PUSH_BUTTON_1_DOWN) mode = eLeds;
        if (PUSH_BUTTON_2_DOWN) mode = eLcd;
        if (PUSH_BUTTON_3_DOWN) mode = eButtons;

        show_select_screen();
    }

}
//..............................................................................

