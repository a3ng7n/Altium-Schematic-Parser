
/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Example of lcd_ks0066u driver use
|*
\*****************************************************************************/

#include <stdio.h>
#include "util_timing.h"
#include "hardware.h"
#include "io_lcd_ks0066u.h"
#include "lcd_ks0066u.h"
#include "proc_bluestreak_arm7_startup.h"

// _write() is used to re-direct stdio to your device. In this case, the device
// is the Samsung KS0066U (Hitachi HD44780A Compatible) LCD Controller Chip.
int _write ( int fd, const char * buf, int size )
{
    return lcd_ks0066u_write(Base_LCD, buf, size);
}

void initialize ( void )
{
    timing_set_clock_freq_hz ( 51.6096 * 1000 * 1000);
    init_clock();
    lcd_ks0066u_init (Base_LCD);
    lcd_ks0066u_clear_screen (Base_LCD);
}

const unsigned int delay = 1500;

void set_custom_chars(const unsigned int base, unsigned char *cdata )
{
    for (int i = 0 ; i < 8 ; i++ )
    {
       lcd_ks0066u_set_custom_char( base, i, cdata + i * 8 );
    }
}


void main (void)
{
    unsigned int count = 0;
    unsigned long long start_tick;
    unsigned int elapsed;

    char ctop[] = {
       0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x00, 0x01,
       0x02, 0x03, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20 };
    char cbot[] = {
       0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x04, 0x05,
       0x06, 0x07, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20 };

    unsigned char data[] = {                                //ARM7
    0x00, 0x00, 0x00, 0x06, 0x06, 0x0F, 0x0D, 0x0D,
    0x00, 0x00, 0x00, 0x1E, 0x1F, 0x13, 0x13, 0x1E,
    0x00, 0x00, 0x00, 0x11, 0x19, 0x1B, 0x1F, 0x1F,
    0x00, 0x00, 0x00, 0x1F, 0x1F, 0x03, 0x06, 0x0E,
    0x0F, 0x0F, 0x19, 0x19, 0x00, 0x00, 0x00, 0x00,
    0x1E, 0x1B, 0x1B, 0x1B, 0x00, 0x00, 0x00, 0x00,
    0x15, 0x15, 0x11, 0x11, 0x00, 0x00, 0x00, 0x00,
    0x0C, 0x1C, 0x18, 0x18, 0x00, 0x00, 0x00, 0x00
    };

    startup();
    initialize();

    set_custom_chars( Base_LCD, data );
    lcd_ks0066u_set_cursor(Base_LCD, 0, 0);
    start_tick = timing_get_tick_count();
    printf("LCD Driver\nVersion 1.0");
    fflush(stdout);
    timing_delay_ms(delay);

    // Main Program Loop
    while (1)
    {
       printf("\f\r-=Introducing=-\n");
       printf("\rDiscrete ARM\n");
       timing_delay_ms(delay * 2);
       lcd_ks0066u_goto_xy( Base_LCD, 0, 0 );
       for( int i = 0 ; i < 16 ; i++ )
       {
          lcd_ks0066u_write_char( Base_LCD, ctop[i] );
       }
       lcd_ks0066u_goto_xy( Base_LCD, 0, 1 );
       for( int i = 0 ; i < 16 ; i++ )
       {
          lcd_ks0066u_write_char( Base_LCD, cbot[i] );
       }

       timing_delay_ms(delay * 3);
       printf("\fUsing printf() &\n");
       timing_delay_ms(delay);
       printf("I/O re-direction\n");
       timing_delay_ms(delay);
       printf("for easy output\n");
       timing_delay_ms(delay);
       printf("using format \n");
       timing_delay_ms(delay);
       printf("operators\n");
       timing_delay_ms(delay);
       printf("such as %%x, \n");
       timing_delay_ms(delay);
       printf("e.g. count: 0x%02x\n", count);
       timing_delay_ms(delay);
       printf("Time since reset\n");
       timing_delay_ms(delay);
       elapsed = timing_elapsed_time_ms(start_tick);
       printf("%02d days\n", elapsed / ((60 * 60 * 1000 * 24)));
       timing_delay_ms(delay);
       printf("%02d hours\n", (elapsed / (60 * 60 * 1000)) % 24);
       timing_delay_ms(delay);
       printf("%02d minutes\n", (elapsed / (60 * 1000)) % 60);
       timing_delay_ms(delay);
       printf("%02d seconds\n", (elapsed / 1000) % 60);
       timing_delay_ms(delay);
       printf("Form Feed will\nclear the LCD\n");
       timing_delay_ms(delay);
       printf("\f\n");
       printf("3 Backspaces:");
       timing_delay_ms(delay);
       printf("\b\b\bHere.\n");
       timing_delay_ms(delay);
       printf("\rCarriage Return.\n");
       timing_delay_ms(delay);
       printf("\t\tTwo Tabs\n");
       printf("\t\t^\n");
       timing_delay_ms(delay);
       printf("\f\n");
       lcd_ks0066u_shift( Base_LCD, -16 );
       printf("Shift Left\n     Shift Left\n");
       for( int i = 0 ; i < 32 ; i++ )
       {
          lcd_ks0066u_shift( Base_LCD, 1 );
          timing_delay_ms(200);
       }
       timing_delay_ms(delay);
       printf("Shift Right\n     Shift Right\n");
       for( int i = 0 ; i < 32 ; i++ )
       {
          lcd_ks0066u_shift( Base_LCD, -1 );
          timing_delay_ms(200);
       }
       lcd_ks0066u_shift( Base_LCD, 16 );
       count++;
       count &= 0xFF;
      }

}


