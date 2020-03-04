/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   KS0066U LCD device driver
|*
\*****************************************************************************/

#include "util_timing.h"
#include "lcd_ks0066u.h"

#define LCD_BASE(base) ((volatile unsigned char *) base)
#define LCD_CTRL(base) LCD_BASE(base)[0]
#define LCD_DATA(base) LCD_BASE(base)[1]

//..............................................................................
static unsigned int column_count = 16;
//..............................................................................

//..............................................................................
void lcd_ks0066u_wait_while_busy(unsigned int base)
{
  while(LCD_CTRL(base) & 0x80);
}

void lcd_ks0066u_write_ctrl(unsigned int base, unsigned char c)
{
  lcd_ks0066u_wait_while_busy(base);
  LCD_CTRL(base) = c;
}

//..............................................................................
// initialises the LCD controller
void lcd_ks0066u_init(unsigned int base)
{
  timing_delay_ms(20);                               // wait for more than 15ms after powerup

  lcd_ks0066u_write_ctrl     (base,0x38);           // Function set 8 Bit
  timing_delay_ms(8);                               // wait for another 8 ms

  lcd_ks0066u_write_ctrl     (base,0x38);           // Function set 8 Bit
  timing_delay_ms(2);                               // wait for another 2 ms

  lcd_ks0066u_write_ctrl     (base,0x38);           // Function set 8 Bit
  timing_delay_us(200);

  lcd_ks0066u_write_ctrl     (base,0x38);           // Function set 8 Bit, 2 lines, 5X7 dots
  timing_delay_us(200);

  lcd_ks0066u_write_ctrl     (base,0x06);           // Display Off, Cursor off, Blink off
  timing_delay_us(200);

  lcd_ks0066u_write_ctrl     (base,0x0E);           // Display on, Cursor On

  lcd_ks0066u_write_ctrl     (base,0x01);           // Clear Display
  timing_delay_us(1640);
}

void lcd_ks0066u_set_backlight(unsigned int base, unsigned char On)
{
  //LCD_BACKLIGHT = On;
}

unsigned char lcd_ks0066u_get_backlight(unsigned int base)
{
  return 0;//LCD_BACKLIGHT;
}

void lcd_ks0066u_clear_screen(unsigned int base)
{
  lcd_ks0066u_wait_while_busy(base);
  LCD_CTRL(base) = 0x01;
}

//..............................................................................
// controls Cursor appearance
// Visible: 0     = no cursor
//          non-0 = cursor visible
// Blink  : 0     = don't blink
//          non-0 = cursor blinks
void lcd_ks0066u_set_cursor(unsigned int base,
                            unsigned int visible,
                            unsigned int blink)
{
  lcd_ks0066u_wait_while_busy(base);
  unsigned char data = 0x0C; // display on
  if(visible) data |= 0x02;
  if(blink)   data |= 0x01;
  LCD_CTRL(base) = data;
}

void lcd_ks0066u_goto_xy(unsigned int base,
                         unsigned int x,
                         unsigned int y)
{
  lcd_ks0066u_wait_while_busy(base);
  unsigned char c;
  c = 0x80 | (x + 0x40* y);
  LCD_CTRL(base) = c;
}

unsigned char lcd_ks0066u_get_addr(unsigned int base)
{
    lcd_ks0066u_wait_while_busy(base);
    return LCD_CTRL(base) & 0x7F;
}

void lcd_ks0066u_get_xy(unsigned int base,
                        unsigned int *x,
                        unsigned int *y)
{

    lcd_ks0066u_wait_while_busy(base);
    unsigned char addr;
    addr = LCD_CTRL(base);
    *x = addr & 0x0F;
    *y = (addr & 0x70) >> 6;
}

void lcd_ks0066u_write_char(unsigned int  base,
                            unsigned char c)
{
  lcd_ks0066u_wait_while_busy(base);
  LCD_DATA(base) = c;
}

void lcd_ks0066u_set_custom_char(unsigned int   base,
                                 unsigned int   charno,
                                 unsigned char *data)
{
  unsigned char addr, i;
  if(charno > 7) return;
  addr = 0x40 + (8*charno);
  lcd_ks0066u_wait_while_busy(base);
  LCD_CTRL(base) = addr;
  for(i=0;i<8;i++)
  {
    lcd_ks0066u_wait_while_busy(base);
    LCD_DATA(base) = *data++;
  }
  lcd_ks0066u_wait_while_busy(base);
  lcd_ks0066u_goto_xy(base,0,0);  // set DD ram Address again
}

void lcd_ks0066u_output_string(unsigned int  base,
                               unsigned int  line,
                               unsigned char * msg)
{
   lcd_ks0066u_goto_xy( base, 0, line );
   unsigned char c;
   for (int i = 0; (i < 16) && (c = *msg++ ); i++)
   {
      lcd_ks0066u_write_char(base,c) ;
   }
}

void lcd_ks0066u_shift( unsigned int base, int places)
{
    int i;
    if ( places > 0 )
    {
       for ( i = 0 ; i < places ; i++ )
       {
          lcd_ks0066u_write_ctrl( base, 0x18 );
       }
    }
    if ( places < 0 )
    {
       for ( i = places ; i < 0; i++ )
       {
          lcd_ks0066u_write_ctrl( base, 0x1C );
       }
    }
}
