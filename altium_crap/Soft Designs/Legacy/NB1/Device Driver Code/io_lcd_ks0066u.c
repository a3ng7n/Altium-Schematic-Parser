#include "io_lcd_ks0066u.h"
#include "lcd_ks0066u.h"

static unsigned char line_buffer[16] = "                ";
static int  buffer_offset = 0;
static int  current_line  = 0;
static int  scroll_needed = 0;
static int  column_count = 16;

void lcd_ks0066u_set_columns( unsigned int columns )
{
    column_count = columns;
}


void lcd_ks0066u_scroll ( unsigned int base )
{
    if (current_line == 1)
    {
        lcd_ks0066u_output_string(base, 0, line_buffer);
    }
    lcd_ks0066u_output_string(base, 1, "                ");
    lcd_ks0066u_goto_xy(base, 0, 1);
    current_line = 1;
    buffer_offset = 0;
    for (int i = 0; i < column_count; i++) line_buffer[i] = ' ';
}

void lcd_ks0066u_putchar( unsigned int base, char ch )
{
    if (scroll_needed)
    {
       lcd_ks0066u_scroll(base);
    }
    scroll_needed = 0;
    switch (ch)
    {
       case '\n' : scroll_needed = 1;
                   break;
       case '\f':   lcd_ks0066u_clear_screen(base);
                   break;
       case '\r':  lcd_ks0066u_goto_xy(base, 0, current_line);
                   buffer_offset = 0;
                   break;
       case '\b':  if (buffer_offset > 0)
                   {
                      buffer_offset--;
                      lcd_ks0066u_goto_xy(base, buffer_offset, current_line);
                   }
                   break;
       case '\t':  lcd_ks0066u_write_char(base, ' ');
                   lcd_ks0066u_write_char(base, ' ');
                   buffer_offset += 2;
                   break;
       default  :  if (buffer_offset > column_count) lcd_ks0066u_scroll(base);
                   lcd_ks0066u_write_char(base, ch );
                   line_buffer[buffer_offset] = ch;
                   buffer_offset++;
    }
}

int lcd_ks0066u_write( unsigned int base, const char * buf, int size )
{
    while ( size > 0 )
    {
       lcd_ks0066u_putchar(base, *buf);
       buf++;
       size--;
    }
    return size;
}
