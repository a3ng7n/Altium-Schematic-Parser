/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   KS0066U LCD device driver IO Redirection wrapper
|*
\*****************************************************************************/

#ifndef __IO_LCD_KS0066U__
#define __IO_LCD_KS0066U__

// Function Declarations
void lcd_ks0066u_set_columns( unsigned int columns );
void lcd_ks0066u_scroll ( unsigned int base );
void lcd_ks0066u_putchar( unsigned int base, char ch );
int lcd_ks0066u_write( unsigned int base, const char * buf, int size );

#endif