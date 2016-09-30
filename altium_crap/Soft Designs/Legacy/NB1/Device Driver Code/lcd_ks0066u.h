/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   KS0066U LCD device driver
|*
\*****************************************************************************/

#ifndef __lcd_ks0066u_h__
#define __lcd_ks0066u_h__

void          lcd_ks0066u_init            ( unsigned int base );
void          lcd_ks0066u_wait_while_busy ( unsigned int base );
void          lcd_ks0066u_write_ctrl      ( unsigned int base, unsigned char c );
void          lcd_ks0066u_clear_screen    ( unsigned int base );
void          lcd_ks0066u_set_cursor      ( unsigned int base, unsigned int visible, unsigned int blink );
void          lcd_ks0066u_goto_xy         ( unsigned int base, unsigned int x, unsigned int y );
unsigned char lcd_ks0066u_get_addr        ( unsigned int base );
void          lcd_ks0066u_get_xy          ( unsigned int base, unsigned int *x, unsigned int *y);
void          lcd_ks0066u_write_char      ( unsigned int base, unsigned char c );
void          lcd_ks0066u_output_string   ( unsigned int base, unsigned int line, unsigned char * msg );
void          lcd_ks0066u_shift           ( unsigned int base, int places);

//..............................................................................
// Program custom character bitpattern
// Up to 8 custom characters allowed.
//
// Parameters:
//    charno - Custom character identifier, can be from 0 to 7
//    data   - Array of 8 characters. Only bits 5..0 are significant
//             A set bit indicates dark, cleared light
//             Top row is first byte (*data)
//             LSB is on right
void lcd_ks0066u_set_custom_char ( unsigned int  base, unsigned int charno, unsigned char * data );

int lcd_ks0066u_write( unsigned int base, const char * buf, int size );

#endif

