/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   VDU 80x25 Character display core device driver
|*
\*****************************************************************************/

#ifndef __WB_VDU_H__
#define __WB_VDU_H__

//..............................................................................
#define  VDU_WIDTH      80
#define  VDU_HEIGHT     25
#define  BYTES_PER_LINE (VDU_WIDTH * 2)
//..............................................................................

//..............................................................................
void          vdu_open            ( unsigned int  base);
void          vdu_fill_screen     ( unsigned char foreground_color, unsigned char background_color);
void          vdu_clear_screen    ( void );
//..............................................................................

//..............................................................................
void          vdu_set             ( unsigned char x, unsigned char y, unsigned char chr);
unsigned char vdu_get             ( unsigned char x, unsigned char y);
//..............................................................................

//..............................................................................
void          vdu_set_attributes  ( unsigned char foreground_color, unsigned char background_color, unsigned char blink);

unsigned char vdu_get_blink       ( void                );
unsigned char vdu_set_blink       ( unsigned char blink );

unsigned char vdu_get_background  ( void                );
unsigned char vdu_set_background  ( unsigned char color );

unsigned char vdu_get_foreground  ( void                );
unsigned char vdu_set_foreground  ( unsigned char color );
//..............................................................................

//..............................................................................
void          vdu_test_pattern    ( void );
void          vdu_vertical_scroll ( unsigned char lines);
int           vdu_get_height      ( void );
int           vdu_get_width       ( void );
//..............................................................................

#endif
