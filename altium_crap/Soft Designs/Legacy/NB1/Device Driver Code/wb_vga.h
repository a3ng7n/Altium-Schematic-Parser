/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   VGA Controller core device driver
|*
\*****************************************************************************/

#ifndef __WB_VGA_H__
#define __WB_VGA_H__

//..............................................................................
enum
{
  VideoMode_800x600_bpp8 = 0,
  VideoMode_800x600_bpp4,
  VideoMode_800x600_bpp2,
  VideoMode_800x600_bpp1,

  VideoMode_640x480_bpp8,
  VideoMode_640x480_bpp4,
  VideoMode_640x480_bpp2,
  VideoMode_640x480_bpp1
};
//..............................................................................

//..............................................................................
// Colors in these functions are indexes into the color palette
void          vga_plot                      ( unsigned int x,  unsigned int y,  unsigned char color );
void          vga_open                      ( unsigned int base);
void          vga_line                      ( unsigned int x1, unsigned int y1, unsigned int x2, unsigned int y2, unsigned char color );
void          vga_rect                      ( unsigned int x1, unsigned int y1, unsigned int x2, unsigned int y2, unsigned char color );
void          vga_circle                    ( unsigned int x0, unsigned int y0, unsigned int r ,                  unsigned char color );
void          vga_fill                      ( unsigned int x1, unsigned int y1, unsigned int x2, unsigned int y2, unsigned char color );
void          vga_fill_screen               ( unsigned char color );
void          vga_clear_screen              ( void );
void          vga_draw_bitmask              ( unsigned int x0, unsigned int y0, unsigned int width, unsigned int height,
                                              unsigned char * data, unsigned char back_color, unsigned char fore_color,
                                              unsigned int transparent );
void          vga_set_video_mode            ( unsigned char mode );
unsigned char vga_get_video_mode            ( void );
//..............................................................................
// rgb is a 32 bit integer in the format 00RRGGBB
void         vga_set_palette_entry         ( unsigned int rgb, unsigned int index );
unsigned int vga_get_palette_entry         ( unsigned int index );
//..............................................................................

//..............................................................................
void         vga_test_pattern              ( void );
void         vga_set_default_palette       ( void );
//..............................................................................

//..............................................................................
void         vga_set_address_for_drawing   ( unsigned int address );
unsigned int vga_get_address_for_drawing   ( void );
void         vga_set_address_for_display   ( unsigned int address );
unsigned int vga_get_address_for_display   ( void );
//..............................................................................

//..............................................................................
int          vga_get_video_height          ( void );
int          vga_get_video_width           ( void );
int          vga_get_bpp                   ( void );
//..............................................................................

#endif
