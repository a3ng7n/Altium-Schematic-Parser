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
void vga_open                 (unsigned int base);
void vga_line                 (unsigned int x1, unsigned int y1, unsigned int x2, unsigned int y2, unsigned char color );
void vga_circle               (unsigned int x0, unsigned int y0, unsigned int r ,                  unsigned char color );
void vga_fill                 (unsigned int x1, unsigned int y1, unsigned int x2, unsigned int y2, unsigned char color );
void vga_fill_screen          (unsigned char color);
void vga_clear_screen         (void);
void TestPattern              (void);
void vga_SetVideoMode         (unsigned char mode);
void vga_SetStandardColorTable(void);
void vga_SetAddressForDrawing (unsigned int address);
void vga_SetAddressForDisplay (unsigned int address);
//..............................................................................

#endif
