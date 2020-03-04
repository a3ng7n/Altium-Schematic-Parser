/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   VDU 80x25 Character display core device driver
|*
\*****************************************************************************/

//..............................................................................
//#include <string.h>
#include "wb_vdu.h"
//..............................................................................

//..............................................................................
unsigned int  vdu_base_address  = 0x00;
unsigned char vdu_attributes    = 0x0F;       //Default to high intensity white with black background
//..............................................................................

//..............................................................................
void vdu_open ( unsigned int base )
{
    vdu_base_address = base;
}
//..............................................................................

//..............................................................................
unsigned char get_attribute_byte(unsigned char foreground_color,
                                 unsigned char background_color,
                                 unsigned char blink)
{
    unsigned char attribute = 0;

    if (blink) attribute = attribute | (1 << 7);

    attribute = attribute | ((background_color & 0x07) << 4);
    attribute = attribute | ((foreground_color & 0x0F)     );

    return attribute;
}
//..............................................................................

//..............................................................................
void vdu_set_colors(unsigned char foreground_color,
                    unsigned char background_color)
{
   vdu_attributes = get_attribute_byte(foreground_color,background_color,vdu_get_blink());
}
//..............................................................................

//..............................................................................
unsigned char vdu_get_blink( void )
{
    return (vdu_attributes >> 7);
}
//..............................................................................

//..............................................................................
unsigned char vdu_set_blink( unsigned char blink)
{
    unsigned char old_blink = vdu_get_blink();

    if (blink)
       vdu_attributes = vdu_attributes | 0x80;
    else
       vdu_attributes = vdu_attributes & 0x7f;

    return old_blink;
}
//..............................................................................

//..............................................................................
unsigned char vdu_get_background(void)
{
   return ((vdu_attributes >> 4) & 0x07);
}
//..............................................................................

//..............................................................................
unsigned char vdu_set_background(unsigned char color)
{
   unsigned char old_color = vdu_get_background();
   vdu_attributes = (vdu_attributes & 0x8F) | ((color & 0x07) << 4);
   return old_color;
}
//..............................................................................

//..............................................................................
unsigned char vdu_get_foreground(void)
{
   return (vdu_attributes & 0x0F);
}
//..............................................................................

//..............................................................................
unsigned char vdu_set_foreground(unsigned char color)
{
   unsigned char old_color = vdu_get_foreground();
   vdu_attributes = (vdu_attributes & 0xF0) | (color & 0x0F);
   return old_color;
}
//..............................................................................

//..............................................................................
void vdu_set(unsigned char x,
             unsigned char y,
             unsigned char chr)
{
   unsigned char* pointer;
   unsigned int  offset;

   offset = ((y * VDU_WIDTH) + x) * 2;

   pointer  = (unsigned char*) (vdu_base_address + offset);
   *pointer = chr;

   pointer++;
   *pointer = vdu_attributes;
}
//..............................................................................

//..............................................................................
unsigned char vdu_get(unsigned char x,
                      unsigned char y)
{
   unsigned char* pointer;
   unsigned int  offset;

   offset = ((y * VDU_WIDTH) + x) * 2;

   pointer  = (unsigned char*) (vdu_base_address + offset);
   return *pointer;
}
//..............................................................................

//..............................................................................
void vdu_fill_screen(unsigned char foreground_color, unsigned char background_color)
{
   unsigned char x,y;
   unsigned char attributes = vdu_attributes;

   vdu_set_colors(foreground_color,background_color);
   vdu_set_blink (0);

   for ( y = 0; y < VDU_HEIGHT; y++)
      for ( x = 0; x < VDU_WIDTH; x++)
   {
      vdu_set(x,y,' ');
   }

   vdu_attributes = attributes;
}
//..............................................................................

//..............................................................................
void vdu_clear_screen(void)
{
    vdu_fill_screen(0,0);
}
//..............................................................................

//..............................................................................
void vdu_test_pattern(void)
{
   unsigned char attributes = 0;
   unsigned char character  = 0;
   unsigned char x,y;

   attributes = vdu_attributes;
   vdu_attributes = 0;

   for ( y = 0; y < VDU_HEIGHT; y++)
   {
      character = 'A';
      for ( x = 0; x < VDU_WIDTH; x++)
      {
         vdu_set(x,y,character);
         vdu_attributes++;
         character++;
      }
   }

   vdu_attributes = attributes;
}
//..............................................................................

//..............................................................................
int  vdu_get_height( void )
{
    return 25;
}
//..............................................................................

//..............................................................................
int  vdu_get_width( void )
{
    return 80;
}
//..............................................................................

//..............................................................................
// Scroll the screen up by a number of lines.
// Fill the new lines at the boottom with blanks.
// lines represents the number of lines to scroll - ie the offset.
void vdu_vertical_scroll(unsigned char lines)
{
   unsigned int MoveSize;
   unsigned int MoveTarget;
   unsigned int MoveSource;
   unsigned int FillSize;
   unsigned int FillTarget;

   MoveSize   = (VDU_HEIGHT - lines) * BYTES_PER_LINE;  //15 for a ten line scroll
   MoveTarget = vdu_base_address;
   MoveSource = MoveTarget + (lines * BYTES_PER_LINE);
   FillSize   = lines * BYTES_PER_LINE;
   FillTarget = vdu_base_address + ((VDU_HEIGHT - lines) * BYTES_PER_LINE);

   memcpy(MoveTarget,MoveSource,MoveSize);
   memset(FillTarget,0,FillSize);
}

/*
For a 10 line scroll:

   MoveSize   = 15 * BYTES_PER_LINE;  //15 for a ten line scroll
   MoveTarget = vdu_base_address;
   MoveSource = MoveTarget + (10 * BYTES_PER_LINE);

   memcpy(MoveTarget,MoveSource,MoveSize);
*/
