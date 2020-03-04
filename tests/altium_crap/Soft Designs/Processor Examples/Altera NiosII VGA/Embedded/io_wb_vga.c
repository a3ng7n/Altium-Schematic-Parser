/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   VGA Text routines
|*
\*****************************************************************************/

//..............................................................................
#include <string.h>
#include "wb_vga.h"
#include "io_wb_vga.h"
#include "courier_new_8.h"
//..............................................................................

//..............................................................................
unsigned int  CaretPosX = 0;
unsigned int  CaretPosY = 0;
unsigned char BackColor = 0;
unsigned char ForeColor = 255;
vga_scroll_mode_t ScrollMode = vsm_wrap;
//..............................................................................

//..............................................................................
unsigned int vga_get_caret_pos_x ( void )
{
    return CaretPosX;
}
//..............................................................................

//..............................................................................
void         vga_set_caret_pos_x ( unsigned int x)
{
    CaretPosX = x;
}
//..............................................................................

//..............................................................................
unsigned int vga_get_caret_pos_y ( void )
{
    return CaretPosY;
}
//..............................................................................

//..............................................................................
void         vga_set_caret_pos_y ( unsigned int y)
{
    CaretPosY = y;
}
//..............................................................................

//..............................................................................
unsigned int vga_get_back_color  ( void )
{
    return BackColor;
}
//..............................................................................

//..............................................................................
void         vga_set_back_color  ( unsigned int color )
{
    BackColor = color;
}
//..............................................................................

//..............................................................................
unsigned int vga_get_fore_color  ( void )
{
    return ForeColor;
}
//..............................................................................

//..............................................................................
void         vga_set_fore_color  ( unsigned int color )
{
    ForeColor = color;
}
//..............................................................................

//..............................................................................
void vga_draw_text(unsigned int x0, unsigned int y0, unsigned char * text,
    unsigned char back_color, unsigned char fore_color, unsigned int transparent)
{
    x0 *= FONT_WIDTH_PIXELS;
    y0 *= FONT_HEIGHT_LINES;

    if(y0 > vga_get_video_height())
        return;

    vga_draw_string(x0, y0, text, back_color, fore_color, transparent);
}
//..............................................................................

//..............................................................................
void vga_draw_string(unsigned int x0, unsigned int y0, unsigned char * string,
    unsigned char back_color, unsigned char fore_color, unsigned int transparent)
{
    unsigned char chr;

    while(*string != 0)
    {
        if(x0 + FONT_WIDTH_PIXELS >= vga_get_video_width())
        {
            x0 = 0;
            y0 += FONT_HEIGHT_LINES;
            if(y0 > vga_get_video_height())
                return;
        };
        chr = *string;
        if(chr < 31 || chr > 127)
            chr = 32;
        vga_draw_bitmask(x0, y0, FONT_WIDTH_PIXELS, FONT_HEIGHT_LINES, font_data[chr - 32], back_color, fore_color, transparent);
        x0 += FONT_WIDTH_PIXELS;
        string++;
    }
}
//..............................................................................

//..............................................................................
void vga_write_char(unsigned int chr)
{
    if(chr < 32 || chr > 126)
        chr = 32;
    vga_draw_bitmask(CaretPosX * FONT_WIDTH_PIXELS, CaretPosY * FONT_HEIGHT_LINES, FONT_WIDTH_PIXELS,
        FONT_HEIGHT_LINES, font_data[chr - 32], BackColor, ForeColor, 0);
}
//..............................................................................

//..............................................................................
vga_scroll_mode_t vga_get_vertical_scroll_mode ( void )
{
    return ScrollMode;
}
//..............................................................................

//..............................................................................
void vga_set_vertical_scroll_mode ( vga_scroll_mode_t mode )
{
    ScrollMode = mode;
}
//..............................................................................

//..............................................................................
void vga_vertical_scroll (const int Height)
{
    CaretPosX = 0;
    CaretPosY++;
    switch (ScrollMode)
    {
        case vsm_wrap:
            if (CaretPosY >= Height) CaretPosY = 0;
            break;
        case vsm_wrap_and_clear:
            if (CaretPosY >= Height)
            {
                CaretPosY = 0;
                vga_clear_screen();
            }
            break;
        case vsm_scroll:
            if (CaretPosY >= Height)
            {
                int line_size = vga_get_video_width() * vga_get_bpp() / 8;
                int page_size = line_size * vga_get_video_height();
                line_size *= FONT_HEIGHT_LINES;
                memcpy(vga_get_address_for_drawing(), vga_get_address_for_drawing() + line_size, page_size - line_size);
                CaretPosY--;
                vga_fill(0, CaretPosY * FONT_HEIGHT_LINES, vga_get_video_width() - 1, vga_get_video_height() - 1, BackColor);
            }
            break;
    }
}
//..............................................................................

//..............................................................................
int vga_write(const char * buf, int size)
{
    unsigned int  i;
    unsigned int  Width  = vga_get_video_width()  / FONT_WIDTH_PIXELS;
    unsigned int  Height = vga_get_video_height() / FONT_HEIGHT_LINES;

    for(i = 0; i < size; i++)
    {
        switch(*buf)
        {
            case '\b' :
                {
                    if(CaretPosX == 0)
                    {
                        CaretPosX = Width - 1;
                        if(CaretPosY == 0)
                            CaretPosY = Height - 1;
                        else
                            CaretPosY--;
                    }
                    else
                        CaretPosX--;
                    break;
                };
            case '\t' :
                {
                    CaretPosX += 4 - (CaretPosX % 4);
                    if(CaretPosX >= Width) vga_vertical_scroll (Height);
                    break;
                };
            case '\n' :
                {
                    vga_vertical_scroll (Height);
                    break;
                };
            case '\r' :
                {
                    CaretPosX = 0;
                    break;
                };
            case '\f' :
                {
                    CaretPosX = 0;
                    CaretPosY = 0;
                    vga_fill_screen(BackColor);
                    break;
                }
            default:
                {
                    vga_write_char(*buf);
                    CaretPosX++;
                    if(CaretPosX >= Width) vga_vertical_scroll (Height);
                    break;
                }
        }
        buf++;
    }
    return size;
}
//..............................................................................


