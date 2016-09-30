#include "font.h"
//#include "dsf_system.h"

#include "3dcube.h"

static void draw_char( short x0, short y0, short width, short height, const unsigned short *data, uint16_t color, char italic, uint16_t *vgabuf)
{
    short bit_number = 16;
    short i, j;
    unsigned char value;
    unsigned char d = 0;

    if ( italic )
    {
        d = height / 3;
    }

    for(i = 0; i < height; i++)
    {
        for(j = d; j < width + d; j++)
        {
            if(bit_number == 0)
            {
                data++;
                bit_number = 16;
            }
            bit_number--;
            value = ((*data) >> bit_number) & 1;
            if(value)
            {
                uint16_t pixelnr = (y0 + i) * VGA_W + (x0 + j);
                vgabuf[pixelnr] = color;
            }
        }
        bit_number = 0;
        if ( !( i % 3) && ( d > 0 ))
        {
            d--;
        }
    }
}


void strdraw( short x0, short y0, char *str, t_FONT *font, uint16_t *vgabuf)
{
    char c;

    while(*str != 0)
    {
        c = *str;
        if( c < 32 ) c = 32;
        draw_char(x0, y0, font->fontdata->data[(c-32) * (font->fontdata->height+1)], font->fontdata->height,
                  &font->fontdata->data[(c-32) * (font->fontdata->height+1) + 1], font->color, font->style & fs_italic, vgabuf);
        if (font->style & fs_bold)
        {
            x0++;
            draw_char(x0, y0, font->fontdata->data[(c-32) * (font->fontdata->height+1)], font->fontdata->height,
                      &font->fontdata->data[(c-32) * (font->fontdata->height+1) + 1], font->color, font->style & fs_italic, vgabuf);
        }
        x0 += font->fontdata->data[(c-32) * (font->fontdata->height+1)];
        str++;
    }
}


unsigned int strwidth( char *str, t_FONT *font )
{
    int retval = 0;
    char c;

    while(*str != 0)
    {
        c = *str;
        if( c < 32 ) c = 32;
        retval += font->fontdata->data[(c-32) * (font->fontdata->height+1)];
        if (font->style & fs_bold) retval++;
        str++;
    }
    if (font->style & fs_italic) retval += font->fontdata->height / 3;

    retval--;   /* correct for last interchar spacing */

    return retval;
}


