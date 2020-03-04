#include "bitmap.h"
//#include "dsf_system.h"
//#include "wb_vga16.h"
#include <stdlib.h>

#include <stdio.h>

/*
 * The bitmap format is based on Little Endian processors,
 * so for correct reading the bitmap, conversion from Little ENdian
 * to Big Endian must be made.
 */
#define BE_READ_SHORT(x)    (*(x) + (*(x+1) << 8 ))
#define BE_READ_INT(x)      (*(x) + (*(x+1) << 8 ) + (*(x+2) << 16 ) + (*(x+3) << 24 ))

/*
 * Bitmap structures
 */

typedef struct
{
    char type[2];
    unsigned char size[4];
    unsigned char reserved[4];
    unsigned char offset[4];
} t_BMFILEHEADER;   /* size = 14 */

typedef struct
{
    unsigned char size[4];
    unsigned char width[4];
    unsigned char height[4];
    unsigned char planes[2];
    unsigned char bitcount[2];
    unsigned char compression[4];
    unsigned char imagesize[4];
    unsigned char xppm[4];
    unsigned char yppm[4];
    unsigned char colorused[4];
    unsigned char colorimportant[4];
} t_BMINFOHEADER;   /* size = 40 */

typedef struct
{
    unsigned char blue;
    unsigned char green;
    unsigned char red;
    unsigned char reserved;
} t_BMRGB;           /* size = 4 */

typedef struct
{
    t_BMFILEHEADER bmfh;    // bitmap file header
    t_BMINFOHEADER bmih;    // bitmap info header
    unsigned char data[];   // color data and pixel data
} t_BM;

/*
 * The bitmap format is based on Little Endian processors,
 * For correct reading of the the bitmap on a TSK3000,
 * conversion from Little Endian to Big Endian must be made.
 */


/*********************************************************************
|*
|*  Function    : bmp_decode
|*
|*  Parameters  : bmpdata - pointer to input bitmap
|*                bitmap - pointer to resulting decoded bitmap
|*                bitmaplength - lengt of buffer for decode bitmap
|*
|*  Returns     : none
|*
|*  Description : Decode the given bitmap-data to a plain bitmap image
 */

void bmp_decode(const unsigned char *bmpdata, unsigned short *bitmap, unsigned int bitmaplength)
{
    const t_BM *bm = (const t_BM*) bmpdata;
    int pixel_offset;
    int file_size;
    short color_depth;
    int width;
    int bound_width;
    int height;
    int c, r, i;
    char red, green, blue;
    unsigned char color_index = 0, p;
    short pixel;

    if ( bm->bmfh.type[0] != 'B' || bm->bmfh.type[1] != 'M' )
    {
        return;     /* unsupported or corrupted file format */
    }

    file_size = BE_READ_INT( bm->bmfh.size );
    pixel_offset = BE_READ_INT( bm->bmfh.offset );

    if ( pixel_offset >= file_size )
    {
        return;     /* unsupported or corrupted file format */
    }

    pixel_offset -= ( sizeof( t_BMFILEHEADER ) + sizeof( t_BMINFOHEADER ));
    width = BE_READ_INT( bm->bmih.width );
    height = BE_READ_INT( bm->bmih.height );

    if ((2 + width * height) > bitmaplength)
    {
        // does not fit in supplied bitmap
        return;
    }

    *bitmap++ = (unsigned short) width;
    *bitmap++ = (unsigned short) height;

    bitmap += (height - 1) * width;

    color_depth = BE_READ_SHORT( bm->bmih.bitcount );
    switch( color_depth )
    {
    case 1:
        bound_width = ((( width + 7 ) / 8 ) + 3) & ~3;
        for (r = 0; r < height; r++ )
        {
            for (c = 0, p = 0; c < width; c++)
            {
                if ( p == 0 )
                {
                    color_index = bm->data[pixel_offset + r * bound_width + c / 8];
                    p = 0x80;
                }
                if (color_index & p)
                {
                    blue  = bm->data[4];
                    green = bm->data[5];
                    red   = bm->data[6];
                }
                else
                {
                    blue  = bm->data[0];
                    green = bm->data[1];
                    red   = bm->data[2];
                }
                pixel = (( red & 0xF8 ) << 8 ) + (( green & 0xFC ) << 3 ) + (( blue & 0xF8 ) >> 3 );
                *bitmap++ = pixel;
                p >>= 1;
            }
            bitmap -= 2 * width;
        }
        break;
    case 4:
        bound_width = ((( width + 1 ) / 2 ) + 3) & ~3;
        for (r = 0; r < height; r++ )
        {
            for (c = 0; c < width; c++)
            {
                if ( !(c & 0x01 ))
                {
                    color_index = bm->data[pixel_offset + r * bound_width + c / 2];
                    blue        = bm->data[4 * (color_index >> 4)];
                    green       = bm->data[4 * (color_index >> 4) + 1];
                    red         = bm->data[4 * (color_index >> 4) + 2];
                }
                else
                {
                    blue        = bm->data[4 * (color_index & 0x0f)];
                    green       = bm->data[4 * (color_index & 0x0f) + 1];
                    red         = bm->data[4 * (color_index & 0x0f) + 2];
                }
                pixel = (( red & 0xF8 ) << 8 ) + (( green & 0xFC ) << 3 ) + (( blue & 0xF8 ) >> 3 );
                *bitmap++ = pixel;
            }
            bitmap -= 2 * width;
        }
        break;
    case 8:
        bound_width = (width + 3) & ~3;
        for (r = 0; r < height; r++ )
        {
            for (c = 0; c < width; c++)
            {
                color_index = bm->data[pixel_offset + r * bound_width + c];
                blue        = bm->data[4 * color_index];
                green       = bm->data[4 * color_index + 1];
                red         = bm->data[4 * color_index + 2];

                pixel = (( red & 0xF8 ) << 8 ) + (( green & 0xFC ) << 3 ) + (( blue & 0xF8 ) >> 3 );
                *bitmap++ = pixel;
            }
            bitmap -= 2 * width;
        }
        break;
    case 24:
        bound_width = (( width * 3 ) + 3) & ~3;
        for (r = 0; r < height; r++ )
        {
            for (c = 0, i = 0; c < width; )
            {
                blue  = bm->data[r * bound_width + i++];
                green = bm->data[r * bound_width + i++];
                red   = bm->data[r * bound_width + i++];
                pixel = (( red & 0xF8 ) << 8 ) + (( green & 0xFC ) << 3 ) + (( blue & 0xF8 ) >> 3 );
                *bitmap++ = pixel;
                c++;
            }
            bitmap -= 2 * width;
        }
        break;
    default:
        return;     /* unsupported or corrupted file format */
    }
}

