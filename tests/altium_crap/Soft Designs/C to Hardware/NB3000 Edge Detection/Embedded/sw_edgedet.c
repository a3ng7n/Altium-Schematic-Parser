/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:         Edge detection
|*
|*  COPYRIGHT:          Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:        Edge detection functions using SOBEL
|*                      Kernel matrices:  | +1 +2 +1 | and | -1 0 +1 |
|*                                        |  0  0  0 |     | -2 0 +2 |
|*                                        | -1 -2 -1 |     | -1 0 +1 |
|*
|*                      This module will be compiled into software
 */

#include <stdlib.h>
#include <stdint.h>
#include "edgedet.h"

// Cache (will go into internal MCU memory)
static int32_t sw_cache0[600];
static int32_t sw_cache1[600];
static int32_t sw_cache2[600];

/********************************************************************
|*
|*  FUNCTION    : sw_luma
|*
|*  PARAMETERS  : pixel = 5:6:5 encoded color pixel
|*
|*  RETURNS     : luma value of pixel
|*
|*  DESCRIPTION : Convert pixel into B/W (luma channel only)
 */

uint16_t sw_luma( uint16_t pixel )
{
    uint16_t blue = pixel & 0x1F;
    pixel >>= 5;
    uint16_t green = pixel & 0x3F;
    pixel >>= 6;
    uint16_t red = pixel;
    return red * 30 + green * 59 / 2 + blue * 11;
}

/********************************************************************
|*
|*  FUNCTION    : sw_find_edges
|*
|*  PARAMETERS  : input = pointer to input image
|*                output = pointer to where filter output should start
|*                width, height = size of image
|*                xres = line size of video (typically 640 or 800 for VGA)
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Filter edges using sobel
 */

 void sw_find_edges( uint16_t * input, uint16_t * output, uint16_t width, uint16_t height, uint16_t xres, unsigned short divisor )
{
    unsigned x, y;
    unsigned index;
    signed prevpixel;
    signed nextpixel;
    signed curpixel;
    signed gx, gy, g;
    int32_t * west = sw_cache0;
    int32_t * northsouth = sw_cache1;
    int32_t * east = sw_cache2;
    uint16_t cache_index = 0;

    // First, smoothen horizontally
    index = 0;
    for ( y = 0; y < height; y++ )
    {
        nextpixel = sw_luma(input[index]);
        curpixel = sw_luma(input[index+1]);  // Mirror!

        for ( x = 0; x < width; x++ )
        {
            prevpixel = curpixel;
            curpixel = nextpixel;
            nextpixel = ( x < width - 1 ) ? sw_luma(input[index + 1]) : prevpixel;
            input[index++] = prevpixel + curpixel * 2 + nextpixel;
        }
    }

    // Step 2: vertically smoothen
    for ( x = 0; x < width; x++ )
    {
        index = x;
        nextpixel = input[index];
        curpixel = input[index + width];
        for ( y = 0; y < height; y++ )
        {
            prevpixel = curpixel;
            curpixel = nextpixel;
            nextpixel = ( y < height - 1 ) ? input[index + width] : prevpixel;
            input[index] = prevpixel + curpixel * 2 + nextpixel;
            index += width;
        }
    }

    // Step 3: sobel

    // Fill north


    index = 0;
    for ( y = 0; y < height; y++ )
    {
        northsouth[y] = input[index];
        east[y] = input[index+1];
        index += width;
    }

    for ( x = 1; x < width - 1; x++ )
    {
        west = northsouth;
        northsouth = east;
        switch ( ++cache_index )
        {
        case 1  : east = sw_cache0; break;
        case 2  : east = sw_cache1; break;
        default : east = sw_cache2; cache_index = 0;
        }
        east[0] = input[x+1];
        east[1] = input[x+1+width];
        index = x + 1 + width;

        for ( y = 1; y < height -1; y++ )
        {
            index += width;
            east[y+1] = input[index];

            gx = -     west[y-1] +     east[y-1]
                 - 2 * west[y]   + 2 * east[y]
                 -     west[y+1] +     east[y+1];
            gy =   west[y-1] + 2 * northsouth[y-1] + east[y-1]
                 - west[y+1] - 2 * northsouth[y+1] - east[y+1];
            g = (abs(gx) + abs(gy)) >> divisor;
            if ( g >= 31 )
            {
                g = 31;
            }
            g = (g << 11) | (g << 6) | g;
            output[x + y * xres] = (uint16_t)g;
        }
    }
}






















