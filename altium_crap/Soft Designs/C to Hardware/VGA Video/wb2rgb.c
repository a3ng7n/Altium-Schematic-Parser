#include "video.h"

// This function:
// - Converts a 32-bit pixel pair (in R5G6B5 format) to separate 8-bit R,G & B values.
//   The low order bits of the R, G, and B are 0.
// - Convert the bt656 address to an X and Y screen coordinates and a histogram index.
//   Screen coordinate {0,0} represents the upper left hand corner of the display.
//   The histogram intensity index H is calculated by interpolating the X position.

void wb2rgb( uint32_t data, addr_t bt656_addr, uint4_t select )
{
    static uint10_t y = 0;     // Pixel y position
    static uint17_t h = 0;     // Histogram intensity index, fix-point value in 9.8  format, domain [0..511]
    static addr_t   next_scanline_start;

    rgb_t  rgb_left  = ((data >> 8) & 0x00F80000) | ((data >> 11) & 0x0000FC00) | ((data >> 13) & 0x000000F8);  // Even pixel in 24-bit rgb format
    rgb_t  rgb_right = ((data << 8) & 0x00F80000) | ((data <<  5) & 0x0000FC00) | ((data <<  3) & 0x000000F8);  // Uneven pixel in 24-bit rgb format

    // Calculate Y & H values
    if ( bt656_addr == 0 )
    {   // Start of the interlaced field with even lines
        h = 0; y = 0;
        next_scanline_start = VGA_X_RES * BYTES_PER_PIXEL * 2;
    }
    else if ( bt656_addr == (VGA_X_RES * BYTES_PER_PIXEL) )
    {   // Start of interlaced field with uneven lines
        h = 0; y = 1;
        next_scanline_start = VGA_X_RES * BYTES_PER_PIXEL * 3;
    }
    else if ( bt656_addr == next_scanline_start )
    {
        // Start of next scanline
        h = 0; y += 2;
        next_scanline_start += (addr_t)(VGA_X_RES * BYTES_PER_PIXEL * 2);
    }
    else
    {   // Proceed with next pixel pair on current scanline
        h += HIS_DELTA_X << 1;                     // 'h' is an interpolation of 'x', converted from [0..799] domain to [0..511] domain
    }

    // Center screen
    bt656_addr += (addr_t)(VID_X_BORDER * BYTES_PER_PIXEL + (VID_Y_BORDER * VGA_X_RES * BYTES_PER_PIXEL));

    NEXT(bt656_addr, rgb_right, rgb_left, y, h);
}

