#include "video.h"

#ifndef USE_RGB_FILTER_HARDWARE_MULTIPLIERS
#include "mul9x9.h"
#endif


// Apply filter, keep 565 encoding intact.
static inline rgb_t filter_channel( rgb_t   rgb,                                // 24-bit RGB value, encoded in 565 precision
                                    uint9_t filter,                             // Filter coefficient, in 9-bit fixed point 0-2 range
                                    int     shift )                             // Indicates wether R, G, or B value is processed
{
    color_t c = (color_t)(rgb >> shift);
    uint9_t f = (filter >> 8) ? ~filter : filter;

    c = (filter >> 8) ? ~c : c;

#if USE_RGB_FILTER_HARDWARE_MULTIPLIERS
    c = (color_t)(( c * ( f + 1 ) ) >> 8 );
#else
    c = (color_t)( mul9x9( c, f + 1 ) >> 8 );
#endif

    c = (filter >> 8) ? ~c : c;

    c = (shift == 8) ? (c & 0x000000FC) : (c & 0x000000F8);                     // Maintain 565 precision, TODO: must be removed for Black/White
    return ((rgb_t)c) << shift;
}


static inline rgb_t filter_rgb( rgb_t   rgb,                                    // RGB value of 1 pixel, 3x8-bit in 565 bits are significant
                                uint9_t r_filter,                               // R filter coefficient, in 9-bit fixed point 0-2 range
                                uint9_t g_filter,                               // G filter coefficient, in 9-bit fixed point 0-2 range
                                uint9_t b_filter                                // B filter coefficient, in 9-bit fixed point 0-2 range
               )
{
    rgb_t r = filter_channel( rgb, r_filter, 16 );                              // r: 8-bit bit 0..7 are significant
    rgb_t g = filter_channel( rgb, g_filter, 8 );                               // g: 8-bit bit 0..7 are significant
    rgb_t b = filter_channel( rgb, b_filter, 0 );                               // b: 8-bit bit 0..7 are significant

    return r | g | b;
}


// Entry point
void rgb_filter( rgb_t   RGB_0_I,                                               // RGB value of 1st pixel, 3x8-bit in 565 bits are significant
                 rgb_t   RGB_1_I,                                               // RGB value of 2nd pixel, 3x8-bit in 565 bits are significant
                 uint9_t R_FILTER,                                              // R filter coefficient, in 9-bit fixed point 0-2 range
                 uint9_t G_FILTER,                                              // G filter coefficient, in 9-bit fixed point 0-2 range
                 uint9_t B_FILTER,                                              // B filter coefficient, in 9-bit fixed point 0-2 range

                 // Unused parameters (passed on to the next component)
                 addr_t  ADDR_I,                                                // Address of the pixel-pair in the output video memory
                 y_t     Y_I,                                                   // Scanline of the pixel-pair in the output video memory
                 his_t   HIS_0_I,                                               // Height of the histogram at the x position of pixel 1
                 his_t   HIS_1_I                                                // Height of the histogram at the x position of pixel 2
               )
{
    rgb_t rgb0 = filter_rgb( RGB_0_I, R_FILTER, G_FILTER, B_FILTER );           // Apply filter on 1st pixel
    rgb_t rgb1 = filter_rgb( RGB_1_I, R_FILTER, G_FILTER, B_FILTER );           // Apply filter on 2nd pixel

    NEXT( rgb0, rgb1, ADDR_I, Y_I, HIS_0_I, HIS_1_I );
}
