#include "video.h"

/*
 * RGB to HSI colorspace conversion.
 *
 * This component is heavily optimized, so it will only work with
 * specific input datasets. Currently it only supports the 8-bit
 * grayscale camera mode and the 16-bit R5G6B5-mode.
 *
 * To configure this component, use the following defines:
 *
 * ENABLE_RGB2HSI_CONVERSION
 * - Enable the entire RGB2HSI conversion. Without this define, HSI
 *   will always be zero.
 *
 * ENABLE_RGB2HSI_PIPELINE
 * - Use a multi-stage pipeline instead of a single component. This
 *   will increase the throughput of the RGB2HSI conversion and is required
 *   to keep up with the camera output bandwidth.
 */


// A multi-stage pipeline is used to apply the RGB2HSI conversion.
// See the CHC user manual for more information about the used qualifiers.

static __CC(parallel,ack,nowait) void rgb2hsi_stage_5( addr_t ADDR_I, y_t Y_I, his_t HIS_I, hue_t hue, uint16_t saturation, uint8_t intensity )
{
    saturation -= (saturation >> 10);
    NEXT( hue << 18 | ((saturation & 0x3FF) << 8) | intensity, ADDR_I, Y_I, HIS_I );
}

static __CC(parallel,ack,nowait) void rgb2hsi_stage_4( addr_t ADDR_I, y_t Y_I, his_t HIS_I, hue_t hue, uint16_t saturation, uint8_t intensity )
{
    uint7_t         divisor;
    divisor = intensity >> 1;
    divisor = ( divisor & 0x40 ) ? 128 - divisor : divisor;

    saturation <<= 8;
    saturation /= divisor;
    rgb2hsi_stage_5( ADDR_I, Y_I, HIS_I, hue, saturation, intensity );
}

static __CC(parallel,ack,nowait) void rgb2hsi_stage_3( addr_t ADDR_I, y_t Y_I, his_t HIS_I, hue_t hue, uint16_t saturation, uint8_t intensity, uint21_t ofs )
{
    rgb2hsi_stage_4( ADDR_I, Y_I, HIS_I, hue + ofs, saturation, intensity );
}

static __CC(parallel,ack,nowait) void rgb2hsi_stage_2( addr_t ADDR_I, y_t Y_I, his_t HIS_I, int9_t hue, uint16_t saturation, uint8_t intensity, uint8_t delta, uint21_t ofs )
{
    rgb2hsi_stage_3( ADDR_I, Y_I, HIS_I, (hue << 9) / delta, saturation, intensity, ofs );
}

static __CC(parallel,ack,nowait) void rgb2hsi_stage_1( color_t r, color_t g, color_t b, color_t max, color_t min, addr_t ADDR_I, y_t Y_I, his_t HIS_I, bool GRAYSCALE )
{
    int9_t          hue;
    uint16_t        saturation;
    uint8_t         intensity;
    uint10_t        delta;
    int             delta_r;
    int             delta_g;
    int             delta_b;
    uint12_t        ofs;

    // Intensity = average( max, min ). I.e. using "bi-hexcone" model.
    intensity = (((uint9_t)max) + ((uint9_t)min)) >> 1;

    saturation = max - min;                                                 // Saturation is defined "chroma"
    delta = ( saturation << 1 ) + saturation;
    delta_r = max - r + delta;
    delta_g = max - g + delta;
    delta_b = max - b + delta;

    if ( max == r )
    {
        hue = delta_b - delta_g;
        ofs = 0;
    }
    else if ( max == g  )
    {
        hue = delta_r - delta_b;
        ofs = 4096 / 3;
    }
    else
    {
        hue = delta_g - delta_r;
        ofs = 2 * 4096 / 3;
    }

    if ( (GRAYSCALE && (max == min)) || (!GRAYSCALE && ((max >> 3) == (min >> 3))) )
    {
        /* Special case: grayscale value */
        hue = 0;
        saturation = 0;
        ofs = 0;
        delta = 4;
    }

    rgb2hsi_stage_2( ADDR_I, Y_I, HIS_I, hue, saturation, intensity, delta >> 2, ofs );
}

// This is the entry point of the pipelined function.
void rgb2hsi( rgb_t RGB_I, addr_t ADDR_I, y_t Y_I, his_t HIS_I, bool GRAYSCALE )
{
    uint8_t         r = (uint8_t)(RGB_I >> 16);
    uint8_t         g = (uint8_t)(RGB_I >> 8);
    uint8_t         b = (uint8_t)(RGB_I);
    uint8_t         max, min;

    // Calculate the maximum and minimum color channel values.
    // In grayscale mode, max and min will be identical.
    max = min = r;
    if ( max < g ) max = g; else min = g;
    if ( max < b ) max = b; else if ( min > b ) min = b;

    rgb2hsi_stage_1( r, g, b, max, min, ADDR_I, Y_I, HIS_I, GRAYSCALE );
}
