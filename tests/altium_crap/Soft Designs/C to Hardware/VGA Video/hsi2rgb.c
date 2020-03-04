#include "video.h"

#include "mul11x11.h"

/*
 * HSI to RGB colorspace conversion (stage 1).
 *
 * This component calculates the COLOR_MIN & COLOR_MAX values that are
 * used by stage 2 of the HSI to RGB colorspace conversion to convert
 * the HUE to a color component R, G or B.
 */

void hsi2rgb( saturation_t SATURATION, intensity_t INTENSITY )
{
        uint11_t    saturation = (uint11_t)SATURATION;
        uint11_t    intensity  = ((uint11_t)INTENSITY) << 2;
        uint16_t    color_max  = mul11x11(saturation, intensity + 1) >> 10;
        uint16_t    color_min;

        if ( intensity >> 9 )
        {
                color_max = saturation - color_max;
        }
        color_max += intensity;
        color_min = ( intensity << 1 ) - color_max;

        NEXT( SATURATION, INTENSITY, (uint10_t)color_max, (uint10_t)color_min );
}
