#include "video.h"

/*
 * HSI to RGB colorspace conversion (stage 2).
 *
 * This component converts the HUE to a color component R, G or B. The
 * HSI-cylinder is rotated over an angle of DELTA_HUE so that the same
 * conversion algorithm can be used for each color component. These
 * rotation angles should be set as follows:
 *
 *   Red:    DELTA_HUE = 0x555      ( = ANGLE_120_DEGREES )
 *   Green:  DELTA_HUE = 0          ( = ANGLE_0_DEGREES )
 *   Blue:   DELTA_HUE = -0x555     ( = -ANGLE_120_DEGREES )
 *
 * The value of the color component is calculated like this:
 *
 *      if HUE < ANGLE_60_DEGREES:  color = HUE / ANGLE_60_DEGREES * (COLOR_MAX - COLOR_MIN)
 * else if HUE < ANGLE_120_DEGREES: color = COLOR_MAX
 * else if HUE < ANGLE_180_DEGREES: color = COLOR_MAX
 * else if HUE < ANGLE_240_DEGREES: color = (ANGLE_240_DEGREES - HUE) / ANGLE_60_DEGREES * (COLOR_MAX - COLOR_MIN)
 * else if HUE < ANGLE_300_DEGREES: color = COLOR_MIN
 * else if HUE < ANGLE_360_DEGREES: color = COLOR_MIN
 *
 * For calculation of the values COLOR_MAX and COLOR_MIN, see stage 1 of
 * the HSI to RGB colorspace conversion.
 *
 */

void hsi2rgb_color( hue_t HUE, uint10_t COLOR_MAX, uint10_t COLOR_MIN )
{
        hue_t           hue = HUE + DELTA_HUE;                                  // DELTA_HUE is defined in the code symbol configuration dialog
        color_t         color;
        uint16_t        delta;
        uint16_t        angle;

        delta = COLOR_MAX - COLOR_MIN;
        delta = ( delta << 2 ) + ( delta << 1 );
        angle = ( hue < ANGLE_60_DEGREES ) ? hue : (ANGLE_240_DEGREES - hue );

        if ( hue < ANGLE_60_DEGREES || (ANGLE_180_DEGREES <= hue && hue < ANGLE_240_DEGREES) )
        {
                color = (( ((unsigned int)COLOR_MIN) << 12 ) + ( delta * angle )) >> 14;
        }
        else if ( hue < ANGLE_180_DEGREES )
        {
                color = COLOR_MAX >> 2;
        }
        else
        {
                color = COLOR_MIN >> 2;
        }

        NEXT( color );
}
