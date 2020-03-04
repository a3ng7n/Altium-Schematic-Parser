#include "video.h"
#include "mul10x10.h"

__HIS uint17_t histogram[HIS_X_RES] __at(0);
__CONV uint10_t intensity_conv_tab[HIS_X_RES] __at(0);                          //__at(HIS_X_RES*4);

void contrast_filter( hsi_t HSI_I )
{
    intensity_t intensity_old = (intensity_t)HSI_I;
    histogram[intensity_old]++;
    intensity_t intensity_new = (intensity_t)intensity_conv_tab[intensity_old];

    // Adapt saturation 
# if ENABLE_CONTRAST_SATURATION_ADJUST
     uint10_t pix_sat = (uint10_t)(HSI_I >> 8 );                                // Saturation

     uint10_t f_sat = (intensity_new < intensity_old) ? (0x3ff - (uint10_t)(intensity_old - intensity_new) << 2) : 0x3ff;

     pix_sat = mul10x10( pix_sat, f_sat ) >> 10;                                // Inline function mul10x10() is used to save hardware multipliers

     NEXT( (HSI_I & 0x3FFF0000) | ((hsi_t)pix_sat) << 8 | intensity_new );
# else
     NEXT( (HSI_I & 0x3FFFFF00) | intensity_new );
# endif
}

