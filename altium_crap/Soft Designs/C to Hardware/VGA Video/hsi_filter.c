#include "video.h"

#ifndef USE_HSI_FILTER_MULTIPLIERS
#include "mul9x9.h"
#include "mul10x10.h"
#include "mul11x11.h"
#endif

void hsi_filter( uint12_t  H_FILTER,                                            // Hue filter angle, in fixed point 0-359 degree range
                 uint11_t  S_FILTER,                                            // Saturation filter coefficient, in fixed point 0-2 range
                 uint9_t   I_FILTER,                                            // Intensity filter coefficient, in fixed point 0-2 range
                 hsi_t     HSI_I                                                // HSI output of rgb-to-hsi filter, format H 12-bit, S 10-bit, I 8-bit
               )
{
    hsi_t        c_h = (hue_t)(HSI_I >> 18);
    uint10_t     c_s = (uint10_t)(HSI_I >> 8 );
    intensity_t  c_i = (intensity_t)HSI_I;
    uint11_t     f_s = (S_FILTER >> 10) ? ~S_FILTER : S_FILTER;
    uint9_t      f_i = (I_FILTER >> 8) ? ~I_FILTER : I_FILTER;

    c_h += H_FILTER;

#if USE_HSI_FILTER_MULTIPLIERS
    c_s = (S_FILTER >> 10) ? ~c_s : c_s;
    c_s = ( c_s * (f_s + 1) ) >> 10;
    c_s = (S_FILTER >> 10) ? ~c_s : c_s;

    c_s = (I_FILTER >> 8) ? ~c_s : c_s;
    c_s = ( c_s * ((((uint11_t)f_i)<<2) + 1) ) >> 10;                           // When intensity is scaled the staturation shall be scaled with same factor
    c_s = (I_FILTER >> 8) ? ~c_s : c_s;
                                                                                //  Done to mitigate the effects of "false" colors when the intensity of white tones is scaled.
    c_i = (I_FILTER >> 8) ? ~c_i : c_i;
    c_i = ( c_i * (f_i + 1) ) >> 8;
    c_i = (I_FILTER >> 8) ? ~c_i : c_i;
#else
    c_s = (S_FILTER >> 10) ? ~c_s : c_s;
    c_s = mul11x11( c_s, (f_s + 1) ) >> 10;                                     // Inline function mul11x11() is used to save hardware multipliers
    c_s = (S_FILTER >> 10) ? ~c_s : c_s;

    c_s = (I_FILTER >> 8) ? ~c_s : c_s;
    c_s = mul11x11( c_s, ((((uint11_t)f_i)<<2) + 1) ) >> 10;
    c_s = (I_FILTER >> 8) ? ~c_s : c_s;

    c_i = (I_FILTER >> 8) ? ~c_i : c_i;
    c_i = mul9x9( c_i, f_i + 1 ) >> 8;
    c_i = (I_FILTER >> 8) ? ~c_i : c_i;
#endif

    NEXT( ((hsi_t)c_h) << 18 | ((hsi_t)c_s) << 8 | c_i );
}














