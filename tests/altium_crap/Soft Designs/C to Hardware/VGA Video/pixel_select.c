#include "video.h"

void pixel_select( y_t Y_I, his_t HIS_I, addr_t ADDR_I, rgb_t RGB_I, hsi_t HSI_I, bool HSI_MODE )
{
    uint16_t rgb;

    if ( (VID_Y_RES - 1 - Y_I) < HIS_I )
    {
        // Display histogram
         rgb = 0xa514;
    }
    else
    {
        // Display rgb or hsi output of filtered camera data
        uint5_t r = (uint5_t)((HSI_MODE) ? HSI_I >> 21 : RGB_I >> 19);
        uint6_t g = (uint6_t)((HSI_MODE) ? HSI_I >> 10 : RGB_I >> 10);
        uint5_t b = (uint5_t)((HSI_MODE) ? HSI_I >> 3  : RGB_I >> 3 );

        rgb = (r << 11) | (g << 5) | b;
    }

    NEXT( ADDR_I, rgb );
}

