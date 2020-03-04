#include "video.h"

__HIS uint10_t histogram[HIS_X_RES] __at(0);

void add_histogram( uint20_t ADDR_I,                                            // BT656 address of pixel pair
                    uint24_t RGB_0_I,                                           // RGB value of right / uneven pixel
                    uint24_t RGB_1_I,                                           // RGB value of left / even pixel
                    uint10_t Y,                                                 // Y screen coordinate
                    uint17_t H                                                  // H histogram intensity index [0..511] (concatenated by caller but split using bus splitter)
                  )
{
    // Read histogram data form memory and pass histogram data together with input parameters to the next component.
    NEXT( ADDR_I, RGB_0_I, RGB_1_I, Y, histogram[(H + HIS_DELTA_X) >> HIS_FRACT_BITS], histogram[H >> HIS_FRACT_BITS] );
}

