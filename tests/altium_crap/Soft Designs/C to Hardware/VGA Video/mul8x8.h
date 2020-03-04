#include <stdint.h>

static inline uint16_t mul8x8( uint8_t x, uint8_t y )
{
        uint16_t res0 = ( y & 1   ) ? ((uint16_t)x << 0) : 0;
        uint16_t res1 = ( y & 2   ) ? ((uint16_t)x << 1) : 0;
        uint16_t res2 = ( y & 4   ) ? ((uint16_t)x << 2) : 0;
        uint16_t res3 = ( y & 8   ) ? ((uint16_t)x << 3) : 0;
        uint16_t res4 = ( y & 16  ) ? ((uint16_t)x << 4) : 0;
        uint16_t res5 = ( y & 32  ) ? ((uint16_t)x << 5) : 0;
        uint16_t res6 = ( y & 64  ) ? ((uint16_t)x << 6) : 0;
        uint16_t res7 = ( y & 128 ) ? ((uint16_t)x << 7) : 0;

        res0 += res2;
        res1 += res3;
        res4 += res6;
        res5 += res7;

        res0 += res1;
        res4 += res5;

        return res0 + res4;
}


