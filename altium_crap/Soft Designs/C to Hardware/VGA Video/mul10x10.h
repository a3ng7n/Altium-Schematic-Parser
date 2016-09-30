#include <stdint.h>

static inline uint20_t mul10x10( uint10_t x, uint10_t y )
{
        uint20_t res0 = ( y & 1   ) ? ((uint20_t)x << 0) : 0;
        uint20_t res1 = ( y & 2   ) ? ((uint20_t)x << 1) : 0;
        uint20_t res2 = ( y & 4   ) ? ((uint20_t)x << 2) : 0;
        uint20_t res3 = ( y & 8   ) ? ((uint20_t)x << 3) : 0;
        uint20_t res4 = ( y & 16  ) ? ((uint20_t)x << 4) : 0;
        uint20_t res5 = ( y & 32  ) ? ((uint20_t)x << 5) : 0;
        uint20_t res6 = ( y & 64  ) ? ((uint20_t)x << 6) : 0;
        uint20_t res7 = ( y & 128 ) ? ((uint20_t)x << 7) : 0;
        uint20_t res8 = ( y & 256 ) ? ((uint20_t)x << 8) : 0;
        uint20_t res9 = ( y & 512 ) ? ((uint20_t)x << 9) : 0;

        res0 += res2;
        res1 += res3;
        res4 += res6;
        res5 += res7;
        res8 += res9;

        res0 += res1;
        res4 += res5;

        return res0 + res4 + res8;
}


