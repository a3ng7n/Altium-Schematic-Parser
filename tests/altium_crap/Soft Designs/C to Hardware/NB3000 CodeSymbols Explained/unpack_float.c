#include <stdint.h>

#define FLOAT_FRACTION_SIZE             23
#define FLOAT_BIAS                      127
#define GET_FLOAT_MANTISSA( l )         ( (l) & 0x7fffff )
#define GET_FLOAT_EXPONENT( l )         ( ( (l) >> FLOAT_FRACTION_SIZE) & 0xff )

extern void sqr_float(uint32_t fraction, int32_t exponent);

typedef union
{
        uint32_t    l;
        float       f;
}
float2long_t;

void unpack_float(float f)
{
    float2long_t    fl;
    int32_t     exponent;
    uint32_t    fraction;

    __debug_printf("f=%f\n", f);
    fl.f = f;
    exponent = GET_FLOAT_EXPONENT(fl.l);
    fraction = GET_FLOAT_MANTISSA(fl.l) + ((exponent > 0) ? (1 << 23) : 0);

    // exponent & 0x80  --> +/-INF or NaN

    exponent -= FLOAT_BIAS;
    __debug_printf("exponent %x\n", exponent);
    __debug_printf("fraction %x\n", fraction);

    sqr_float(fraction, exponent);
}
