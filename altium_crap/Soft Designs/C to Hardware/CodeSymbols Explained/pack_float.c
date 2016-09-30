#include <stdint.h>

#define FLOAT_FRACTION_SIZE             23
#define FLOAT_BIAS                      127

typedef union
{
        uint32_t    l;
        float       f;
}
float2long_t;

__output float data;
__output bool WE;

void pack_float(uint32_t fraction, int32_t exponent)
{
    float2long_t    fl;

    WE = 0;
    // normalize the fraction so it is >= 1.0
    // note that this causes loss of precision,
    // it would be better to use an unsigned fixed
    // point type (MSB means 1) for fraction
    while (((fraction & (1 << 23)) == 0) && (fraction != 0))
    {
        fraction <<= 1;
        --exponent;
    }
    fl.l = ((exponent + FLOAT_BIAS) << FLOAT_FRACTION_SIZE) | (fraction & 0x7fffff);

    data = fl.f;
    WE = 1;             // Write data to FiFo
    WE = 0;
    __debug_printf("sent data: %f\n", fl.f);
}

