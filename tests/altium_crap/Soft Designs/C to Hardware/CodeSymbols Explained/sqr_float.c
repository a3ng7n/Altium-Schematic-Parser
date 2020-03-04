#include <stdint.h>

extern void pack_float(uint32_t fraction, int32_t exponent);

void sqr_float(uint32_t fraction, int32_t exponent)
{
    uint32_t out_fraction = (uint32_t)((uint64_t)fraction * fraction >> 24);
    int32_t out_exponent = exponent * 2 + 1;

    // (out_exponent == 0xff) => overflow,
    // so we need to set out_fraction to zero,
    // otherwise the result represents NaN (Not A Number)

    if (out_exponent == 0xff)
    {
        out_fraction = 0;
    }
    
    pack_float(out_fraction, out_exponent);
}
