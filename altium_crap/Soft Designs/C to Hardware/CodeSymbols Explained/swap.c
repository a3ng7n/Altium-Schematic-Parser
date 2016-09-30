#include <stdint.h>
#include <stdbool.h>

uint32_t swap(uint32_t DAT_IN)
{
    return ((DAT_IN & 0x0f0f0f0f) << 4) | ((DAT_IN & 0xf0f0f0f0) >> 4);
}
