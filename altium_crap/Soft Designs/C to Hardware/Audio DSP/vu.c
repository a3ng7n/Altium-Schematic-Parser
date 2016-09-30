#include "process_audio.h"

__output uint8_t LEDS_O;   // Connected to physical led on Nano Board

// Return led settings based on signal level
unsigned char led_level(uint32_t level)
{
        uint8_t retval = 0;

        if (level & 0x00030000)       retval = 0x80;
        if (level & 0x000C0000)       retval = 0xC0;
        if (level & 0x00300000)       retval = 0xE0;
        if (level & 0x00C00000)       retval = 0xF0;
        if (level & 0x03000000)       retval = 0xF8;
        if (level & 0x0C000000)       retval = 0xFC;
        if (level & 0x30000000)       retval = 0xFE;
        if (level & 0xC0000000)       retval = 0xFF;

        return retval;
}

static inline int16_t abs16(int16_t x)
{
        return x > 0 ? x : -x;
}

#define VU_SAMPLE_COUNT 256  // must be power of two
#define VU_SHIFT        8    // to average the VU_SAMPLE_COUNT power samples

void NEXT(sample_device_t INP, channel_t CHANNEL);

void VU(sample_device_t INP, channel_t CHANNEL)
{
        static uint32_t power_history[VU_SAMPLE_COUNT];
        static uint16_t last = 0;
        static uint64_t total = 0;
        uint32_t        shifted_total;

        total -= power_history[last];

        power_history[last] = INP * INP;
        total += power_history[last];
        shifted_total = (uint32_t) (total >> VU_SHIFT);

        last = (last + 1) & (VU_SAMPLE_COUNT-1);
        LEDS_O = led_level(shifted_total);

        NEXT(INP, CHANNEL);
}

