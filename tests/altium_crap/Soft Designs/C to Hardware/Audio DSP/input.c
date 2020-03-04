#include <stdint.h>
#include <stdbool.h>
#include "process_audio.h"

__I2S volatile uint32_t i2s_control __at(0);
__I2S volatile uint32_t i2s_mode    __at(4);
__I2S volatile uint32_t i2s_status  __at(8);
__I2S volatile uint32_t i2s_data    __at(12);

void control_in(void)
{
    i2s_control = 0x00011004; // watermark = 16 (half), prescaler = 4
    i2s_mode    = 0x00001010; // no interrupts, right justified, receive = true, transmit = true, stereo in and out, width = 16

    for (;;)
    {
        while ((i2s_status & 0x80) == 0);

        FILTER((sample_device_t) i2s_data, true);
        FILTER((sample_device_t) i2s_data, false);

        i2s_mode = 0x00001810; // no interrupts, receive = true, transmit = true, stereo in and out, width = 16
    }
}

