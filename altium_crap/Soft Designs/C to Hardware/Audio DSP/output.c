#include <stdint.h>
#include <stdbool.h>
#include "process_audio.h"

__I2S volatile uint32_t i2s_data    __at(12);

void control_out(int16_t SAMPLE)
{
    i2s_data = SAMPLE;
}

