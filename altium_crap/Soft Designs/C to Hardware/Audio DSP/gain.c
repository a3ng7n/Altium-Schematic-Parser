#include "process_audio.h"
#include "clip.h"

__input gain_t GAIN_I;
__output bool CLIP_ERROR_O;

// volume is in 4.12 fixed integer format
#define GAIN_SHIFT 12
#define MAX_POS 32768

void NEXT(sample_device_t INP, channel_t CHANNEL);

void GAIN(sample_device_t INP, bool CHANNEL)
{
        sample_long_t    adjusted_sample;
        sample_device_t  clipped_sample;
        bool             clipping_error;

        adjusted_sample = (INP * GAIN_I) >> GAIN_SHIFT;
        clipped_sample = clip_long_to_device(adjusted_sample, &clipping_error);
        CLIP_ERROR_O = clipping_error;

        NEXT(clipped_sample, CHANNEL);
}
