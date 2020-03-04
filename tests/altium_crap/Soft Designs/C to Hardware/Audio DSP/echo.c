#include "process_audio.h"
#include "clip.h"

__input uint8_t LEVEL_I;        // .8 fixed point value
__input uint8_t DELAY_I;        // 0-255 range gives 0-1.5 second delay, at 44KHz sampling and 64K stored samples
__output bool CLIP_ERROR_O;

__MEM static sample_long_t echo_buffer_left  [BUFFER_SIZE] __at(0 * BUFFER_SIZE * sizeof(sample_long_t));
__MEM static sample_long_t echo_buffer_right [BUFFER_SIZE] __at(1 * BUFFER_SIZE * sizeof(sample_long_t));

void NEXT(sample_device_t INP, channel_t CHANNEL);

void ECHO(sample_device_t INP, bool CHANNEL)
{
    static uint32_t       index = 0;
    __MEM sample_long_t*  buffer = CHANNEL ? echo_buffer_left : echo_buffer_right;
    sample_long_t         processed;
    sample_device_t       retval;
    bool                  clip_error = false;
    sample_long_t         echo_sample;
    sample_long_t         scaled_input;

    if (DELAY_I > 0)
    {
        // Mix echo samples with the original sound.
        // We naively try to keep the output volume of the mixed signal constant
        // by scaling the input value. This is done to prevent clipping.
        echo_sample = buffer[OLD_INDEX(DELAY_I, index )];
        echo_sample *= LEVEL_I;
        scaled_input = INP * (LEVEL_200 - LEVEL_I);
        processed = (scaled_input + echo_sample) >> LEVEL_SHIFT;
        buffer[index] = processed;
        retval = clip_long_to_device(processed, &clip_error);
   }
    else
    {
        retval = INP;
        buffer[index] = 0;
    }

    index = (index + 1) & (BUFFER_SIZE - 1);
    CLIP_ERROR_O = clip_error;

    NEXT(retval, CHANNEL);
}

