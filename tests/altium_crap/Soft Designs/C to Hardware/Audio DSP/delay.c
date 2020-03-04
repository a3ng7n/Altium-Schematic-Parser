
#include "process_audio.h"
#include "clip.h"

#define LEVEL_SHIFT 9
#define LEVEL_200 0x200

__input uint8_t LEVEL_I;        // .8 fixed point value
__input uint8_t DELAY_I;        // 0-255 range gives 0-1.5 second delay, at 44KHz sampling and 64K stored samples
__output bool CLIP_ERROR_O;

__MEM static sample_long_t    delay_buffer_left [BUFFER_SIZE] __at( 0 * BUFFER_SIZE * sizeof(sample_long_t) );
__MEM static sample_long_t    delay_buffer_right[BUFFER_SIZE] __at( 1 * BUFFER_SIZE * sizeof(sample_long_t) );

void NEXT(sample_device_t INP, channel_t CHANNEL);

void DELAY(sample_device_t INP, bool CHANNEL)
{
    static uint32_t       index       = 0;
    __MEM sample_long_t*  buffer      = CHANNEL ? delay_buffer_left : delay_buffer_right;
    bool                  clip_error  = false;
    sample_device_t       retval      = INP;
    sample_long_t         processed   = INP;
    sample_long_t         delay_sample;
    sample_long_t         scaled_input;

    // fill the delay buffer
    buffer[index] = INP;

    if (DELAY_I > 0)
    {
        // Mix delay to original sound.
        // We naively try to keep the output volume of the mixed signal constant
        // by scaling the input value. This is done to prevent clipping.
        delay_sample = buffer[OLD_INDEX(DELAY_I, index )];
        delay_sample *= LEVEL_I;
        scaled_input = INP * (LEVEL_200 - LEVEL_I);
        processed = (scaled_input + delay_sample) >> LEVEL_SHIFT;

        retval = clip_long_to_device(processed, &clip_error);
    }
    index = (index + 1) & (BUFFER_SIZE - 1);
    CLIP_ERROR_O = clip_error;

    NEXT(retval, CHANNEL);
}



