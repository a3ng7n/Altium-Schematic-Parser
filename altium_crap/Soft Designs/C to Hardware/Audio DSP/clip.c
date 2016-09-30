#include "process_audio.h"

#define CLIPPED_TIME_LIMIT  50000 //* CLIP_TIME_MULTIPLIER arg need define on the sch symbol

// Return 'true' if 'input_sample' of type sample_device_t overflows
bool is_clipped(sample_device_t input_sample)
{
    bool clipped=false;

    static uint32_t clipped_time=0;
    static uint32_t clipped_time_limit=CLIPPED_TIME_LIMIT;

    if (input_sample == SAMPLE_DEVICE_MAX)
    {
        clipped = true;
    }
    else if (input_sample == SAMPLE_DEVICE_MIN)
    {
        clipped=true;
    }

    if (clipped)
    {
        clipped_time=1;
    }
    else if (clipped_time > 0)
    {
        clipped_time++;
        if (clipped_time > clipped_time_limit)
        {
            clipped_time = 0;
        }
    }
    return clipped_time > 0;
}

sample_device_t clip_long_to_device(sample_long_t input_sample, bool* clip_error)
{
    sample_device_t retval;
    bool clipped=false;

    static uint32_t clipped_time=0;
    static uint32_t clipped_time_limit=CLIPPED_TIME_LIMIT;

    if (input_sample > SAMPLE_DEVICE_MAX)
    {
        retval = SAMPLE_DEVICE_MAX;
        clipped = true;
    }
    else if (input_sample < SAMPLE_DEVICE_MIN)
    {
        retval = SAMPLE_DEVICE_MIN;
        clipped=true;
    }
    else
    {
        retval = (sample_device_t) input_sample;
    }

    if (clipped)
    {
        clipped_time=1;
    }
    else if (clipped_time > 0)
    {
        clipped_time++;
        if (clipped_time > clipped_time_limit)
        {
            clipped_time = 0;
        }
    }

    *clip_error = clipped_time > 0;

    return retval;
}



