#include "process_audio.h"
#include "clip.h"

__output bool CLIP_INPUT_ERROR_O;

void NEXT(sample_device_t INP, channel_t CHANNEL);

void FILTER(sample_device_t INP, channel_t CHANNEL)
{
    CLIP_INPUT_ERROR_O = is_clipped(INP);
    NEXT(INP, CHANNEL);
}
