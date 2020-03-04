#include "process_audio.h"
#include "clip.h"

// Interface to Equalizer Virtual Instrument
__input uint16_t CH00_GAIN;
__input uint16_t CH01_GAIN;
__input uint16_t CH02_GAIN;
__input uint16_t CH03_GAIN;
__input uint16_t CH04_GAIN;
__input uint16_t CH05_GAIN;
__input uint16_t CH06_GAIN;
__input uint16_t CH07_GAIN;
__input uint16_t CH08_GAIN;
__input uint16_t CH09_GAIN;
__input uint16_t CH10_GAIN;
__input uint16_t CH11_GAIN;
__input uint16_t CH12_GAIN;
__input uint16_t CH13_GAIN;
__input uint16_t CH14_GAIN;
__input bool ENABLE_EQ;
__input bool DELTA_ONLY;

__output bool CLIP_ERROR_EQ;    // Linked to "Error Filter led" on Equalizer VI


static uint16_t band_gain(band_t band_no)
{
    switch(band_no)
    {
        case  0 : return CH00_GAIN;
        case  1 : return CH01_GAIN;
        case  2 : return CH02_GAIN;
        case  3 : return CH03_GAIN;
        case  4 : return CH04_GAIN;
        case  5 : return CH05_GAIN;
        case  6 : return CH06_GAIN;
        case  7 : return CH07_GAIN;
        case  8 : return CH08_GAIN;
        case  9 : return CH09_GAIN;
        case 10 : return CH10_GAIN;
        case 11 : return CH11_GAIN;
        case 12 : return CH12_GAIN;
        case 13 : return CH13_GAIN;
        case 14 : return CH14_GAIN;
    }
    return 0;
}


sample_device_t BIQUAD(sample_device_t INP, channel_t CHANNEL, band_t BAND);

void NEXT(sample_device_t INP, channel_t CHANNEL);

void equalize(sample_device_t INP, channel_t CHANNEL)
{
    sample_long_t    processed;
    sample_device_t  retval;
    bool             clip_error = false;

    if (ENABLE_EQ)
    {
        processed  = 0;

        for (band_t i = 0; i <= 14; i++)
        {
            processed += ((int16_t)(band_gain(i) - GAIN_UNITY)) * BIQUAD(INP, CHANNEL,  i);
        }
        processed >>= GAIN_SHIFT;
        if (!DELTA_ONLY)
        {
            processed += INP;
        }
    }
    else
    {
         processed = INP;
    }
    retval = clip_long_to_device(processed, &clip_error);
    CLIP_ERROR_EQ = clip_error;

    NEXT(retval, CHANNEL);
}
