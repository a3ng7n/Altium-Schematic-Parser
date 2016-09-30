/*
 * NB3000 CS4270 Audio Code Example
 *
 * The example reads data from audio input and sends it back to audio output.
 *
 */

#include "swplatform.h"

#define AUDIO_BUF_SIZE      500

void main(void)
{
    int16_t audio_buf[AUDIO_BUF_SIZE];
    uint32_t rx_size;

    // generated initialization code
    swplatform_init_stacks();

    // if initialization failed, keep trying
    while (audio_codec_ctrl == NULL)
    {
        audio_codec_ctrl = cs4270_open(AUDIO_CODEC_CTRL);
    }

    i2s_rx_start(audio_codec);
    i2s_tx_start(audio_codec);

    // receive buffer from audio-input and send back to audio-output
    while(1)
    {
        // wait till there are a number of samples available
        while (i2s_rx_avail(audio_codec) < AUDIO_BUF_SIZE / 2)
        {
            __nop();
        }

        // make even, the same number of samples for both channels
        rx_size = i2s_rx_avail(audio_codec) & ~1;
        rx_size = rx_size > AUDIO_BUF_SIZE ? AUDIO_BUF_SIZE : rx_size;

        // read samples
        i2s_read16(audio_codec, audio_buf, rx_size);

        // wait till there is enough space available
        while (i2s_tx_avail(audio_codec) < rx_size)
        {
            __nop();
        }

        // write samples
        i2s_write16(audio_codec, audio_buf, rx_size);
    }
}

