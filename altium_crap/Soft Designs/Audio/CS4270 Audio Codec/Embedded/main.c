#include <drv_cs4270.h>
#include <drv_i2s.h>
#include "devices.h"

#define AUDIO_BUF_SIZE      500

int16_t audio_buf[AUDIO_BUF_SIZE];

cs4270_t * cs4270_drv;
i2s_t * i2s_drv;

void loopback_audio(void);

/*
 * main()
 */
void main(void)
{
    while (cs4270_drv == NULL)
    {
        cs4270_drv = cs4270_open(DRV_CS4270_0);
    }
    i2s_drv = i2s_open(DRV_I2S_1);

    i2s_rx_start(i2s_drv);
    i2s_tx_start(i2s_drv);

    while(1)
    {
        loopback_audio();
    }
}

/*
 * receive buffer from audio-input and send back to audio-output
 */
void loopback_audio(void)
{
    uint32_t rx_size;

    while (i2s_rx_avail(i2s_drv) < AUDIO_BUF_SIZE / 2)  // wait till there are a number of samples available
    {
        __nop();
    }
    rx_size = i2s_rx_avail(i2s_drv) & ~1;               // make even, the same number of samples for both channels

    rx_size = rx_size > AUDIO_BUF_SIZE ? AUDIO_BUF_SIZE : rx_size;

    i2s_read16(i2s_drv, audio_buf, rx_size);            // read samples

    while (i2s_tx_avail(i2s_drv) < rx_size)             // wait till there is enough space available
    {
        __nop();
    }
    i2s_write16(i2s_drv, audio_buf, rx_size);           // write samples
}



