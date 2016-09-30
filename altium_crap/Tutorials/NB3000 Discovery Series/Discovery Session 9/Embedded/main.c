#include <stdint.h>
#include <devices.h>
#include <audio.h>
#include <drv_led.h>
#include <led_info.h>

#define AUDIO_BUF_SIZE      512
#define LEFT                0
#define RIGHT               1

/* bufffers */
// audio
int16_t stereo_buf[AUDIO_BUF_SIZE] = {0};
int16_t audio_buf_l[AUDIO_BUF_SIZE] = {0};
int16_t audio_buf_r[AUDIO_BUF_SIZE] = {0};
// contexts for drivers
audio_t *audio;
led_t   *leds;

/* function prototypes */
int get_audio(int16_t *buffer, int size);
int put_audio(int16_t *buffer, int n);
uint8_t abs_ave(int16_t *buffer, int n);
void update_intensity(uint8_t intensity, uint8_t channel);

void main(void)
{
    int i, j;
    audio = audio_open(AUDIO_1);
    leds  = led_open(LEDS);
    while (1)
    {
        // get audio to left and right buffers for processing.
        get_audio(stereo_buf, AUDIO_BUF_SIZE);
        for (i = 0, j = 0; i < AUDIO_BUF_SIZE ; i++, j++)
        {
          audio_buf_r[j] = stereo_buf[i++];
          audio_buf_l[j] = stereo_buf[i];
        }

        // Loop left and right channels through.
        put_audio(stereo_buf, AUDIO_BUF_SIZE);

        // Put the D.C. average value of the wavelet on LEDs
        update_intensity(abs_ave(audio_buf_l, AUDIO_BUF_SIZE/2),LEFT);
        update_intensity(abs_ave(audio_buf_r, AUDIO_BUF_SIZE/2),RIGHT);
    }
}


/**********************************************************************
|*
|*  FUNCTION    : get_audio
|*
|*  PARAMETERS  : buffer = pointer to audio buffer
|*                n = number of samples to read
|*
|*  RETURNS     : Number of samples actually received in buffer
|*
|*  DESCRIPTION : Receive buffer from audio-input
 */

int get_audio(int16_t *buffer, int n)
{
    int s;

    do
    {
        s = audio_record(audio, buffer, n);
        n -= s;
        buffer += s;
    }while (n != 0);

    return s;
}


/**********************************************************************
|*
|*  FUNCTION    : put_audio
|*
|*  PARAMETERS  : buffer = pointer to audio stream
|*                n = Number of samples to write
|*
|*  RETURNS     : Number of samples actually transmitted
|*
|*  DESCRIPTION : Send buffer to audio-output
 */

int put_audio(int16_t *buffer, int n)
{
    int s;

    do
    {
        s = audio_play(audio, buffer, n);
        n -= s;
        buffer += s;
    } while (n != 0);

    return 0;
}

/**********************************************************************
|*
|*  FUNCTION    : abs_ave
|*
|*  PARAMETERS  : buffer = pointer to audio wavelet
|*                n = number of samples to use for calculation
|*
|*  RETURNS     : 8-bit value representing audio average volume
|*
|*  DESCRIPTION : Finds the average of the absolute values of a sample
|*                buffer and scales down to unsigned 8-bit for VU meter.
 */

uint8_t abs_ave(int16_t *buffer, int n)
{
    int16_t cusum = 0;
    for (int i = 0; i < n; i++)
    {
        cusum += (buffer[i] < 0) ? -(buffer[i]/n) : buffer[i]/n;
    }
    return (uint8_t)(cusum>>5);
}

/**********************************************************************
|*
|*  FUNCTION    : update_intensity
|*
|*  PARAMETERS  : intensity = unsigned value rep. signal strength
|*                channel = 0 = LEFT, 1 = RIGHT.
|*
|*  DESCRIPTION : Lights 4 LEDs in sequence depending on intensity & channel
|*
 */

void update_intensity(uint8_t intensity, uint8_t channel)
{
    uint8_t b0 = 0;
    uint8_t b1 = 0;
    uint8_t b2 = 0;
    uint8_t b3 = 0;
    if (intensity > 0xF0)
    {
        b0 = b1 = b2 = b3 = 0xFF;
    } else {
        if (intensity > 0xC0)
        {
            b0 = b1 = b2 = 0xFF;
            b3 = 0;
        } else {
            if (intensity > 0xA0)
            {
                b0 = b1 = 0xFF;
                b2 = b3 = 0;
            } else {
                if (intensity > 0x40)
                {
                    b0 = 0xFF;
                    b1 = b2 = b3 = 0;
                } else {
                    if (intensity < 0x10)
                    {
                        b0 = b1 = b2 = b3 = 0;
                    }
                    // if not down to 0x10 then no change.
                }
            }
        }
    }
    switch (channel)
    {
        case LEFT:
             led_set_intensity(leds, LEDS_LED7, b3);
             led_set_intensity(leds, LEDS_LED6, b2);
             led_set_intensity(leds, LEDS_LED5, b1);
             led_set_intensity(leds, LEDS_LED4, b0);
             break;
        case RIGHT:
             led_set_intensity(leds, LEDS_LED0, b3);
             led_set_intensity(leds, LEDS_LED1, b2);
             led_set_intensity(leds, LEDS_LED2, b1);
             led_set_intensity(leds, LEDS_LED3, b0);
             break;
        default: break;
    }
}

