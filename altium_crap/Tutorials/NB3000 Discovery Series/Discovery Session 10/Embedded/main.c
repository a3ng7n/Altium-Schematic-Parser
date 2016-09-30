#include <stdint.h>
#include <devices.h>
#include <audio.h>
#include <drv_led.h>
#include <led_info.h>

#define AUDIO_BUF_SIZE      512

#define LEFT  0
#define RIGHT 1

/* bufffers */
// audio
int16_t stereo_buf[AUDIO_BUF_SIZE] = {0};
int16_t audio_buf_l[AUDIO_BUF_SIZE/2] = {0};
int16_t audio_buf_r[AUDIO_BUF_SIZE/2] = {0};
// contexts for drivers
audio_t *audio;
led_t   *leds;
uint32_t intensity_l, intensity_r;

/* function prototypes */
int get_audio(int16_t *buffer, int size);
int put_audio(int16_t *buffer, int n);
uint32_t abs_ave(int16_t *buffer, int n);
void update_intensity(uint32_t intensity, uint8_t channel);

/**********************************************************************
|*
|*  FUNCTION    : main
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Start here
 */

void main(void)
{
    int i, j;
    audio = audio_open(AUDIO_1);
    leds  = led_open(LEDS);
    led_set_all_on_intensity(leds, 0);
    led_turn_all_on(leds);

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

        // Get the D.C. average value of the wavelet.
        intensity_l = abs_ave(audio_buf_l, AUDIO_BUF_SIZE/2);
        intensity_r = abs_ave(audio_buf_r, AUDIO_BUF_SIZE/2);

        // Set VU meter on LEDS
        update_intensity(intensity_l, LEFT);
        update_intensity(intensity_r, RIGHT);
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

uint32_t abs_ave(int16_t *buffer, int n)
{
    int32_t cusum = 0;
    for (int i = 0; i < n; i++)
    {
        cusum += (buffer[i] < 0) ? -(buffer[i]) : buffer[i];
    }
    return cusum<<8;
}


/**********************************************************************
|*
|*  FUNCTION    : update_intensity
|*
|*  PARAMETERS  : intensity = 32-bit average wavelet intensity value
|*                channel   = 0 = LEFT, 1 = RIGHT, else don't care
|*
|*  DESCRIPTION : Updates LED intensity registers based on average
|*                sound value
 */

void update_intensity(uint32_t intensity, uint8_t channel)
{
    uint8_t bred, byel, bgrn2, bgrn;
    if (intensity > 0x8ffffff)
    {
        bred = 0xFF;
        byel = 0xFF;
        bgrn2 = bgrn = 0x80;
    } else {
        if (intensity > 0x4ffffff)
        {
           bred = 0;
           byel = 0x80;
           bgrn2 = bgrn = 0x80;
        } else {
           if (intensity > 0x2000000)
           {
              bred = 0;
              byel = 0;
              bgrn2 = 0x80;
              bgrn = 0xFF;
           } else {
              if (intensity > 0x0200000)
              {
                bred = 0;
                byel = 0;
                bgrn2 = 0;
                bgrn  = 0x40;
              } else {
                bred = byel = bgrn = bgrn2 = 0;
              }
           }
        }
    }
    switch (channel)
    {
        case (LEFT):
        {
             led_set_intensity(leds, LEDS_LED7_R, bred);
             led_set_intensity(leds, LEDS_LED6_R, byel);
             led_set_intensity(leds, LEDS_LED6_G, byel/2);
             led_set_intensity(leds, LEDS_LED5_G, bgrn2);
             led_set_intensity(leds, LEDS_LED4_G, bgrn);
             break;
        }
        case (RIGHT):
        {
             led_set_intensity(leds, LEDS_LED0_R, bred);
             led_set_intensity(leds, LEDS_LED1_R, byel);
             led_set_intensity(leds, LEDS_LED1_G, byel/2);
             led_set_intensity(leds, LEDS_LED2_G, bgrn2);
             led_set_intensity(leds, LEDS_LED3_G, bgrn);
             break;
        }
        default:
        {
            break;
        }
    }
}
