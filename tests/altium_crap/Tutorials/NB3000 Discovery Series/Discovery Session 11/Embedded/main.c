#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <devices.h>
#include <audio.h>
#include <drv_led.h>
#include <led_info.h>
#include "IIR.h"

#define PI                  3.1415926535897932384626433832795
#define AUDIO_BUF_SIZE      512
#define FSAMPLE             44100
#define BASS_Hz             64
#define MID_Hz              1024
#define TREB_Hz             8192

// Coeffs for band-pass IIR filters, FS = 44100, Q = 1.4, 16-bit Fixed Point
int16_t alpha_hi, beta_hi, gamma_hi,
        alpha_mid, beta_mid, gamma_mid,
        alpha_lo, beta_lo, gamma_lo;

//coe2pol_t iir buffers;
// Left Channel
int16_t   x_l_lo[2] = {0};
int16_t   x_l_mid[2] = {0};
int16_t   x_l_hi[2] = {0};
int16_t   y_l_lo[2] = {0};
int16_t   y_l_mid[2] = {0};
int16_t   y_l_hi[2] = {0};
// Right Channel
int16_t   x_r_lo[2] = {0};
int16_t   x_r_mid[2] = {0};
int16_t   x_r_hi[2] = {0};
int16_t   y_r_lo[2] = {0};
int16_t   y_r_mid[2] = {0};
int16_t   y_r_hi[2] = {0};

// for internal loop.
int i, j;

/* bufffers */
// Hi/Mid/Low filter output buffers
// Left Channel
int16_t bass_buf_l[AUDIO_BUF_SIZE/2] = {0};
int16_t mid_buf_l [AUDIO_BUF_SIZE/2] = {0};
int16_t hi_buf_l  [AUDIO_BUF_SIZE/2] = {0};
// Right Channel
int16_t bass_buf_r[AUDIO_BUF_SIZE/2] = {0};
int16_t mid_buf_r [AUDIO_BUF_SIZE/2] = {0};
int16_t hi_buf_r  [AUDIO_BUF_SIZE/2] = {0};

// Audio pass-through buffer
int16_t stereo_buf[AUDIO_BUF_SIZE] = {0};

// contexts for drivers
audio_t *audio;
led_t   *leds;
uint8_t rgb_v[3] = {0};

/* function prototypes */
int get_audio(int16_t *buffer, int size);
int put_audio(int16_t *buffer, int n);
uint8_t abs_ave(int16_t *buffer, int n);
void update_coeffs ( double frequency, double qf,
                     int16_t * ialpha, int16_t * ibeta, int16_t * igamma );
void update_intensity(uint8_t intensity, uint8_t * rgb);


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
    audio = audio_open(AUDIO_1);
    leds  = led_open(LEDS);
    update_coeffs(BASS_Hz, 1.4, &alpha_lo,  &beta_lo,  &gamma_lo );
    update_coeffs(MID_Hz,  1.4, &alpha_mid, &beta_mid, &gamma_mid);
    update_coeffs(TREB_Hz, 1.4, &alpha_hi,  &beta_hi,  &gamma_hi );

    while (1)
    {
       // get audio to left and right buffers for processing.
       get_audio(stereo_buf, AUDIO_BUF_SIZE);

       // Loop left and right channels through.
       put_audio(stereo_buf, AUDIO_BUF_SIZE);

       for (i = 0, j = 0; i < AUDIO_BUF_SIZE; i++, j++)
       {
           // Split left and right channels, pass through filters
           bass_buf_r[j] = do_iir( stereo_buf[i], alpha_lo,  beta_lo,
                                   gamma_lo,  x_l_lo,  y_l_lo );
           mid_buf_r[j]  = do_iir( stereo_buf[i], alpha_mid, beta_mid,
                                   gamma_mid, x_l_mid, y_l_mid);
           hi_buf_r[j]   = do_iir( stereo_buf[i], alpha_hi,  beta_hi,
                                   gamma_hi,  x_l_hi,  y_l_hi );
           i++; // Next Channel
           bass_buf_l[j] = do_iir( stereo_buf[i], alpha_lo,  beta_lo,
                                   gamma_lo,  x_r_lo,  y_r_lo );
           mid_buf_l[j]  = do_iir( stereo_buf[i], alpha_mid, beta_mid,
                                   gamma_mid, x_r_mid, y_r_mid);
           hi_buf_l[j]   = do_iir( stereo_buf[i], alpha_hi,  beta_hi,
                                   gamma_hi,  x_r_hi,  y_r_hi );
       }

       // Put the D.C. average value of the wavelet on LEDs
       update_intensity(abs_ave(bass_buf_r, AUDIO_BUF_SIZE/8), rgb_v);
       led_set_intensity(leds, LEDS_LED0_R, rgb_v[0]);
       led_set_intensity(leds, LEDS_LED0_G, rgb_v[1]);
       led_set_intensity(leds, LEDS_LED0_B, rgb_v[2]);

       update_intensity(abs_ave(mid_buf_r , AUDIO_BUF_SIZE/8), rgb_v);
       led_set_intensity(leds, LEDS_LED1_R, rgb_v[0]);
       led_set_intensity(leds, LEDS_LED1_G, rgb_v[1]);
       led_set_intensity(leds, LEDS_LED1_B, rgb_v[2]);

       update_intensity(abs_ave(hi_buf_r  , AUDIO_BUF_SIZE/8), rgb_v);
       led_set_intensity(leds, LEDS_LED2_R, rgb_v[0]);
       led_set_intensity(leds, LEDS_LED2_G, rgb_v[1]);
       led_set_intensity(leds, LEDS_LED2_B, rgb_v[2]);

       update_intensity(abs_ave(bass_buf_l, AUDIO_BUF_SIZE/8), rgb_v);
       led_set_intensity(leds, LEDS_LED7_R, rgb_v[0]);
       led_set_intensity(leds, LEDS_LED7_G, rgb_v[1]);
       led_set_intensity(leds, LEDS_LED7_B, rgb_v[2]);

       update_intensity(abs_ave(mid_buf_l , AUDIO_BUF_SIZE/8), rgb_v);
       led_set_intensity(leds, LEDS_LED6_R, rgb_v[0]);
       led_set_intensity(leds, LEDS_LED6_G, rgb_v[1]);
       led_set_intensity(leds, LEDS_LED6_B, rgb_v[2]);

       update_intensity(abs_ave(hi_buf_l  , AUDIO_BUF_SIZE/8), rgb_v);
       led_set_intensity(leds, LEDS_LED5_R, rgb_v[0]);
       led_set_intensity(leds, LEDS_LED5_G, rgb_v[1]);
       led_set_intensity(leds, LEDS_LED5_B, rgb_v[2]);
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
    return (uint8_t)(cusum>>2);
}

/**********************************************************************
|*
|*  FUNCTION    : update_coeffs
|*
|*  DESCRIPTION : updates alpha, beta and gamma IIR coefficients
|*                for left and right channel filters.
|*
 */

void update_coeffs ( double frequency, double qf,
                     int16_t * ialpha, int16_t * ibeta, int16_t * igamma )
{
    double alpha;
    double beta;
    double gamma;
    double theta;
    theta =  2 * (double)PI * (frequency/FSAMPLE);
    beta = 0.5 * ((1 - tan(theta/2*qf))/(1 + tan(theta/2*qf)));
    gamma = (0.5 + beta)*cos(theta);
    alpha = (0.5 - beta)/2;
    *ialpha = (int16_t)(alpha * 32767);
    *ibeta  = (int16_t)(beta  * 32767);
    *igamma = (int16_t)(gamma * 32767);
}

/**********************************************************************
|*
|*  FUNCTION    : update_intensity
|*
|*  PARAMETERS  : intensity = 8-bit average wavelet intensity value
|*                rgb       = UINT8_T * RGB value
|*                            (pointer to red, green and blue).
|*
|*  DESCRIPTION : Updates LED intensity registers based on average
|*                sound value
 */

void update_intensity(uint8_t intensity, uint8_t * rgb)
{
    if (intensity < 0x08)
    {
        rgb[0] = 0;
        rgb[1] = 0;
        rgb[2] = 0;
    } else
    {
        rgb[0] = 0x80 - intensity < 0    ? 0 : 0x80 - intensity;
        rgb[1] = 0x40 - intensity < 0    ? 0 : 0x40 - intensity/2;
        rgb[2] =        intensity < 0x80 ? 0 : intensity/2 - 0x40;
    }
}

