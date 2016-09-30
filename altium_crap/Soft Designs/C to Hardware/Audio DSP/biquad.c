//
// Formulae to calculate the coefficents for the audio EQ biquad filter
// can be found at http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt
//

#include "process_audio.h"
#include "clip.h"

#define BPF_COEF      0              // Select between band pass or peakingEQ coefficients
#define NUM_BANDS     16
#define NUM_CHANNELS  2

__output bool CLIP_ERROR_BQ;         // Linked to "Error Mix led" on Equalizer VI

#if BPF_COEF
    // ype = BPF, 3 octave, start at 25 Hz, 44100Hz sample rate
    static const coefficient_t b_0[NUM_BANDS] = {
        0x0003,    0x0005,    0x0008,    0x000D,    0x0015,    0x0021,    0x0035,    0x0054,
        0x0084,    0x00CE,    0x013E,    0x01E4,    0x02CF,    0x0411,    0x05DA};
    static const coefficient_t b_1[NUM_BANDS] = {
        0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,
        0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000};
    static const coefficient_t b_2[NUM_BANDS] = {
        0xFFFD,    0xFFFB,    0xFFF8,    0xFFF3,    0xFFEB,    0xFFDF,    0xFFCB,    0xFFAC,
        0xFF7C,    0xFF32,    0xFEC2,    0xFE1C,    0xFD31,    0xFBEF,    0xFA26};
    static const coefficient_t a_1[NUM_BANDS] = {
        0xE007,    0xE00B,    0xE012,    0xE01C,    0xE02E,    0xE04A,    0xE079,    0xE0CB,
        0xE15B,    0xE267,    0xE467,    0xE850,    0xEFDA,    0xFD11,    0x0D7C};
    static const coefficient_t a_2[NUM_BANDS] = {
        0x0FF9,    0x0FF5,    0x0FEE,    0x0FE4,    0x0FD5,    0x0FBC,    0x0F94,    0x0F56,
        0x0EF6,    0x0E62,    0x0D82,    0x0C37,    0x0A60,    0x07DD,    0x044B};
#else
     // type = PEQ, 2/3 octave, start at 25 Hz, 44100Hz sample rate
    static const coefficient_t b_0[NUM_BANDS] = {
        0x0003,    0x0005,    0x0008,    0x000D,    0x0015,    0x0021,    0x0035,    0x0054,
        0x0084,    0x00CE,    0x013E,    0x01E4,    0x02CF,    0x0411,    0x05DA};
    static const coefficient_t b_1[NUM_BANDS] = {
        0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,
        0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000,    0x0000};
    static const coefficient_t b_2[NUM_BANDS] = {
        0xFFFD,    0xFFFB,    0xFFF8,    0xFFF3,    0xFFEB,    0xFFDF,    0xFFCB,    0xFFAC,
        0xFF7C,    0xFF32,    0xFEC2,    0xFE1C,    0xFD31,    0xFBEF,    0xFA26};
    static const coefficient_t a_1[NUM_BANDS] = {
        0xE007,    0xE00B,    0xE012,    0xE01C,    0xE02E,    0xE04A,    0xE079,    0xE0CB,
        0xE15B,    0xE267,    0xE467,    0xE850,    0xEFDA,    0xFD11,    0x0D7C};
    static const coefficient_t a_2[NUM_BANDS] = {
        0x0FF9,    0x0FF5,    0x0FEE,    0x0FE4,    0x0FD5,    0x0FBC,    0x0F94,    0x0F56,
        0x0EF6,    0x0E62,    0x0D82,    0x0C37,    0x0A60,    0x07DD,    0x044B};
#endif

sample_device_t BIQUAD(sample_device_t INP, channel_t CHANNEL, band_t BAND)
{
    // history buffers
    static sample_device_t  xnm1[NUM_CHANNELS][NUM_BANDS] = {{0}};
    static sample_device_t  xnm2[NUM_CHANNELS][NUM_BANDS] = {{0}};
    static sample_long_t    ynm1[NUM_CHANNELS][NUM_BANDS] = {{0}};
    static sample_long_t    ynm2[NUM_CHANNELS][NUM_BANDS] = {{0}};

    sample_long_t           processed;
    sample_device_t         retval;
    bool                    clip_error;

    sample_device_t    x1 = xnm1[CHANNEL][BAND];
    sample_device_t    x2 = xnm2[CHANNEL][BAND];
    sample_long_t      y1 = ynm1[CHANNEL][BAND];
    sample_long_t      y2 = ynm2[CHANNEL][BAND];

    processed =    INP * b_0[BAND] +
                    x1 * b_1[BAND] +
                    x2 * b_2[BAND] -
                    y1 * a_1[BAND] -
                    y2 * a_2[BAND];

    processed /= 1 << COEFFICIENT_SHIFT;

    retval = clip_long_to_device(processed, &clip_error);
    CLIP_ERROR_BQ = clip_error;

    xnm2[CHANNEL][BAND] = x1;
    xnm1[CHANNEL][BAND] = INP;
    ynm2[CHANNEL][BAND] = y1;
    ynm1[CHANNEL][BAND] = processed;

    return retval;
}
