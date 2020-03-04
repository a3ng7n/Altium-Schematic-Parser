

#ifndef AUDIO_MIXER_H
#define AUDIO_MIXER_H

#include "fractional.h"
#include "MixerControl.h"
#include "tsk3000_reg.h"
#define SAMPLE_RATE 22050
#define LIFESUPPORT_RATE 50

//---------------------------------------------------------------------
// IIR Filter Definition
//---------------------------------------------------------------------

// 3 Coeffs, unrolled version
typedef struct IIRFILT_tag {
    unsigned int sample_rate;     // rate in samples/sec
    int   Flags;         // Unused      //
    int Order;           // number of ACoeffs
    _fract cur_val;      // last calculated value
    _fract ACoeffs[3];   // pointer to a coeffs
    _fract BCoeffs[3];   // pointer to b coeffs
    _fract Buf[6];     // holds Order most recent input samples
} IIRFILT;

typedef IIRFILT * LPIIRFILT;

//---------------------------------------------------------------------
// Inline IIR Filter macro
//---------------------------------------------------------------------

#define ACOEFF(LOC,VAL) (LOC.ACoeffs[VAL])
#define BCOEFF(LOC,VAL) (LOC.BCoeffs[VAL])
#define IIRBUF(LOC,VAL) (LOC.Buf[VAL])

#define IIR_PROCESS(LOC,SAMPLE)                  \
        old = IIRBUF(LOC,0);                      \
        IIRBUF(LOC,0) = (SAMPLE);                 \
        mac = ((SAMPLE) * ACOEFF(LOC,0));         \
                                                  \
        cur = IIRBUF(LOC,1);                      \
        IIRBUF(LOC,1) = old;                      \
        mac += (old * ACOEFF(LOC,1));             \
                                                  \
        old = IIRBUF(LOC,2);                      \
        IIRBUF(LOC,2) = cur;                      \
        mac += (cur * ACOEFF(LOC,2));             \
                                                  \
        cur = IIRBUF(LOC,3);                      \
        old = IIRBUF(LOC,4);                      \
        mac += (cur * BCOEFF(LOC,1));             \
                                                  \
        mac += (old * BCOEFF(LOC,2));             \
        mac >>= IIRscale;                         \
                                                  \
        IIRBUF(LOC,3) = (int)mac;                 \
        IIRBUF(LOC,4) = cur;                      \
        IIRBUF(LOC,5) = old;                      \
        result = mac;


//---------------------------------------------------------------------
// Delay Definition
//---------------------------------------------------------------------

typedef struct DELAY_tag {
    int maxsize;      // maximum delay length
    int cur_delay;    // delay in samples
    _fract * Buffer;  // pointer to buffer
    int head;         // current head
    int tail;         // current tail
    _fract Feedback;  // fractional feedback
} DELAY;

typedef DELAY * LPDELAY;

//---------------------------------------------------------------------
// Inline Delay Macros
//---------------------------------------------------------------------

#define DELAYBUF(LOC,INDEX) (LOC.Buffer[(INDEX)])

#define DELAY_PROCESS(LOC,SAMPLE)                                           \
        result = DELAYBUF(LOC,LOC.tail++);                                  \
        DELAYBUF(LOC,LOC.head++) = SAMPLE + ((result*LOC.Feedback)>>15);    \
        if (LOC.tail >= LOC.maxsize)                                        \
          LOC.tail = 0;                                                     \
        if (LOC.head >= LOC.maxsize)                                        \
          LOC.head = 0;

//---------------------------------------------------------------------
// Reverb Unit Definition
//---------------------------------------------------------------------

typedef struct REVERB_tag {
    int         IN;
    int         LowpassIN;
    IIRFILT     Lowpass;
    DELAY       delay1;
    DELAY       delay2;
} REVERB;

typedef REVERB * LPREVERB;


//---------------------------------------------------------------------
// Audio Mixer Definition
//---------------------------------------------------------------------


typedef struct tag_MixerChannel {
    _fract SampleGain;
    _fract ChannelGain;
    _fract Left;
    _fract Right;
    _fract ReverbSend;
    _fract leftgain;
    _fract rightgain;
    _fract reverbgain;
    int   Solo;
    int   Mute;
} MixerChannel;

#define delay1size 8192
#define delay2size 4096

typedef struct tag_AudioMixer {
    MixerChannel Channel[4];   // Four Input Channels for MOD Channels
    int          SoloCount;    // number of solo buttons currently pressed

    REVERB       Reverb;

    _fract       MasterGain;
    _fract       ReverbLeft;
    _fract       ReverbRight;
}  AudioMixer;

//---------------------------------------------------------------------
// Incoming Audio Sample
//---------------------------------------------------------------------
// PackedAudio.ui contains 4 8bit signed audio samples, i.e. mod player channels

typedef union {
    unsigned int ui;
    char channels[4];
} PackedAudio;


void AUDIOMIXER_Process();

void AUDIOMIXER_UpdateChannel(int channel);

#ifdef EXTERNAL_CONTROL
void AUDIOMIXER_SetChannelSampleGain(int channel, _fract gain);

void AUDIOMIXER_SetChannelGain(int channel, _fract gain);

void AUDIOMIXER_SetPan(int channel, _fract Pan);

void AUDIOMIXER_SetReverbSend(int channel,_fract gain);

void AUDIOMIXER_SetChannel(int channel,_fract SampleGain,_fract ChannelGain,_fract Pan,_fract ReverbSend);

void AUDIOMIXER_SetReverbPan(_fract Pan);

void ReceiveMixerCommand(LPMixerControl pM);

#endif

void StartAudio(unsigned int SampleFrequency);

void AUDIOMIXER_Init( void );


#endif
