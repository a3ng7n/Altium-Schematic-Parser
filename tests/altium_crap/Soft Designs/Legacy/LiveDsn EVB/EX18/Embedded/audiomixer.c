

#include "hware.h"
#include "hardware.h"
#include "math.h"

#include "AudioMixer.h"

#ifdef EXTERNAL_CONTROL
#pragma section "MixerControl"
MixerControl MixCtrl ={0,0,0,0};
#pragma endsection
#endif

__sdata PackedAudio PA;

volatile unsigned int TickTimer;
volatile unsigned int TimeOutTimer;

//---------------------------------------------------------------------
// Audio Mixer Data
//---------------------------------------------------------------------


// Associated Memory
__sdata _fract delay1buffer[delay1size];
__sdata _fract delay2buffer[delay2size];

__sdata AudioMixer AM = {
    {
       {
       floatf(0.9999999999999),   // Sample Gain
       floatf(0.9999999999999),   // Channel Gain
       floatf(0.8), // Left;
       floatf(0.2), // Right;
       floatf(0.4), // ReverbSend;
       0, // leftgain
       0, // rightgain
       0, // reverbgain
       0,                   //   Solo;
       0},                  //   Mute;
       {
       floatf(0.9999999999999),   // Sample Gain
       floatf(0.9999999999999),   // Channel Gain
       floatf(0.2), // Left;
       floatf(0.8), // Right;
       floatf(0.0), // ReverbSend;
       0, // leftgain
       0, // rightgain
       0, // reverbgain
       0,                   //   Solo;
       0},                  //   Mute;
       {
       floatf(0.9999999999999),   // Sample Gain
       floatf(0.9999999999999),   // Channel Gain
       floatf(0.2), // Left;
       floatf(0.8), // Right;
       floatf(0.0), // ReverbSend;
       0, // leftgain
       0, // rightgain
       0, // reverbgain
       0,                   //   Solo;
       0},                  //   Mute;
       {
       floatf(0.9999999999999),   // Sample Gain
       floatf(0.9999999999999),   // Channel Gain
       floatf(0.8), // Left;
       floatf(0.2), // Right;
       floatf(0.4), // ReverbSend;
       0, // leftgain
       0, // rightgain
       0, // reverbgain
       0,                   //   Solo;
       0}                  //   Mute;
    },

    {0},                   // SoloCount

    // Reverb Unit
    {
       {1},             // in or out, 0 = out
       {1},              // lowpass in or out , 0 = out
       // IIR
       {
          SAMPLE_RATE,               // rate in samples/sec
          0,                   // Unused      //
          3,                   // number of ACoeffs
          0,                   // last calculated value
          // A coeffs
          {
             floatf_IIR( 0.004398985238245204200),
             floatf_IIR( 0.008797970476490408300),
             floatf_IIR( 0.004398985238245204200)
          },
          // B coeffs
          {
             0,          // not needed, always 0
             floatf_IIR( 1.848754449785037900000),
             floatf_IIR(-0.866350390738018340000)
          },
          {0,0,0,0,0,0}   // Buffers
       },

          // delay1
       {
          delay1size,          // maximum delay length
          delay1size,          // delay in samples
          &delay1buffer[0],    // pointer to buffer
          0,                   // current head
          0,                   // current tail
          floatf(0.2)          // fractional feedback
       },

          // delay2
       {
          delay2size,          // maximum delay length
          delay2size,          // delay in samples
          &delay2buffer[0],    // pointer to buffer
          0,                   // current head
          0,                   // current tail
          floatf(0.2)          // fractional feedback
       }
    },

    floatf(0.9999999999),   // MasterGain;
    floatf(0.8),   // ReverbLeft;
    floatf(0.8)   // ReverbRight;
};

void DELAY_SetDelay(LPDELAY d,int samples)
{
    // set delay in samples
    if (samples < 1)
       samples = 1;
    if (d->maxsize < samples)
       samples = d->maxsize;
    d->cur_delay = samples;
    d->tail = d->head - d->cur_delay;
    if (d->tail < 0)
       d->tail = d->maxsize + d->tail;
}

void DELAY_SetDelayByFract(LPDELAY d,_fract fract)
{
    DELAY_SetDelay(d,mpy(d->maxsize,fract));
}

#ifdef DEBUG
_fract Min=0;
_fract Max=0;
#endif

#pragma section "FastSection"

_fract REVERB_Process_U(_fract sample)
{

    register _fract result;
    register _fract Res;
    register _fract old,cur;
    _fract mac = 0;

//    if (sample < Min) Min = sample;
//    if (sample > Max) Max = sample;

    if (AM.Reverb.IN)
    {

      if (AM.Reverb.LowpassIN)
      {
          IIR_PROCESS(AM.Reverb.Lowpass,sample);
          Res = result;
      }
      else
          Res = sample;

      DELAY_PROCESS(AM.Reverb.delay1,Res);
      Res = result;
      DELAY_PROCESS(AM.Reverb.delay2,Res);
      Res += result;

 //   if (Res < Min) Min = Res;
 //   if (Res > Max) Max = Res;

      return Res;
    }
    else
        return 0;
}


#define BG_PEAK_DECAY 40       // decay time for 1 bar peak hold in 20ms ticks
#define BG_CURRENT_DECAY 0     // decay time for 1 bar current value in 20ms ticks
#define BG_DB_STEPS -1.5       // steps in dB from one bar to next
#define BG_STEP_FACTOR (BG_DB_STEPS/20.0)
#define BG_MAX_VALUE 0x7FFF    // full range value
#define BG_STEPS  12           // number of bars (leave at 12)

static __sdata unsigned int BarGraphTable[BG_STEPS];

//-------------------------------------------------
// initialises the compare table for bargraph
//-------------------------------------------------
static void Init_BarGraphTable(unsigned int MaxValue, double dB_Step)
{
   signed int i;
   unsigned int CurrentValue;
   CurrentValue = MaxValue;
   for (i=BG_STEPS-1; i >=0; i--)
   { CurrentValue = CurrentValue * pow(10,(dB_Step/20.0));
     BarGraphTable[i] = CurrentValue;
   }
}

unsigned int BarPosition(unsigned short int Value)
{
    unsigned int i;
    for(i=0; i < BG_STEPS ;i++)
    {
      if(Value < BarGraphTable[i])
        return i;
    }
    return 0;
}

void  UpdateBarGraph(unsigned short int InLeft, unsigned short int InRight)
{
    static unsigned short int PDecayCounter = BG_PEAK_DECAY;
    static unsigned short int CDecayCounter = BG_CURRENT_DECAY;
    static unsigned short int PeakL, PeakR;
    static unsigned short int CurL, CurR;
    unsigned short int CurPosR, CurPosL;
    unsigned short int CurOutL, CurOutR;
    unsigned short int PeakOutL, PeakOutR;
    unsigned int OutValue;
    unsigned int i;
    if(PDecayCounter-- == 0)   // handle decay for peak value
    {
       if(PeakL) PeakL--;
       if(PeakR) PeakR--;
       PDecayCounter = BG_PEAK_DECAY;
    }
    if(CDecayCounter-- == 0)   // handle decay for peak value
    {
       if(CurL) CurL--;
       if(CurR) CurR--;
       CDecayCounter = BG_CURRENT_DECAY;
    }
    CurPosR = BarPosition(InRight);
    if(CurR < CurPosR)  CurR  = CurPosR;
    if(PeakR < CurR) PeakR = CurR;
    CurPosL = BarPosition(InLeft);
    if(CurL < CurPosL)  CurL  = CurPosL;
    if(PeakL < CurL) PeakL = CurL;
    if(PeakL) PeakOutL = 1 << PeakL;
    if(PeakR) PeakOutR = 1 << PeakR;
    CurOutR = CurOutL = 0;
    for(i=0;i<CurR;i++)
    {
       CurOutR <<= 1;
       CurOutR  |= 1;
    }
    for(i=0;i<CurL;i++)
    {
       CurOutL <<= 1;
       CurOutL  |= 1;
    }
//    PeakOutL = PeakOutR = 0;
//    CurOutL = 0; CurOutR = 0;
    OutValue = (((unsigned int)(CurOutL | PeakOutL)) <<16) | (unsigned int)(CurOutR | PeakOutR);
    PAR_REG(OFS_BARGRAPH) = OutValue;
}

__sdata unsigned int PrevSample = 0;
__sdata unsigned int MixerLifeSupport = SAMPLE_RATE/50;

void __interrupt(0) AUDIOMIXER_Process()
{
    register _fract Left = 0;
    register _fract Right = 0;
    register _fract ReverbSend = 0;
    register _fract signal;
    static unsigned int PeakLeft,PeakRight;
//    unsigned int SPI_Out;

    // 1. Output Previous Sample with no jitter

    PAR_REG(OFS_DSP) = PrevSample;   // write L+R channels to DeltaSigma DACs (if installed)

         // Make a Mono 8 bit signal and send it to the NB1 MAX1104 audio DAC

//    SPI_Out = ((PrevSample & 0xFFFF0000) >> 16);  // add left and right channel
//    SPI_Out +=((PrevSample & 0x0000FFFF));
//    SPI_Out >>= 9;  // scale back to 8 bits
//    SPI_WB_SendReceive(Base_Audio_SPI,(unsigned char)SPI_Out);

    ResetIntervalTimer();

    // 2. Obtain New Samples from Mod Player

    PA.ui = PAR_REG(OFS_DSP); // PA.ui contains 4 8bit signed audio samples, i.e. mod player channels

    // 3. Process Input Channels

    signal = PA.channels[0] << 8 ;
    Left  += signal*AM.Channel[0].leftgain;
    Right += signal*AM.Channel[0].rightgain;
    ReverbSend += signal*AM.Channel[0].reverbgain;

    signal = PA.channels[1] << 8 ;
    Left  += signal*AM.Channel[1].leftgain;
    Right += signal*AM.Channel[1].rightgain;
    ReverbSend += signal*AM.Channel[1].reverbgain;

    signal = PA.channels[2] << 8 ;
    Left  += signal*AM.Channel[2].leftgain;
    Right += signal*AM.Channel[2].rightgain;
    ReverbSend += signal*AM.Channel[2].reverbgain;

    signal = PA.channels[3] << 8;
    Left  += signal*AM.Channel[3].leftgain;
    Right += signal*AM.Channel[3].rightgain;
    ReverbSend += signal*AM.Channel[3].reverbgain;


    // 3.Process Reverb

    ReverbSend >>= 15;   // scale mac
    signal = REVERB_Process_U(ReverbSend);
    Left  += signal*AM.ReverbLeft;
    Right += signal*AM.ReverbRight;

    // 4. Scale Outputs
    Left  >>= 15;
    Right >>= 15;


    // 4. Master Gain;
    Left = mpy(Left,AM.MasterGain);
    Right = mpy(Right,AM.MasterGain);

 //   if (Left < Min) Min = Left;
 //   if (Left > Max) Max = Left;

    // 5. Build Output Samples
    // two 16bit samples packed in an unsigned int
    // samples are converted from signed to unsigned for the delta sigma DACS

    {
       if( Left > 0)
         if(Left > PeakLeft) PeakLeft = Left;
       if( Right > 0)
         if(Right > PeakRight) PeakRight = Right;
    }
    PrevSample = ( ( (Left ^ 0x8000) << 16 ) | ( (Right ^ 0x8000) & 0x0000ffff )) ;

    // 6. Occasional life support
    if (MixerLifeSupport-- == 0)
    {
        MixerLifeSupport = SAMPLE_RATE/LIFESUPPORT_RATE;    // 50 times per second
        TickTimer++;
        if(TimeOutTimer) TimeOutTimer--;

        // ---------------------------------------------------
        // Test Code, remove for remote operation by PC      |
        // Read DIP Switch 8 for Filter Enable and           |
        //      DIP Switch 5 for Reverb Enable               |
        AM.Reverb.IN = (PAR_REG(OFS_KBD) & 0x1)?0:1;         //
        AM.Reverb.LowpassIN = (PAR_REG(OFS_KBD) & 0x2)?0:1;  //
        //----------------------------------------------------
#ifdef EXTERNAL_CONTROL
        ReceiveMixerCommand(&MixCtrl);
#endif
//        PeakLeft = PeakRight = 0x77;
        UpdateBarGraph((unsigned short)PeakLeft,(unsigned short)PeakRight);
        PeakLeft = PeakRight = 0;
    }
    ClearInterruptEdgeFlags(1);
}

#pragma endsection

void AUDIOMIXER_UpdateChannel(int channel)
{
    int signal = AM.SoloCount;

    if ((AM.Channel[channel].Mute == 0) && ((!signal) || (signal && AM.Channel[channel].Solo)))
    {
       signal = (floatf(0.4) * AM.Channel[channel].ChannelGain) >> 15;
       signal = (signal * AM.Channel[channel].SampleGain) >> 15;
       AM.Channel[channel].leftgain = (signal * AM.Channel[channel].Left) >> 15;
       AM.Channel[channel].rightgain = (signal * AM.Channel[channel].Right) >> 15;
       AM.Channel[channel].reverbgain = (signal * AM.Channel[channel].ReverbSend) >> 15;
    }
    else
    {
       AM.Channel[channel].leftgain = 0;
       AM.Channel[channel].rightgain = 0;
       AM.Channel[channel].reverbgain = 0;
    }
}

#ifdef EXTERNAL_CONTROL

void AUDIOMIXER_SetChannelGain(int channel, _fract gain)
{
    AM.Channel[channel].ChannelGain = gain;
    AUDIOMIXER_UpdateChannel(channel);
}

void AUDIOMIXER_SetChannelSampleGain(int channel, _fract gain)
{
    // This can be used by the MOD player to make channel gain changes
    // This command cannot be called by the PC host
    AM.Channel[channel].SampleGain = gain;
    AUDIOMIXER_UpdateChannel(channel);
}

void AUDIOMIXER_SetPan(int channel, _fract Pan)
{
    // sanity
    if (Pan < 0) Pan=0;
    if (Pan > FRACT_MAX) Pan = FRACT_MAX;

    AM.Channel[channel].Left = Pan;
    AM.Channel[channel].Right = floatf(0.999999999999999)-Pan;
    AUDIOMIXER_UpdateChannel(channel);
}

void AUDIOMIXER_SetReverbSend(int channel,_fract gain)
{
    AM.Channel[channel].ReverbSend = gain;
    AUDIOMIXER_UpdateChannel(channel);
}

void AUDIOMIXER_SetSolo(int channel,int value)
{
    if (AM.Channel[channel].Solo) AM.SoloCount--;
    AM.Channel[channel].Solo = value;
    if (value) AM.SoloCount++;
    for (channel=0;channel<4;channel++)
       AUDIOMIXER_UpdateChannel(channel);
}

void AUDIOMIXER_SetMute(int channel,int value)
{
    AM.Channel[channel].Mute = value;
    AUDIOMIXER_UpdateChannel(channel);
}


#define PAN_TAPER(PAN) (PAN) // linear for now, should be log/log-

void AUDIOMIXER_SetChannelProperties(int channel,_fract SampleGain,_fract ChannelGain,_fract Pan,_fract ReverbSend)
{
    AM.Channel[channel].SampleGain = SampleGain;
    AM.Channel[channel].ChannelGain = ChannelGain;
    AM.Channel[channel].Left = PAN_TAPER(Pan);
    AM.Channel[channel].Right = PAN_TAPER(floatf(0.999999999999999)-Pan);
    AM.Channel[channel].ReverbSend = ReverbSend;
    AUDIOMIXER_UpdateChannel(channel);
}

void AUDIOMIXER_SetReverbDelay1(_fract Delay)
{
    DELAY_SetDelayByFract(&AM.Reverb.delay1,Delay);
}

void AUDIOMIXER_SetReverbFeedback1(_fract Feedback)
{
    AM.Reverb.delay1.Feedback = Feedback;
}

void AUDIOMIXER_SetReverbDelay2(_fract Delay)
{
    DELAY_SetDelayByFract(&AM.Reverb.delay2,Delay);
}

void AUDIOMIXER_SetReverbFeedback2(_fract Feedback)
{
    AM.Reverb.delay2.Feedback = Feedback;
}

void AUDIOMIXER_SetReverbPan(_fract Pan)
{
    AM.ReverbLeft = PAN_TAPER(Pan);
    AM.ReverbRight = PAN_TAPER(floatf(0.999999999999999)-Pan);
}

void AUDIOMIXER_SetReverbEnable(int state)
{
    AM.Reverb.IN = state;
}

void AUDIOMIXER_SetReverbLowpassEnable(int state)
{
    AM.Reverb.LowpassIN = state;
}

void AUDIOMIXER_SetMasterGain(_fract Gain)
{
    AM.MasterGain = Gain;
}


// This is presently called fromn the 22kHz interrupt service routine - AUDIOMIXER_Process();

// Would be better polled from the foreground task

void ReceiveMixerCommand(LPMixerControl pM)
{
    register MixerControl M = *pM;
    _fract value;

    if (M.PendingFlag)
    {
       value = M.Value << 8;

       switch (M.FunctionCode)
       {
          case CHANNEL_GAIN:
             AUDIOMIXER_SetChannelGain(M.Channel,value);
             break;
          case CHANNEL_PAN:
             AUDIOMIXER_SetPan(M.Channel,value);
             break;
          case CHANNEL_REVERBSEND:
             AUDIOMIXER_SetReverbSend(M.Channel,value);
             break;
          case CHANNEL_SOLO:
             AUDIOMIXER_SetSolo(M.Channel,M.Value);
             break;
          case CHANNEL_MUTE:
             AUDIOMIXER_SetMute(M.Channel,M.Value);
             break;
          case REVERB_PAN:
             AUDIOMIXER_SetReverbPan(value);
             break;
          case REVERB_IN:
             AUDIOMIXER_SetReverbEnable(M.Value);
             break;
          case REVERB_LOWPASS:
             AUDIOMIXER_SetReverbLowpassEnable(M.Value);
             break;
          case REVERB_DELAY1:
             AUDIOMIXER_SetReverbDelay1(value);
             break;
          case REVERB_FEEDBACK1:
             AUDIOMIXER_SetReverbFeedback1(value);
             break;
          case REVERB_DELAY2:
             AUDIOMIXER_SetReverbDelay2(value);
             break;
          case REVERB_FEEDBACK2:
             AUDIOMIXER_SetReverbFeedback2(value);
             break;
          case MASTERGAIN:
             AUDIOMIXER_SetMasterGain(value);
             break;
       }
       pM->PendingFlag = 0;
    }
}

#endif

void StartAudio(unsigned int SampleFrequency)
{
    SetIntervalTimer(FCLK / SampleFrequency);
    SetEnabledInterrupts(1);
    EnableIntervalTimer();
    EnableInterrupts();
}


void AUDIOMIXER_Init( void )
{

    // This is functionally to reset the mixer to a default state
    // however
    // Without an external mixer controller this function provides the only
    // method to adjust Audio Mixer and Reverb state.

    int i;
    AM.SoloCount = 0;
    for (i=0;i<4;i++)
    {
       AM.Channel[i].Mute = 0;
       AM.Channel[i].Solo = 0;

#ifndef COMPLETE_BUILD
    AUDIOMIXER_UpdateChannel(i);
#endif

    }

#ifdef COMPLETE_BUILD
                                // channel   SampleGain             ChannelGain               Pan          Reverb Send
    AUDIOMIXER_SetChannelProperties(0,floatf(0.9999999999999),floatf(0.9999999999999),floatf(0.9999999999999),floatf(0.4));
    AUDIOMIXER_SetChannelProperties(1,floatf(0.9999999999999),floatf(0.9999999999999),floatf(0.0000000000000),floatf(0.0));
    AUDIOMIXER_SetChannelProperties(2,floatf(0.9999999999999),floatf(0.9999999999999),floatf(0.0000000000000),floatf(0.0));
    AUDIOMIXER_SetChannelProperties(3,floatf(0.9999999999999),floatf(0.9999999999999),floatf(0.9999999999999),floatf(0.4));

    AUDIOMIXER_SetReverbEnable(1);        // Enable the Reverb Unit
    AUDIOMIXER_SetReverbLowpassEnable(1); // Insert the Reverb Lowpass Filter

    AUDIOMIXER_SetReverbDelay1(floatf(0.999999999));   // maximum delay  0.00000000000000 = minimum delay
    AUDIOMIXER_SetReverbFeedback1(floatf(0.2));        // i.e 20% feedback

    AUDIOMIXER_SetReverbDelay2(floatf(0.99999999));    // maximum delay  0.00000000000000 = minimum delay
    AUDIOMIXER_SetReverbFeedback2(floatf(0.2));

    AUDIOMIXER_SetReverbPan(floatf(.4));               // Slightly left of center!
    AUDIOMIXER_SetMasterGain(floatf(0.9999999999999)); // Nomen est Omen

#endif

    Init_BarGraphTable(BG_MAX_VALUE, BG_DB_STEPS);

}



