
#include "MixerControl.h"

#ifdef DEBUG

// These functions are used in the DXP Mixer Script to generate Mixer Control Commands

MixerControl SetChannelGain(int channel,char gain)
{
    MixerControl M = {
                      1,                // Set Pending Flag
                      CHANNEL_GAIN,     // Set Opcode
                      (char)channel,    // Set Channel
                      gain              // set to this value
                     };
    return M;
}

MixerControl SetChannelPan(int channel,char pan)
{
    MixerControl M = {1,CHANNEL_PAN,(char)channel,pan};
    return M;
}

MixerControl SetChannelReverbSend(int channel,char gain)
{
    MixerControl M = {1,CHANNEL_REVERBSEND,(char)channel,gain};
    return M;
}

MixerControl SetChannelSolo(int channel,char flag)
{
    MixerControl M = {1,CHANNEL_SOLO,(char)channel,flag};
    return M;
}

MixerControl SetChannelMute(int channel,char flag)
{
    MixerControl M = {1,CHANNEL_MUTE,(char)channel,flag};
    return M;
}

MixerControl SetReverbEnable(int channel,char flag)
{
    MixerControl M = {1,REVERB_IN,(char)channel,flag};
    return M;
}

MixerControl SetReverbLowpassEnable(char flag)
{
    MixerControl M = {1,REVERB_LOWPASS,0,flag};
    return M;
}

MixerControl SetReverbDelay1(char delay)
{
    MixerControl M = {1,REVERB_DELAY1,0,delay};
    return M;
}

MixerControl SetReverbFeedback1(char feedback)
{
    MixerControl M = {1,REVERB_FEEDBACK1,0,feedback};
    return M;
}
MixerControl SetReverbDelay2(char delay)
{
    MixerControl M = {1,REVERB_DELAY2,0,delay};
    return M;
}

MixerControl SetReverbFeedback2(char feedback)
{
    MixerControl M = {1,REVERB_FEEDBACK2,0,feedback};
    return M;
}

MixerControl SetReverbPan(char pan)
{
    MixerControl M = {1,REVERB_PAN,0,pan};
    return M;
}

MixerControl SetMasterGain(char gain)
{
    MixerControl M = {1,MASTERGAIN,0,gain};
    return M;
}

#endif
