

#ifndef MixerControl_H
#define MixerControl_H 1

#include "CONFIG.H"

//---------------------------------------------------------------------------
//  MIXER CONTROL COMMANDS
//---------------------------------------------------------------------------

// These functions create a MixerControl struct (32 bits, 4 chars) to control all
// mixer fuctions

// Commands are sent from a PC-based Mixer application when faders are moved or buttons
// are pressed, to synchronise the Mixer state with the PC display.
//
// eg
// MixerControl thisMixCtrl = SetChannelGain(0,127);      // channel 0 gain to maximum
//
// Send thisMixCtrl to a known memory location in the TSK 3000 address space
// The mixer software checks the pending flag until it is 1, makes the adjustment,
// then clears the pending flag.
// so if you read the TSK3000 memory location back, if pending is 0 the change
// has been made, so you can send another command.

// The Mixer checks the thisMixCtrl Pending flag at 50 mSec intervals (could be faster)
// so this is currently the fastest rate that commands can be processed. If a command is received
// by the Mixer before the previous has been processed it is guaranteed to replace the previous command.

// COMMAND STRUCTURE

typedef struct tag_MixerControl {
    char  PendingFlag;         // 1 = Pending Change
    char  FunctionCode;        // define which parameter to change as per above defines
    char  Channel;             // 0 to 3  - only meaningful for a channel operation
    char  Value;               // 0 to 127
} MixerControl;

typedef MixerControl * LPMixerControl;

// FUNCTIONCODE DEFINITIONS

#define   REVPARBASE (0)
// Settings for Channels, the channel field in MixerControl must be set to 0..3 to select desired channel
#define   CHANNEL_GAIN            (REVPARBASE)            // Value = 0..127
#define   CHANNEL_PAN             (REVPARBASE+1)          // Value = 0..127   (0=left, 127=right)
#define   CHANNEL_REVERBSEND      (REVPARBASE+2)          // value = 0..127
#define   CHANNEL_SOLO            (REVPARBASE+3)          // value = 0 or 1   1 = solo active for this channel
#define   CHANNEL_MUTE            (REVPARBASE+4)          // value = 0 or 1   1 = channel is muted

// Settings for which the channel field in MixerControl is meaningless
#define   REVERB_PAN              (REVPARBASE+5)          // value = 0..127    (0=left, 127=right)
#define   REVERB_IN               (REVPARBASE+6)          // value = 0 or 1   1 = reverb enabled
#define   REVERB_LOWPASS          (REVPARBASE+7)          // value = 0 or 1   1 = reverb lowpass enabled
#define   REVERB_DELAY1           (REVPARBASE+8)          // value = 0..127   0 = minimum delay, 127 = maximum delay
#define   REVERB_FEEDBACK1        (REVPARBASE+9)          // value = 0..127   0 = no feedback, 127 = maximum feedback (100%)
#define   REVERB_DELAY2           (REVPARBASE+10)         // value = 0..127   0 = minimum delay, 127 = maximum delay
#define   REVERB_FEEDBACK2        (REVPARBASE+11)         // value = 0..127   0 = no feedback, 127 = maximum feedback (100%)
#define   MASTERGAIN              (REVPARBASE+12)         // value = 0..127


#ifdef DEBUG

MixerControl SetChannelGain(int channel,char gain); // gain 0..127

MixerControl SetChannelPan(int channel,char pan);  // pan 0..127

MixerControl SetChannelReverbSend(int channel,char gain);

MixerControl SetChannelSolo(int channel,char flag);

MixerControl SetChannelMute(int channel,char flag);

MixerControl SetReverbEnable(int channel,char flag);

MixerControl SetReverbLowpassEnable(char flag);

MixerControl SetReverbDelay1(char delay);

MixerControl SetReverbFeedback1(char feedback);

MixerControl SetReverbDelay2(char delay);

MixerControl SetReverbFeedback2(char feedback);

MixerControl SetReverbPan(char pan);

MixerControl SetMasterGain(char gain);

#endif

#endif


