#ifndef __MOD_H__
#define __MOD_H__

#include "datatype.h"

#define PAL_MAGIC_NUMBER 7093789l    // amiga clock frequency for PAL
#define MOD_NO_INSTRUMENTS 31  // number of samples in file

typedef struct
{
    UINT8  SampleNumber;
    UINT16 SamplePeriod;
    UINT16 Effect;
} PCNote;   // note converted to PC readable format

typedef struct
{
    UINT32 PatternBreak;
    UINT32 PositionJump;
} PendingEffectsType;

typedef struct
{
    UINT32 Note[4] ;
} LineType4;    // line for 4 channels

typedef LineType4 PatternType[64];   // each pattern consists of 64 lines

typedef struct
{
    UINT8  SampleName[22];  // null-terminated unless 22 characters long
    UINT16 Length;        // sample length in words
    UINT8  FineTune;
    UINT8  Volume;
    UINT16 RepeatOffset;  // sample repeat offset in words
    UINT16 RepeatLength;  // sample repeat length in words
} ModSampleInfoType;

typedef struct
{
  UINT32 StartAddress;
  UINT32 EndAddress;
  UINT32 RepeatStartAddress;
  UINT32 RepeatEndAddress;
  SINT32 FineTune;
  UINT32 Volume;
} PCSampleInfoType;  // sample info in 'hardware friendly' format

typedef PCSampleInfoType SampleInfoRecordType[MOD_NO_INSTRUMENTS];

typedef struct
{
  UINT8 SongTitle[20];
  ModSampleInfoType SampleInfo[MOD_NO_INSTRUMENTS];
  UINT8 NoOfPatterns;     // number of patterns in song
  UINT8 EndPos; // song end position
  UINT8 PatternTable[128];  // pattern number sequence
  UINT8 FileFormatTag[4];   // "M.K.", "FLT4", "M!K!", "4CHN" : 4 channels, 31 instruments
  PatternType Pattern;              // pattern info
}   ModFileType;

extern ModFileType *MFP;
extern SampleInfoRecordType SampleInfo;

//------------------------------------------------
// reads mod file to memory starting at Dest
//------------------------------------------------
unsigned long ReadFile(void * Dest);

//-------------------------------------------------------------------------
// Plays pattern 'pat' consisting of 64 lines
//-------------------------------------------------------------------------
PendingEffectsType * PlayPattern(ModFileType * MFP, unsigned char Pat);

//------------------------------------------------
// reads mod file to memory starting at Dest
//------------------------------------------------
unsigned long ReadFile(void * Dest);

//---------------------------------------------------------------------
// Sets up all hardware registers to play one sample of one channel
// but does not start the sample
//---------------------------------------------------------------------
void PlaySample2(UINT8 SampleNumber, UINT8 Channel, UINT32 SampleRate,
                 SampleInfoRecordType SampleInfo, UINT32 Volume );

//------------------------------------------------------------
// restarts samples for each voice channel that has a bit set
// in 'ChannelMask'
// bit 0: Channel 0
// bit 1: Channel 1 etc
//------------------------------------------------------------
void StartSample(unsigned int ChannelMask);

//-------------------------------------------------------------------------
// plays complete mod file from start to end
//-------------------------------------------------------------------------
void PlayMod(ModFileType * MFP);

//-----------------------------------------------------------------------------
// initialises all Sample Hardware for minimum sample rate and quiet output
//-----------------------------------------------------------------------------
void ModHardware_Init(void);

//---------------------------------------------------------
// returns amount of ram in bytes available for MOD file
//---------------------------------------------------------
UINT32 FreeRam(void);


#endif
