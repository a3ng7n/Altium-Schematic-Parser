#include "hware.h"
#include "hardware.h"
#include "MOD.H"
#include "strio.h"
#include "sio.h"

extern volatile unsigned int TickTimer;
extern volatile unsigned int TimeOutTimer;

// #define VERIFY_HEADER

#define DISPLAY_PROPELLOR   // display busy propellor during file download

#define IGNORE_END_REPEAT     // define this to stop repeats due to end repeat byte
// #define IGNORE_POSITION_JUMP  // define this to stop repeats due to position jumps

ModFileType *MFP;// = (ModFileType *) 0x1020000;   // Points memory after all variables WARNING: Not initialised corecctly in 8.2.8.2533
SampleInfoRecordType SampleInfo;                // sample information in 'hardware friendly' form

#pragma section slowrom

//---------------------------------------------------------
// returns amount of ram in bytes available for MOD file
//---------------------------------------------------------
UINT32 FreeRam(void)
{
    UINT32 retval;
    retval = RAM_SIZE - 1;
    retval -= (( (UINT32)(&_lc_ub_stack) | 0xFF) + 1) - Base_RAM ;
    return retval;
}


//----------------------------------------------------------------------
// calculates SampleRate based on the period in the Pattern Data
//----------------------------------------------------------------------
inline unsigned long CalcSampleRate(unsigned int period)
{
    return PAL_MAGIC_NUMBER / (period);
}

//-------------------------------------------------------------------
// sets up 32bit sample rate divider based on Mod file sample rate
//-------------------------------------------------------------------
UINT32 CalcSampleRateDivider(UINT32 long SamplesPerSecond)
{
    UINT32 Divider;
    Divider = (unsigned long) 2* FCLK / SamplesPerSecond;
    return Divider;
}



//----------------------------------------------------------------------
// reads the patterntable sequentially and returns the largest pattern
// in the table number found
//----------------------------------------------------------------------
unsigned int CalcNoOfPatterns(ModFileType * MFP)
{
    unsigned int NoOfPatterns = 0 , i;
    PatternType * PTP = &(MFP->Pattern);
    for(i=0;i<sizeof(MFP->PatternTable); i++)
    {
       unsigned char c;
       c = MFP->PatternTable[i];
       if(c > NoOfPatterns) NoOfPatterns = c;  // find largest number
    }
    return ++NoOfPatterns;
}

//------------------------------------------------------------------------
// converts the 'scrambled' 4 byte 'Note' to a 'Compiler readable' struct
// 'PCNote'
//------------------------------------------------------------------------
PCNote LineType4ToPCNote(unsigned int Note)
{
    PCNote retval;
    retval.SampleNumber = ((Note >> 12) & 0x0F) | ((Note >> 24) & 0xF0);
    retval.SamplePeriod = (Note >> 16) & 0x0FFF;
    retval.Effect       = Note & 0x0FFF;
    return retval;
}

//------------------------------------------------------------------------
//  reads sample information from MFP and converts it to a form that
//  requires less runtime overhead in SR
//------------------------------------------------------------------------
void FillSampleInfoRecord(SampleInfoRecordType SR, ModFileType * MFP)
{
    UINT32 i;
    UINT32 AD;  // current address;
    UINT32 NumberOfPatterns;
    PatternType * PTP;
    NumberOfPatterns = CalcNoOfPatterns(MFP);
    PTP = &(MFP->Pattern); // pointer to pattern #0;
    AD = (UINT32) &(PTP[NumberOfPatterns]);  // points to 1rst byte after pattern data,
                                           // which is the 1st sample
    for (i=0;i<MOD_NO_INSTRUMENTS;i++)
    {
       UINT32 temp;
       SR[i].StartAddress       = AD;
       SR[i].RepeatStartAddress = AD + ((MFP->SampleInfo[i].RepeatOffset) << 1 );
       temp = (MFP->SampleInfo[i].RepeatLength);
       if( temp >1 )
       {
          SR[i].RepeatEndAddress   = SR[i]. RepeatStartAddress + (temp << 1 );
       }
       else
       {
          SR[i].RepeatStartAddress = 0;  // do not repeat marker
          SR[i].RepeatEndAddress = 0;  // do not repeat marker
       }
       temp = (MFP->SampleInfo[i].Length) << 1;
       SR[i].EndAddress         = AD + temp;   // sample length
       SR[i].FineTune           = MFP->SampleInfo[i].FineTune;
       SR[i].Volume             = MFP->SampleInfo[i].Volume;
       AD = SR[i].EndAddress;
    }
}

void ModPrintTitle(ModFileType * MFP)
{
       unsigned char i,c;
       for(i=0;i<sizeof(MFP->SongTitle);i++)  // display song title
       {
          c = MFP->SongTitle[i];
          if(c==0) break;
             __Out_Char(c);
       }
}


#ifdef VERIFY_HEADER
//---------------------------------------------------------------------------
// returns 0 if no error
// Todo: perform basic checks and modify return code accordingly
//---------------------------------------------------------------------------
unsigned char VerifyHeader(ModFileType * MFP)
{

  PatternType * PTP;
  unsigned char retval = 0;
  unsigned char c,i;
  unsigned int line;
  unsigned int NoOfPatterns = 0;  // contains the highest value found in the pattern table
  OutStr("\r\n",0);
  ModPrintTitle(MFP);

  for(i=0;i<31;i++)   // display Mod sample type info
  {
     unsigned char EndOfName=0;
     unsigned char x;
     OutStr("\r\n%2d ",i);
     for(x=0;x<sizeof(MFP->SampleInfo[i].SampleName);x++)
     {
        c = MFP->SampleInfo[i].SampleName[x];
        if(0 == c) EndOfName = 1;
        if (0 != EndOfName) c = ' ';
        __Out_Char(c);
     }
     OutStr(": L: %5d ",MFP->SampleInfo[i].Length);
     OutStr("Ft: %3d ", MFP->SampleInfo[i].FineTune); //MFP->SampleInfo.FineTune);
     OutStr("V: %3d ",MFP->SampleInfo[i].Volume);
     OutStr("R.O: %5d ",MFP->SampleInfo[i].RepeatOffset);
     OutStr("R.L: %5d ",MFP->SampleInfo[i].RepeatLength);
  }

    NoOfPatterns=CalcNoOfPatterns(MFP);
    OutStr("\r\nNumber of different Patterns:%3d", NoOfPatterns);
    OutStr("\n\rNumber of played Patterns   :%3d",MFP->NoOfPatterns);
    OutStr("\n\rEnd Position: %d",MFP->EndPos);
    for(i=0;i<sizeof(MFP->PatternTable); i++) // display pattern table
    {
       if (0 == (i & 0x0F))
          OutStr("\r\n",0);
       c = MFP->PatternTable[i];
       OutStr("%3d",c);
    }
    OutStr("\r\n File Format Tag: ",0);
    for(i=0;i<sizeof(MFP->FileFormatTag);i++)
    {
       c = MFP->FileFormatTag[i];
       __Out_Char(c);
    }
    PTP = &(MFP->Pattern); // pointer to pattern #0;
    OutStr("Patterns Start at %08X",(unsigned int) PTP);
    for(line=0;line<64;line++) // display pattern information
    {
       PCNote PCN;
       unsigned char Channel;
       OutStr("\r\n Line%03d: ",line);
       for(Channel = 0; Channel <4; Channel++)
       {
          unsigned int Note;
          OutStr("Ch%d:",Channel);
          Note = MFP->Pattern[line].Note[Channel];
          PCN = LineType4ToPCNote(Note);
          OutStr("SN:%02X ",PCN.SampleNumber);
          OutStr("SP:%04X ",PCN.SamplePeriod);
          OutStr("EF:%04X ",PCN.Effect);
       }
    }
    OutStr("\r\nPattern Memory:%d",NoOfPatterns * sizeof(PatternType));
    OutStr("\r\nSamples start at  :%X",(unsigned long)&(MFP->Pattern) +(NoOfPatterns * sizeof(PatternType)));
    return retval;
}
#endif


//------------------------------------------------------------
// restarts samples for each voice channel that has a bit set
// in 'ChannelMask'
// bit 0: Channel 0
// bit 1: Channel 1 etc
//------------------------------------------------------------
void StartSample(unsigned int ChannelMask)
{
  PAR_REG(ADR_START) = ChannelMask;
  __asm("nop");
  __asm("nop");
  __asm("nop");
  __asm("nop");
  PAR_REG(ADR_START) = 0;
}


//---------------------------------------------------------------------
// Sets up all hardware registers to play one sample of one channel
// but does not start the sample
//---------------------------------------------------------------------
void PlaySample2(UINT8 SampleNumber, UINT8 Channel, UINT32 SampleRate, SampleInfoRecordType SampleInfo, UINT32 Volume )
{
   UINT32 DividerRatio;
   UINT32 ChannelOffset;
   ChannelOffset = 8 * Channel;
   DividerRatio = CalcSampleRateDivider(CalcSampleRate(SampleRate + SampleInfo[SampleNumber].FineTune));
   DividerRatio /=2;   // compensate for extra /2 hardware
   PAR_REG(OFS_START_ADR + ChannelOffset)      = SampleInfo[SampleNumber].StartAddress;
   PAR_REG(OFS_END_ADR + ChannelOffset)        = SampleInfo[SampleNumber].EndAddress;
   PAR_REG(OFS_START_REPEAT + ChannelOffset)   = SampleInfo[SampleNumber].RepeatStartAddress;
   PAR_REG(OFS_END_REPEAT + ChannelOffset)     = SampleInfo[SampleNumber].RepeatEndAddress;
   if(Volume)                                  // limit volume to 6 bits
     if(--Volume > 0x3F) Volume = 0x3F;
   PAR_REG(OFS_VOLUME + ChannelOffset)         = Volume;
   if(DividerRatio < 10) DividerRatio = 0xFFFF;   // limit sample rate
   PAR_REG(OFS_DATA_RATE + ChannelOffset)      = DividerRatio;
 }

//-------------------------------------------------------------------------
// Plays pattern 'pat' consisting of 64 lines
//-------------------------------------------------------------------------
PendingEffectsType * PlayPattern(ModFileType * MFP, unsigned char Pat)
{
    static PendingEffectsType PendingEffects;
    unsigned int TargetTick;
    unsigned char Speed = 6;
    static unsigned char NewSpeed = 6;
    unsigned char Channel = 1;
    unsigned char Line;
    unsigned char StartMask;  // Mask for Channels that need to be started
    PatternType * PTP;
    PTP = &(MFP->Pattern); // pointer to 1st current pattern;
    OutStr(" Pattern: %2d ",Pat);
    for(Line = 0; Line <64; Line++)  // play Line by line
    {
      PCNote PCN;
      if(PendingEffects.PatternBreak)
        Line = PendingEffects.PatternBreak & 0x7F;
      PendingEffects.PatternBreak = 0;
      Speed = NewSpeed;
      TargetTick = TickTimer+Speed;
      StartMask = 0;
      for(Channel = 0; Channel < 4; Channel++)
      {
        unsigned int Note;
        unsigned int Volume = 0;
        Note = PTP[Pat][Line].Note[Channel];
        PCN = LineType4ToPCNote(Note);  // convert to machine readable format
        switch(PCN.Effect >>8)
        {
         case 0x00: // no effect : ignore
            break;
         case 0x0B:   // Position Jump
            PendingEffects.PositionJump = PCN.Effect & 0x7F;
            OutStr(" PJ:%d ",PendingEffects.PositionJump);
            PendingEffects.PositionJump |= 0x80;
          break;
          case 0x0C:   // Set Volume
            OutStr(" SV:%d", PCN.Effect & 0x7F);
            Volume = PCN.Effect & 0x7F;
            Volume |= 0x80;
          break;
          case 0x0D:   // Pattern Break  // only 00 is implemented !!!
            PendingEffects.PatternBreak = 10 * ((PCN.Effect & 0x00F0) >> 4) + (PCN.Effect & 0x000F);
            OutStr(" PB:%d ",PendingEffects.PatternBreak);
            PendingEffects.PatternBreak |= 0x80;
          break;
          case 0x0F:   // Set Speed from here on in
            NewSpeed = PCN.Effect & 0x00FF;
            if(NewSpeed >31)          // ignore bpm instructions
            {
               NewSpeed = Speed;
            }
            Speed = NewSpeed;
            OutStr(" SP:%d", NewSpeed);
          break;
          default:
//            OutStr(" EF:%04X",PCN.Effect);  // unsupported effects
          break;
        }
        if(Volume &0x80)   // effect set volume
        {
          Volume &= 0x7F;
          if(Volume > 0x40) Volume = 0x40;
        }
        else
        {  // default sample value volume;
             Volume = SampleInfo[PCN.SampleNumber-1].Volume;
        }
        if(PCN.SampleNumber >0)
        {
             PlaySample2(PCN.SampleNumber-1, Channel, PCN.SamplePeriod, SampleInfo, Volume);
             StartMask |= (1 << Channel);
        }
      } // for Channel
      while(TickTimer!= TargetTick);
      StartSample(StartMask);
      StartMask = 0;
      if(PendingEffects.PatternBreak) return &PendingEffects;
      if(PendingEffects.PositionJump) return &PendingEffects;
    }
    return &PendingEffects;
}

#pragma endsection

//------------------------------------------------
// reads mod file to memory starting at MFP
//------------------------------------------------
unsigned long ReadFile(void * Dest)
{
    unsigned char PropChars[] = "|""\x8""/""\x8""-""\x8""\\""\x8";
    unsigned char *PCurChar = PropChars;
    unsigned char *D = (unsigned char *) Dest;
    unsigned int bytecounter;
#ifdef WAIT_FOR_FILE_DOWNLOAD
    while(RxBufferEmpty());
#endif
    TimeOutTimer = (unsigned int) (RX_TIMEOUT * INT1FREQ);
    for(bytecounter = 0; (unsigned int)D < (RAM_SIZE-1) + Base_RAM; bytecounter++)
    {
       TimeOutTimer = (unsigned int) (RX_TIMEOUT * INT1FREQ);
       while(RxBufferEmpty())
       {
          if(0==TimeOutTimer)
             goto READFILE_ABORT;   // receive timeout ?
       }
       *D++ = RXSerial();
       TimeOutTimer = (unsigned int) (RX_TIMEOUT * INT1FREQ);
       if(0==(bytecounter & 0x00FF))
       {
          unsigned char c;
          c = *PCurChar++;
          if(0==c)
          {
             PCurChar = PropChars;
             c = *PCurChar++;
          }
#ifdef DISPLAY_PROPELLOR
          SER_REG(SBUF_OFS) = c;    // write directly to buffer unless interrupt driven
#endif
       }
    }

READFILE_ABORT:
#ifdef VERIFY_HEADER
    VerifyHeader(MFP);
#endif
    FillSampleInfoRecord(SampleInfo, MFP);
    return(bytecounter);
}

#pragma section slowrom

//-------------------------------------------------------------------------
// plays complete mod file from start to end
//-------------------------------------------------------------------------
void PlayMod(ModFileType * MFP)
{
    unsigned char PatternCount = 0;   //number of patterns
    unsigned char NoOfPatterns = CalcNoOfPatterns(MFP);
    OutStr("\r\nTitle :",0);
    ModPrintTitle(MFP);
    OutStr("\r\nNumber of Tracks: %3d",MFP->NoOfPatterns);
    PatternCount = 0;
#ifndef IGNORE_END_REPEAT
PLAYMOD_ENDREPEAT:
#endif
    for(; PatternCount < MFP->NoOfPatterns; PatternCount++) // play until last pattern is reached
    {
       PendingEffectsType * PEP;
       OutStr("\r\nTrack No.:%3d",PatternCount);
       PEP = PlayPattern(MFP, MFP->PatternTable[PatternCount]);
       if(PEP->PositionJump > 0)  // jump to track
       {
#ifndef IGNORE_POSITION_JUMP
          PatternCount = (PEP->PositionJump & 0x7F) - 1;
#endif
          PEP->PositionJump = 0;
       }
    }
#ifndef IGNORE_END_REPEAT
    if(MFP->EndPos < 127)   // repeat
    {
       PatternCount = MFP->EndPos;
       OutStr("\r\nEndRepeat Pattern %d",PatternCount);
       if(PatternCount < MFP->NoOfPatterns)
          goto PLAYMOD_ENDREPEAT;
    }
#endif
}

UINT8 InitSample = 0x80;

//-----------------------------------------------------------------------------
// initialises all Sample Hardware for minimum sample rate and quiet output
//-----------------------------------------------------------------------------
void ModHardware_Init(void)
{
  UINT32 Channel;
  for (Channel = 0; Channel < 4; Channel++)
  {
     UINT32 ChannelOffset;
     ChannelOffset = 8 * Channel;
     PAR_REG(OFS_START_ADR + ChannelOffset)      = (unsigned int) &InitSample;
     PAR_REG(OFS_END_ADR + ChannelOffset)        = (unsigned int) &InitSample;
     PAR_REG(OFS_START_REPEAT + ChannelOffset)   = 0;
     PAR_REG(OFS_END_REPEAT + ChannelOffset)     = 0;
     PAR_REG(OFS_VOLUME + ChannelOffset)         = 0;
     PAR_REG(OFS_DATA_RATE + ChannelOffset)      = 0xFFFF;
  }
  StartSample(0xF);
}


#pragma endsection

