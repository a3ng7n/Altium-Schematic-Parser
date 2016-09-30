//------------------------------------------------------------------------------
//      T  S  K  3  0  0  0        M  O  D         P  L  A  Y  E  R
//------------------------------------------------------------------------------
#include "hware.h"
#include "hardware.h"
#include "strio.h"
#include "mod.h"
#include "sio.h"
#include "AudioMixer.h"
#include "ctype.h"


#define VERSION 900

//extern volatile unsigned int TimeOutTimer;

//-----------------------------------------------------------
// all STRIO character output happens through this function
//-----------------------------------------------------------
void __Out_Char(unsigned char c)
{
    TXSerial(c);
}


//--------------------------------------------------------------------------------------
void main (void)
{
    unsigned char c;
    unsigned char Volume =0x40;
    unsigned char Sample = 0;

    // point to 1st free 256byte bank after top of stack
    MFP = (ModFileType *) (( (unsigned int)(&_lc_ub_stack) | 0xFF) + 1);

    ModHardware_Init();

    SIO_Init(BAUDRATE);

    AUDIOMIXER_Init();

    StartAudio(SAMPLE_RATE);

    OutStr("\r\n/------------------------------------/\r\nTSK3000 Mod File Player V%d\r\n",VERSION);
    OutStr("Compiled: "__DATE__" "__TIME__"\r\n",0);
    OutStr("Free Memory : %d bytes\r\n", FreeRam());

    OutStr("\r\nSend Mod File Now %d Baud, 8 Data Bits, No parity, 1 Stop Bit, Binary Transfer ... ",BAUDRATE);
    ReadFile(MFP);
    OutStr("\n\n\rPress 'p' to play mod file, '0'..'9' to play samples, '+' or '-' to change sample volume\r\n",0);
    for(;;)
    {
      while(!RxBufferEmpty()) c=RXSerial();
      while(RxBufferEmpty());
      c=RXSerial();
      c= tolower(c);
      if(c=='p')
      {
        PlayMod(MFP);
        ModHardware_Init();
        c=0xFF;
      }
      if(c>='0' && c<='9') c -='0';
      if(c>='a' && c<='z') c -= ('a'-10);
      if((c=='+') || (c=='-'))
      {
         if (c=='+')
         {
           Volume++;
           if (Volume > 0x40) Volume = 0x40;
         }
         else
         {
           if (Volume)
             Volume--;
         }
         OutStr("\r\nNew Sample Play Volume is %d",Volume);

      }
      else
      {
         Sample = c;
      }
      if(c != 0xFF)
      {
        OutStr("\r\nPlaying Sample %3d ",Sample);
        OutStr(" Volume %d",Volume);
        PlaySample2(Sample, 0, 0x016E,SampleInfo,Volume);
        PlaySample2(Sample, 1, 0x016E,SampleInfo,Volume);
        PlaySample2(Sample, 2, 0x016E,SampleInfo,Volume);
        PlaySample2(Sample, 3, 0x016E,SampleInfo,Volume);
        StartSample(0x0000000F);
      }
    }
}

























































































































