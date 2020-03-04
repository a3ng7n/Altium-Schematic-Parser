#include "wave.h"
#include "strio.h"
#include "uart.h"
#include "wswap.h"
#include "hware.h"
#include "util.h"
#include "timer.h"

DataChunkType DC;
FormatChunkType WaveFormat;

// inline macros for arming receive timeout timer
#define ARM_TIMER {ET0 = 0; Timer[TIMER_UART_TIMEOUT] = 0xFF; ET0 = 1;}
#define CHECK_TIMER {ET0 = 0; if(Timer[TIMER_UART_TIMEOUT] == 0) {ET0 = 1; return(0xFF);} else ET0 = 1;}


//-------------------------------
// reads wave file header
// returns 0    if no errors
//         1    if unsupported file type
//         0xFF if timeout
//-------------------------------
unsigned char WaveReadHeader(void)
{
  unsigned char i;
  static volatile unsigned char c;
  unsigned char retval = 0;
  unsigned char *p;
  WaveHeaderType WH;
  ARM_TIMER;            // set timeout timer
  p = (unsigned char *) &WH;
  for(i=0;i<sizeof(WH);i++)
  {
    while (RxBufEmpty()) // wait for next character
    {
      CHECK_TIMER;
    };
    c = RxBufGetChar();
//    OutStr(" %02X",c);
    *p++ = c;
  }        // here we check for validity
  if(romstrcmp("RIFF" ,WH.RiffHeader.id,4))
    retval |= 1;
  if(romstrcmp("WAVE",WH.WaveId,4))
    retval |= 2;
  return retval;
}

//-------------------------------
// reads wave file format chunk
// returns 0    if no errors
//         1    if unsupported file type
//         0xFF if timeout
//-------------------------------
unsigned char WaveReadFormat(void)
{
  unsigned char retval =0;
  unsigned char i,c;
  unsigned char *p = (unsigned char *)&WaveFormat;
  ARM_TIMER;            // set timeout timer
  for(i=0;i<sizeof(WaveFormat); i++)
  {
    while (RxBufEmpty())         // todo: handle timeout
    {
      CHECK_TIMER;
    };
    c = RxBufGetChar();
//    OutStr(" %02X",c);
    *p++ = c;
  }        // here we check for validity
  Wswap(&WaveFormat.wBitsPerSample,2);
  Wswap(&WaveFormat.wBlockAlign,2);
  Wswap(&WaveFormat.wChannels,2);
  Wswap(&WaveFormat.wFormatTag,2);
  Wswap(&WaveFormat.dwSamplesPerSec,4);
  SetSampleRateDivider(WaveFormat.dwSamplesPerSec);  // set up divider
  Wswap(&WaveFormat.chunkSize,4);
  if(romstrcmp("fmt " ,WaveFormat.chunkID,4)) retval = 1;  // check chunk id (should be "fmt ")
  c = (unsigned char) (WaveFormat.chunkSize - 16);
  for(i=0; i< c; i++)  // ignore extra information the chunk may contain
  {
    while (RxBufEmpty())         // todo: handle timeout
    {
       CHECK_TIMER;
    };
    c = RxBufGetChar();
  }
  return retval;
}

#define TOP_ADDRESS 0x0007FFFFl

// inline macro
// increment address counter, if memory limit exceeded, return with error code
#define INC_ADDRESS {address++; if (address ==0)  {P0 = ++bank; if(bank >(unsigned char)(TOP_ADDRESS >>16)) {retval = 0x01; goto ABORT;}}}

//--------------------------------------
// reads the wave data chunk
// returns 0 : success
//         1 : memory limit exceeded
//      0xFF : timeout
//--------------------------------------
unsigned char WaveReadData(void)
{
   unsigned int FreezeAddress=0;
   unsigned char FreezeBank=0;
   unsigned volatile char c;
   unsigned char retval = 0;
   unsigned char ByteNumber= 0;
//   unsigned char Channel;
   unsigned long TempL;
   unsigned int i,oldi = 0;
   unsigned int address = 0;
   unsigned char bank = 0;
   unsigned char *pc;
   pc = (unsigned char *) &DC;
   for(i=0; i<sizeof(DC); i++)
   {
      ARM_TIMER;            // set timeout timer
      while (RxBufEmpty())
      {
        CHECK_TIMER;
      };
      c = RxBufGetChar();
      *pc++ = c;
//      OutStr("%02X\r\n",c);  // for debug purposes only
   }
   Wswap(&DC.chunkSize,sizeof(DC.chunkSize));         // must swap bytes on all ints and longs
   TempL = DC.chunkSize;
   while(TempL--)
   {
      ARM_TIMER;            // set timeout timer
      while (RxBufEmpty())
      {
        ET0 = 0;
          if(Timer[TIMER_UART_TIMEOUT] == 0)
          {
            ET0 = 1;
            retval= 0xFF;
            goto ABORT;
          }
        ET0 = 1;
      };
      c = RxBufGetChar();
      P0 = bank;   // set record mode and bank
      switch(ByteNumber)
      {
         case 0:
           FreezeAddress = address;
           FreezeBank = bank;
           if(WaveFormat.wChannels == 1)  // mono?
           {
             if(WaveFormat.wBitsPerSample == 8)  // 8 bit samples mono?
             {
               CHANNEL_SELECT = CHANNEL_LEFT;
               *(__xdata unsigned char*) address = 0;  // upconvert to 16 bits
               CHANNEL_SELECT = CHANNEL_RIGHT;
               *(__xdata unsigned char*) address = 0;  // upconvert to 16 bits
               INC_ADDRESS;   // increment address counter
               CHANNEL_SELECT = CHANNEL_LEFT;
               *(__xdata unsigned char*) address = c;
               CHANNEL_SELECT = CHANNEL_RIGHT;
               *(__xdata unsigned char*) address = c;
               INC_ADDRESS;
               ByteNumber = 0;
             }
             else                             // 16 bit samples mono?
             {
               CHANNEL_SELECT = CHANNEL_LEFT;
               *(__xdata unsigned char*) address = c;
               CHANNEL_SELECT = CHANNEL_RIGHT;
               *(__xdata unsigned char*) address = c;
               INC_ADDRESS;
               ByteNumber = 1;
             }
           }
           else  // stereo?
           {
             if(WaveFormat.wBitsPerSample == 8)  // 8 bit samples stereo?
             {
               CHANNEL_SELECT = CHANNEL_LEFT;
               *(__xdata unsigned char*) address = 0;  // upconvert to 16 bits
               INC_ADDRESS;
               *(__xdata unsigned char*) address = c;
               ByteNumber = 1;
            }
             else                             // 16 bit samples stereo?
             {
               CHANNEL_SELECT = CHANNEL_LEFT;  //1st byte is left channel lsNibble
               *(__xdata unsigned char*) address = c;
               INC_ADDRESS;
               ByteNumber = 1;
             }
           }
         break;
         case 1:
           if(WaveFormat.wChannels == 1)  // mono?  --> can only be 16 bit samples
           {                              // 16 bit samples mono?
              c^=0x80;        // convert to unsigned
              CHANNEL_SELECT = CHANNEL_LEFT;
              *(__xdata unsigned char*) address = c;
              CHANNEL_SELECT = CHANNEL_RIGHT;
              *(__xdata unsigned char*) address = c;
              INC_ADDRESS;
              ByteNumber = 0;
           }
           else  // stereo?
           {
             if(WaveFormat.wBitsPerSample == 8)  // 8 bit samples stereo?
             {
               address = FreezeAddress;
               bank = FreezeBank;
               CHANNEL_SELECT = CHANNEL_RIGHT;
               *(__xdata unsigned char*) address = 0;  // upconvert to 16 bits
               INC_ADDRESS;
               *(__xdata unsigned char*) address = c;
               INC_ADDRESS;
               ByteNumber = 0;
             }
             else                             // 16 bit samples stereo?
             {
               c ^=0x80;   // convert to unsigned
               CHANNEL_SELECT = CHANNEL_LEFT;  //2nd byte is left channel msNibble
               *(__xdata unsigned char*) address = c;
               ByteNumber = 2;
             }
           }
         break;
         case 2:   // only ever the case for 16 bit stereo
           address = FreezeAddress;
           bank = FreezeBank;
           CHANNEL_SELECT = CHANNEL_RIGHT;  //3rd byte is right channel lsNibble
           *(__xdata unsigned char*) address = c;
           INC_ADDRESS;
           ByteNumber = 3;

         break;
         case 3:
           c ^=0x80;   // convert to unsigned
           CHANNEL_SELECT = CHANNEL_RIGHT;  //4th byte is right channel msNibble
           *(__xdata unsigned char*) address = c;
           INC_ADDRESS;
           ByteNumber = 0;
         break;
         default:
           ByteNumber = 0;
      };
      i=(unsigned int) ((TempL) >> 12);   // update display
      if(oldi != i)
      {
         __Out_Char('.');
         oldi = i;
      }
   };
ABORT:
   if(retval ==0x01)   // memory limit exceeded?
   {
     address = (unsigned int)(TOP_ADDRESS & 0xFFFFl);
     bank = (unsigned char)(TOP_ADDRESS >>16);
   }
   Write32BitReg(SFR32_END_ADDRESS, (((unsigned long)address) | ((unsigned long)bank) <<16) >> 1);
   P0 = 0x80; // set play mode
   return retval;
}

