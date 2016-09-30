
#include "hware.h"
#include "uart.h"
#include "strio.h"
#include "wswap.h"
#include "wave.h"
#include "timer.h"

//--------------------------------------------------------
// All character I/O in STRIO is handled by this function
//--------------------------------------------------------
void __Out_Char(unsigned char c)
{
    UART_TxChar_Poll(c);
}


void main(void)
{
  unsigned char c=0;
  unsigned int i;
  unsigned char Bank = 0;
  int address = 0;
  EA = 0;
  UartInit();
  Timer0Init();
  EA = 1;
  OutStr("\r\n\n---------------------------------------------\r\n",0);
  OutStr("\r\n Send WAV file in binary form ",0);
  OutStr("\r\n using 57600, 8N1, no flow protocol",0);
  OutStr("\r\n WAV file must be in Windows PCM format",0);
  OutStr("\r\n with no compression used",0);
  OutStr("\r\n for best results use 22050Bps, 16 bit stereo",0);
  OutStr("\r\n\n---------------------------------------------\r\n",0);
   for(;;)
  {
    OutStr("\r\nREADY TO RECEIVE FILE!!!\r\n",0);
    while(!RxBufEmpty()) RxBufGetChar();
    P0 = 0x80;
    P1_3 = 0;
    while(RxBufEmpty());
    P1_3 = 1;
    if((i=WaveReadHeader()))
      OutStr("Hdr ERROR:%d\r\n",i);
    else
    {
      OutStr("\r\nReceiving...\r\n",0);
      if(WaveReadFormat())
      {
        OutStr("Format ERROR",0);
      }
      else
      {
        c=WaveReadData();
        if(c==0) OutStr("\r\nSUCCESS!!!",0);
        if(c==1) OutStr("\r\nOUT OF MEMORY!!!",0);
        OutStr("\r\n Tag               :%d",WaveFormat.wFormatTag);
        OutStr("\r\n Bits Per Sample   :%d",WaveFormat.wBitsPerSample);
        OutStr("\r\n Number Of Channels:%d ",WaveFormat.wChannels);
        OutStr("\r\n Block Align       :%d ",WaveFormat.wBlockAlign);
        OutStr("\r\n Sample Rate       :%d ",(unsigned int) WaveFormat.dwSamplesPerSec);
        OutStr("\r\n Chunk Size(Hex)   :%04X",(unsigned int) (DC.chunkSize>>16));
        OutStr("%04X \r\n",(unsigned int) (DC.chunkSize & 0xFFFF));
      }
    }
    OutStr("\r\n",0);
    i=0;
  }
}
