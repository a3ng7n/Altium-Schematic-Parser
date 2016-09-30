
#include "hware.h"
#include "uart.h"
#include "strio.h"
#include "wswap.h"
#include "wave.h"
#include "timer.h"
#include "conio.h"
#include "seven_segment.h"

#define printCenter(text) _printCenter(text,sizeof(text))

void _printCenter(const char __rom *text, char length)
{
     char spaces;
     for (spaces = (32-length)/2; spaces>0; spaces--)
         putch(' ');
     cputs(text);
     putch('\n');
}

void displayTitle(void)
{
    static char __rom helpStr[] = "\n Press Button SW1 to activate\n"
                                   " Press Test/Reset to reset\n";
    settextcolor(YELLOW);
    setcursormode(0);
    clrscr();
    putch('\n');
    printCenter("-- Altium LiveDesign --");
    printCenter("-- Evaluation Board  --");
    printCenter("-- Countdown Example --");
    cputs(helpStr);
}

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
  char loaded=0;
  int address = 0;
  EA = 0;

  wave_stop();
  seven_clearall();
  displayTitle();

  UartInit();
  Timer0Init();
  EA = 1;

  for(;;)
  {
    loaded=0;

    /* Timer Alarm sound download */
    seven_puts(0,"-L0AD-");

    OutStr("\r\n\n---------------------------------------------\r\n",0);
    OutStr("\r\n Send WAV file in binary form ",0);
    OutStr("\r\n using 57600, 8N1, no flow protocol",0);
    OutStr("\r\n WAV file must be in Windows PCM format",0);
    OutStr("\r\n with no compression used",0);
    OutStr("\r\n for best results use 22050Bps, 16 bit stereo",0);
    OutStr("\r\n\n---------------------------------------------\r\n",0);
    while(!loaded)
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
        if(c==0)
        {
                OutStr("\r\nSUCCESS!!!",0);
                loaded=1;
        }
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

  /* Timer Countdown */
  char lastState;
  char state;
  long counter = 20;
  while (counter>0)
  {
         seven_putlong(counter);
         while (lastState == state)
               state = P2 & 0x20;
         lastState = state;
         if (state) //Rising edge
            counter--;
  };
  wave_play();
  }
}
