
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
    settextcolor(WHITE);
    setbordercolor(GREEN);
    settextbackground(GREEN);
    setcursormode(0);

    clrscr();
    putch('\n');
    putch('\n');
    printCenter("***********************");
    printCenter("**                   **");
    printCenter("** Altium LiveDesign **");
    printCenter("** Evaluation Board  **");
    printCenter("** Countdown Example **");
    printCenter("**                   **");
    printCenter("***********************");

}

void displayDownloadHelp(void)
{
    displayTitle();
    settextcolor(YELLOW);
    cputs("\n\n");
    printCenter("-----------------------------");
    cputs("\n");
    printCenter("Ready to download WAV file");
    printCenter("for countdown sound.");
    cputs("\n");
    printCenter("Download via serial port.");
    printCenter("Using 57600 BPS");
    printCenter("8 Bits, No parity, 1 Stop bit");
    printCenter("No Flow Control");
    cputs("\n");
    printCenter("-----------------------------");
    cputs("\n");
    printCenter("Press SW1 to bypass download.");
    seven_puts(0,"-L0AD-");
};


void displayTimerHelp(void)
{
    displayTitle();

    settextcolor(YELLOW);
    cputs("\n\n");
    printCenter("-----------------------------");
    cputs("\n");
    printCenter("Press SW1 to restart countdown.");
    printCenter("Press SW2 to stop and reload.  ");
    cputs("\n");
    printCenter("-----------------------------");
}

//--------------------------------------------------------
// All character I/O in STRIO is handled by this function
//--------------------------------------------------------
void __Out_Char(unsigned char c)
{
    UART_TxChar_Poll(c);
}

void serialLoader(void)
{
    unsigned int i;
    unsigned char c=0;

    /* Timer Alarm sound download */
    displayDownloadHelp();

    OutStr("\r\n\n**********************\r\n",0);
    OutStr("** Altium LiveDesign **\r\n",0);
    OutStr("** Evaluation Board  **\r\n",0);
    OutStr("** Countdown Example **\r\n",0);
    OutStr("***********************\r\n",0);
    OutStr("\r\n\n---------------------------------------------\r\n",0);
    OutStr("\r\n Send WAV file in binary form ",0);
    OutStr("\r\n using 57600, 8N1, no flow protocol",0);
    OutStr("\r\n WAV file must be in Windows PCM format",0);
    OutStr("\r\n with no compression used",0);
    OutStr("\r\n for best results use 22050Bps, 16 bit stereo",0);
    OutStr("\r\n\n---------------------------------------------\r\n",0);

    char loaded=0;
    while(!loaded)
    {
        OutStr("\r\nREADY TO RECEIVE FILE!!!\r\n",0);
        while(!RxBufEmpty()) RxBufGetChar();
        P0 = 0x80;
        P1_3 = 0;

        //Wait for an event
        while(RxBufEmpty() && !kbhit());

        if (kbhit())
        {
           if (getch()=='1')
           {
               OutStr("\r\nDOWNLOAD ABORTED BY USER!\r\n",0);
               return;
           }
        }
        else
        {
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
       };
    }
}

void timerRun(void)
{
   displayTimerHelp();
   /* Timer Countdown */
   char abort=0;

   while (!abort)
   {
       char lastState=0;
       char state=0;
       long startCount = 20;
       long counter = startCount;

       while (counter>=0 && !abort)
       {
            //Update display
            seven_putlong(counter);

            //Check for clock edge
            state = P2 & 0x20;
            if ((lastState != state) && state)
            {
               counter--;
            }
            lastState = state;

            //Check for key press
            if (kbhit())
            {
                switch (getch())
                {
                  case '1':   counter = startCount;   break;
                  case '2':   abort = 1;              break;
                }
            }
       };

       //Play waveform until key pressed
      if (!abort)
      {
         wave_play();
         if (getch()=='2')
            abort=1;
         wave_stop();
      }
   }
}

void main(void)
{
  //unsigned char Bank = 0;

  EA = 0;

  wave_stop();
  seven_clearall();

  UartInit();
  Timer0Init();
  EA = 1;

  for(;;)
  {
    serialLoader();
    timerRun();
  }
}
