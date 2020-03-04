//--------------------------------
// Seven Segment Countdown timer
//--------------------------------

#include "keyboard.h"
#include "seven_segment.h"
#include "rttl.h"

char __rom scroll_text[] = "LIVE  DESIGN     ";
char __rom tune[] ="final:d=16,o=6,b=100,l=2:e,d,4e,2a5,8p,f,e,f,p,e,p,2d,8p,f,e,4f,2a5,8p,d,c,8d,8c,8b5,8d,4c,8p,b5,c,4d,8p,c,d,8e,8d,8c,8b5,4a5,4f,2e,4p,e,f,e,d,2e,p1";
//-----------------------------------------
// Read start count value from JTAG I/O
//-----------------------------------------

unsigned int getStartCount(void)
{
   const unsigned int DEFAULT_COUNT = 20;
   unsigned int count;

   //Read count high and low value from ports
   count = (P1<<8)+P0;

   if (count==0)
      count = DEFAULT_COUNT;
   return count;
}

//--------------------------------------------
// Update count onto display and JTAG I/O
//--------------------------------------------

void updateDisplay(unsigned int count)
{
    //Display count on seven segment display
     seven_putlong((unsigned int)count);

     //Write count low and high bytes to port
     P0 = count & 0xff;
     P1 = (count >> 8);
}

//------------------------------------
// Countdown timer FSM
//------------------------------------

void timer_run(void)
{
   static unsigned int counter;
   static enum {RESET,IDLE,RUN,WAIT,ALARM} state=RESET;
   static char textPos=0;
   static char lastTimer=0, timer=0;
   static char div2;

   switch (state)
   {
          case RESET:
                resetTune(tune);
                P2 |= 0x01; //Disable alarm (LEDS)
                textPos=0;  //Reset scrolled message
                state = IDLE;
                break;

          case IDLE:
                counter = getStartCount();
                updateDisplay(counter);
                if (kbhit())
                   if (getch()=='1')
                      state = RUN;
                break;

          case RUN:
               if  (P2 & 0x20) // External timer input is high
               {
                   counter = counter-1;
                   updateDisplay(counter);
                   if (counter==0)
                   {
                      P2 &= 0xFE; //Enable alarm (LEDS)
                      state = ALARM;
                   }
                   else
                      state = WAIT;
               }
               if (kbhit())
                  switch (getch())
                  {
                         case '1' : counter = getStartCount();   break;
                         case '2' : state = RESET;               break;
                  }
               break;

          case WAIT:
               if ((P2 & 0x20)==0 || kbhit()) //External timer input is low
                  state = RUN;
               break;

          case ALARM:
               PlayTune();
               timer = P2 & 0x10;
               if ((lastTimer != timer) && timer)
               {
                  if (div2)
                  {
                   seven_scroll_puts(textPos,scroll_text);
                   textPos = (textPos+1) % (sizeof(scroll_text)-1);
                  }
                  div2 = !div2;
               }
               lastTimer = timer;

               if (kbhit())
                  state = RESET;
               break;

   }
}

//---------------------
// Main entry point
//---------------------

void main(void)
{
  for (;;)
  {
    timer_run();
  }
}
