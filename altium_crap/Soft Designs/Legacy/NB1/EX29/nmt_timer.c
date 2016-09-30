//-----------------------------------------------------------------
//      _   _  __  __  _____   _____  ___  __  __  _____  ____
//     | \ | ||  \/  ||_   _| |_   _||_ _||  \/  || ____||  _ \
//     |  \| || |\/| |  | |     | |   | | | |\/| ||  _|  | |_) |
//     | |\  || |  | |  | |     | |   | | | |  | || |___ |  _ <
//     |_| \_||_|  |_|  |_|_____|_|  |___||_|  |_||_____||_| \_\
//                        |_____|
//
// (c) 2003 Altium
// Started: 18.11.2003 Ch.Weimann
// Timer related stuff for NanoBoard Tester
//-----------------------------------------------------------------

#ifndef __NBT_TIMER_H__
#define __NBT_TIMER_H__

#include "hware.h"
#include "nmt_kbd.h"

#define T0_INT_FREQ (((FOSC / 12.0) / 0x1FFF)+.5) 

enum {TIMER_0,TIMER_1,TIMER_NO_OF};  // These get counted down in the timer0_ISR

__idata volatile unsigned char Timer[TIMER_NO_OF];

/*-----------------------------------------------------
Init timer0 in Mode 0 with maximum prescaler
------------------------------------------------------*/
void Timer0Init(void)
{
  /*-------------------------------------
  Set the Timer0 Run control bit.
  --------------------------------------*/
  TMOD = (TMOD & 0xF0) | 0x00;  /* Set T/C0 Mode 13 bit counter */
  TL0  = 0xFF;                  /* Set prescaler to maximum */
  ET0 = 1;                      /* Enable Timer 0 Interrupts */
  TR0 = 1;                      /* Start Timer 0 Running */
}


/*------------------------------------------------
Timer 0 Interrupt Service Routine.

Set a breakpoint on 'overflow_count++' and run the
program in the debugger.  You will see this line
executes every 65536 clock cycles.
------------------------------------------------*/
__interrupt(INTVEC_T0) void timer0_ISR (void)
{
  unsigned char i;
  for(i=0;i < TIMER_NO_OF; i++)     // handle software timers
  {
    if(Timer[i]) Timer[i]--;
  }
  Kbd_DoScan();        // scan Keyboard
}

#endif // __NBT_TIMER_H__



