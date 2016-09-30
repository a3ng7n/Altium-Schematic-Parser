#include "timer.h"
#include "hware.h"

__idata volatile TIMER_DATATYPE Timer[TIMER_NO_OF];

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
------------------------------------------------*/
__interrupt(INTVEC_T0) void timer0_ISR (void)
{
  unsigned char i;
  for(i=0;i < TIMER_NO_OF; i++)     // handle software timers
  {
    if(Timer[i]) Timer[i]--;
  }
}




















