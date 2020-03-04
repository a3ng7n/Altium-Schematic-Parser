#ifndef __NBT_TIMER_H__
#define __NBT_TIMER_H__

#include "hware.h"

#define T0_INT_FREQ (((FOSC / 12.0) / 0x1FFF)+.5)          // ~203.47Hz@20MHz
                                                           // so maximum time is 600ms @20MHz s

#define TIMER_SECONDS(x) ((unsigned int) ((x) * (unsigned long)FOSC / (12 * 0x1FFFL)))

enum {TIMER_0,TIMER_1,TIMER_LCD, TIMER_BEEP, TIMER_NO_OF}; // These get counted down in the timer0_ISR

extern __idata volatile unsigned char Timer[TIMER_NO_OF];

/*-----------------------------------------------------
Init timer0 in Mode 0 with maximum prescaler
------------------------------------------------------*/
void Timer0Init(void);
            
/*------------------------------------------------
Timer 0 Interrupt Service Routine.

Set a breakpoint on 'overflow_count++' and run the
program in the debugger.  You will see this line
executes every 65536 clock cycles.
------------------------------------------------*/
__interrupt(INTVEC_T0) void timer0_ISR (void);

#endif // __NBT_TIMER_H__

