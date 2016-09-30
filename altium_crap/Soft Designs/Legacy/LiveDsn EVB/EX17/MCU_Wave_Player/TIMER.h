#ifndef __TIMER_H__
#define __TIMER_H__

#define T0_INT_FREQ (((FOSC / 12.0) / 0x1FFF)+.5)

#define TIMER_SECONDS(x) ((unsigned int)(T0_INT_FREQ / x))

#define TIMER_DATATYPE unsigned char

enum {TIMER_UART_TIMEOUT,TIMER_1,TIMER_NO_OF};  // These get counted down in the timer0_ISR

extern __idata volatile TIMER_DATATYPE Timer[TIMER_NO_OF];

/*-----------------------------------------------------
Init timer0 in Mode 0 with maximum prescaler
------------------------------------------------------*/
void Timer0Init(void);

#endif
