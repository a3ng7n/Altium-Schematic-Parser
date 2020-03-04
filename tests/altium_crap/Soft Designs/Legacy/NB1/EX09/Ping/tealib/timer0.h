#ifndef TIMER0_H
#define TIMER0_H

/*
 * USER DEFINED part of the header file
 * adjust your setting here
 */

#define TMR0_TICKS_PER_SECOND 100
//#define TMR0_IDLE idle_loop1
#define TMR0_INT_PRIO 1     /* sets also the external interrupt 2 priority */
#define TMR0_TIMER_SIZE 4


/* 
 * HARDWARE / SYSTEM DEFINED part of the header file
 * DO NOT EDIT BELOW THIS LINE
 */

#undef TMR_NR
#define TMR_NR 0

#include "tealib_cfg.h"
#include "timer.h"

#undef TMR_INT
#define TMR_INT 0x0b

#ifndef TMR0_TICKS_PER_SECOND
void init( unsigned long ticks_per_second );
#else
void init( void );
#endif /* TMR0_TICKS_PER_SECOND */

#endif /* TIMER0_H */
