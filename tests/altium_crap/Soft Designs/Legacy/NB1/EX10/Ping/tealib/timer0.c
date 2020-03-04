#include "timer0.h"

#ifdef TMR0_IDLE
 #define TMR_IDLE TMR0_IDLE
#endif

#include "timer.c"  /* include common source */

#ifdef TMR0_TICKS_PER_SECOND
 #if TMR0_TICKS_PER_SECOND+0 == 0
  #undef TMR0_TICKS_PER_SECOND
  #define TMR0_TICKS_PER_SECOND 1000
 #endif
#endif

 
/*****************************************************************************
 *
 *  FUNCTION:   init
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: if TMR0_TICKS_PER_SECOND is defined: none
 *          if TMR0_TICKS_PER_SECOND not defined: ticks_per_second
 *
 *  RETURN VALUE:   none
 *
 *  DESCRIPTION:    initialize the timer to reach the given ticks/second
 */
#ifndef TMR0_TICKS_PER_SECOND
void init( unsigned long ticks_per_second )
{
#else
void init( void )
{
 #define ticks_per_second TMR0_TICKS_PER_SECOND
#endif
    unsigned long x;

    TR0 = 0;    /* stop timer during init */

    /*
     * calculate values to reach ticks/sec (tps)
     * Fosc/12 = timer clk (Ft)
     * x = Ft/tps
     * x = ( timer interrupt rate ) * irqticks
     * irqticks = Ft/tps/256 +1 (always round up to make sure interrupt rate < 256)
     * interrupt rate = Ft/tps/irqticks (must be rounded off)
     * TH0 = 256 - interrupt rate
     */

    x = ( FOSC / 12U ) / ticks_per_second;
    reload_ticks = ( x >> 8 ) + 1;              /* +1 = round up */
    if (( x % reload_ticks ) > ( reload_ticks >> 1 ))   /* round off */
    {
        TH0 = 256 - ( unsigned char )(( x / reload_ticks ) + 1 );    /* round up */
    }
    else
    {
        TH0 = 256 - ( unsigned char )( x / reload_ticks );       /* round down */
    }

    TL0 = TH0;

    TMOD = ( TMOD & 0xF0 ) | 0x02;  /* set timer/counter 0 to 8 bit auto-reload timer */

    system_time = 0;
    tick_down = reload_ticks;

    /* set interrupt priority */
#if TMR0_INT_PRIO == 0
    IP = IP & ~0x02;
#elif TMR0_INT_PRIO == 1
    IP = IP | 0x02;
#endif

    /* turn on the timer and interrupt */
    TR0 = 1;
    ET0 = 1;
    EA = 1;
}


