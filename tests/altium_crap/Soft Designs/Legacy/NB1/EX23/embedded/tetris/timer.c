/******************************************************************************
**  FILE:	    @(#)timer.c	1.1 04/08/18 
**  DESCRIPTION:    RTOS Timer initialization (TSK51A)   
******************************************************************************/
#include <osek/osek.h>

#define CLOCK_HZ            44236800L
#define CLOCKPRESCALE       12

/************************************************************************
 * TIMERRATE specifies how many times per second the system timer 
 * tick must occur (i.e. number of clock interrupts per second).
 ***********************************************************************/
#define  TIMERRATE      ((1000)/(OSTICKDURATIONINMSCS))

/* These values should change if TickType is not 'unsigned int' anymore */
#define MIN_RELOAD_VALUE        0
#define MAX_RELOAD_VALUE        65535

#define TIMER0  0
#define TIMER1  1
#define TIMER   TIMER0

#if  (TIMER ==  TIMER0)
    #define TL              TL0
    #define TH              TH0
    #define TR              TR0
    #define PT              PT0
    #define ET              ET0
    #define TMODV           0x01
#elif (TIMER == TIMER1)
    #define TL              TL1
    #define TH              TH1
    #define TR              TR1
    #define PT              PT1
    #define ET              ET1
    #define TMODV           0x10
#endif


static unsigned char reload_value_low;
static unsigned char reload_value_high;


void InitRTOSTimer( void)
{
    unsigned int reload_value;

    if (TIMERRATE  >= (CLOCK_HZ/CLOCKPRESCALE) )
    {
        reload_value = MAX_RELOAD_VALUE;
    }
    else if ( TIMERRATE <= (CLOCK_HZ/(CLOCKPRESCALE*65535)) )
    {
        reload_value = MIN_RELOAD_VALUE;
    }
    else
    {
        reload_value = 65535 - (unsigned int)(CLOCK_HZ/(CLOCKPRESCALE*TIMERRATE));
    }

    TMOD |= TMODV; /* set timer in 16-bit timer function (no auto-reload) */

    TL = reload_value_low = reload_value & 0xff; /* set timer 1 value */
    TH = reload_value_high = reload_value >> 8;

    TR = 1;
    PT = 0;
    ET = 1;
    return;
}

void DisableRTOSTimer(void)
{
    ET = 0;
}

void EnableRTOSTimer(void)
{
    ET = 1;
}

void ReloadRTOSTimer(void)
{
    TL = reload_value_low;
    TH = reload_value_high;
}





