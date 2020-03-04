/**************************************************************************
**                                                                        *
**  FILE        :  clock.c                                                *
**                                                                        *
**  DESCRIPTION :  The clock function returns the current processor time. *
**         To determine the time in seconds, the value returned   *
**         by the clock function should be divided by the value   *
**         of the macro CLOCKS_PER_SEC, defined in <time.h>.      *
**                                                                *
**  Copyright 1996-2005 Altium BV                                         *
**                                                                        *
**************************************************************************/

#include <time.h>
/*****************************************************************************/
/*  RTC (Real Time Clock) Control Registers                                  */
/*****************************************************************************/

#define RTCBase         0xFFFE0000  /* Real Time Clock Registers             */
#define RTC_DR     *((volatile unsigned *)RTCBase)
#define RTC_MR     *((volatile unsigned *)(RTCBase + 0x04))
#define RTC_STAT   *((volatile unsigned *)(RTCBase + 0x08))
#define RTC_EOI    *((volatile unsigned *)(RTCBase + 0x08))
#define RTC_LR     *((volatile unsigned *)(RTCBase + 0x0C))
#define RTC_CR     *((volatile unsigned *)(RTCBase + 0x10))

/*******************************************************************************
 * Reset, Clock Generation and Power Control (RCPC) Register Summary
 ******************************************************************************/

#define RCPCBase        0xFFFE2000
#define RCPCCtrl        (RCPCBase + 0x000)
#define IDString        (RCPCBase + 0x004)
#define RCPCRemapCtrl   (RCPCBase + 0x008)
#define SoftReset       (RCPCBase + 0x00C)
#define ResetStatus     (RCPCBase + 0x010)
#define ResetStatusClr  (RCPCBase + 0x014)
#define HCLKPrescale    (RCPCBase + 0x018)
#define CpuClkPrescale  (RCPCBase + 0x01C)
#define PeriphClkCtrl   (RCPCBase + 0x024)
#define PeriphClkCtrl2  (RCPCBase + 0x028)
#define AHBClkCtrl      (RCPCBase + 0x02C)
#define PeriphClkSel    (RCPCBase + 0x030)
#define PeriphClkSel2   (RCPCBase + 0x034)
#define PWM0Prescale    (RCPCBase + 0x038)
#define PWM1Prescale    (RCPCBase + 0x03C)
#define LCDClkPrescale  (RCPCBase + 0x040)
#define SSPClkPrescale  (RCPCBase + 0x044)
#define IntConfig       (RCPCBase + 0x080)
#define IntClear        (RCPCBase + 0x084)
#define CoreClkConfig   (RCPCBase + 0x088)

/*****************************************************************************/
/*  Timer Registers                                  */
/*****************************************************************************/

#define TIMER0Base  0xFFFC4000  /* Timer0 Registers             */
#define TIMER0_LD    *((volatile unsigned *)TIMER0Base)
#define TIMER0_VALUE    *((volatile unsigned *)(TIMER0Base + 0x04))
#define TIMER0_CTRL    *((volatile unsigned *)(TIMER0Base + 0x08))
#define TIMER0_CLR    *((volatile unsigned *)(TIMER0Base + 0x0C))

#define TIMER1Base  0xFFFC4020  /* Timer1 Registers             */
#define TIMER1_LD    *((volatile unsigned *)TIMER1Base)
#define TIMER1_VALUE    *((volatile unsigned *)(TIMER1Base + 0x04))
#define TIMER1_CTRL    *((volatile unsigned *)(TIMER1Base + 0x08))
#define TIMER1_CLR    *((volatile unsigned *)(TIMER1Base + 0x0C))

#define TIMER2Base  0xFFFC5000  /* Timer2 Registers             */
#define TIMER2_LD    *((volatile unsigned *)TIMER2Base)
#define TIMER2_VALUE    *((volatile unsigned *)(TIMER2Base + 0x04))
#define TIMER2_CTRL    *((volatile unsigned *)(TIMER2Base + 0x08))
#define TIMER2_CLR    *((volatile unsigned *)(TIMER2Base + 0x0C))

#define TIMER3Base  0xFFFC5020  /* Timer3 Registers             */
#define TIMER3_LD    *((volatile unsigned *)TIMER3Base)
#define TIMER3_VALUE    *((volatile unsigned *)(TIMER3Base + 0x04))
#define TIMER3_CTRL    *((volatile unsigned *)(TIMER3Base + 0x08))
#define TIMER3_CLR    *((volatile unsigned *)(TIMER3Base + 0x0C))

/* The _clock() function is used by CrossView to simulate a timer tick register. */
/* This function must have 'extern' scope, to allow the simulator to set a breakpoint. */

clock_t _clock ( clock_t t )
{
      t = TIMER3_VALUE;
      t <<= 16;
      t |= TIMER2_VALUE;
      t <<= 16;
      t |= TIMER1_VALUE;
      t <<= 16;
      t |= TIMER0_VALUE;
    return t;
}

clock_t clock ( void )
{
    return _clock( (clock_t)0 );
}

void init_clock(void)
{
                                              
    *((unsigned int*) PeriphClkCtrl) = 0x0;
    *((unsigned int*) PeriphClkSel) = 0x00000180;
    TIMER0_LD = 0xFFFF;
    TIMER0_CTRL = 0xD0;
    TIMER1_LD = 0xFFFF;
    TIMER1_CTRL = 0xD0;
    TIMER2_LD = 0xFFFF;
    TIMER2_CTRL = 0xD0;
    TIMER3_LD = 0xFFFF;
    TIMER3_CTRL = 0xD0;
}

void reset_clock(void)
{
    TIMER0_LD = 0xFFFF;
    TIMER1_LD = 0xFFFF;
    TIMER2_LD = 0xFFFF;
    TIMER3_LD = 0xFFFF;
}

