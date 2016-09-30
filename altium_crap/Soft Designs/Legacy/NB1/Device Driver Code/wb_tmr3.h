/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   TMR3 Dual Timer device driver
|*
\*****************************************************************************/

#ifndef __WB_TMR3_DUAL_TIMER_H__
#define __WB_TMR3_DUAL_TIMER_H__

typedef enum
{
    mode0_timer,
    mode0_gated_timer,
    mode0_counter,
    mode0_gated_counter,
    mode1_timer,
    mode1_gated_timer,
    mode1_counter,
    mode1_gated_counter,
    mode2_timer,
    mode2_gated_timer,
    mode2_counter,
    mode2_gated_counter,
    mode3_timer,
    mode3_gated_timer,
    mode3_counter,
    mode3_gated_counter
} tmr3_mode_t;

void          tmr3_init             ( unsigned int base );
void          tmr3_t1_init          ( unsigned int base );
void          tmr3_t2_init          ( unsigned int base );

void          tmr3_set_tmod         ( unsigned int base, unsigned char tmod );
unsigned char tmr3_get_tmod         ( unsigned int base );
void          tmr3_set_tcon         ( unsigned int base, unsigned char tcon );
unsigned char tmr3_get_tcon         ( unsigned int base );

void          tmr3_set_th           ( unsigned int base, unsigned int timer, unsigned char th );
unsigned char tmr3_get_th           ( unsigned int base, unsigned int timer );
void          tmr3_set_tl           ( unsigned int base, unsigned int timer, unsigned char tl );
unsigned char tmr3_get_tl           ( unsigned int base, unsigned int timer );

void          tmr3_set_mode         ( unsigned int base, unsigned int timer, tmr3_mode_t mode );

void          tmr3_run              ( unsigned int base, unsigned int timer );
void          tmr3_stop             ( unsigned int base, unsigned int timer );

//..............................................................................
// These functions should be called to set the set a timer delay
// Clock_Freq must be divised by 12 when using the processor frequency (CLK_I)
//
// Only supported by Timer/Counter in Mode 0, 1 and 2
void          tmr3_set_delay_ms     ( unsigned int base, unsigned int timer, unsigned int clock_freq_hz, unsigned int delay_ms );

//..............................................................................
// These functions should be called by the timer interrupt handler
// This function reset the overflow flags (TF1 and/or TF2) and returns the timer that overflowed
// Use tmr3_run and tmr3_stop to control the timers
unsigned int  tmr3_handle_interrupt (unsigned int base );

#endif
