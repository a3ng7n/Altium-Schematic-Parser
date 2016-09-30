/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Timing utility functions
|*
\*****************************************************************************/

#ifndef __UTIL_TIMING_H__
#define __UTIL_TIMING_H__

void               timing_set_clock_freq_hz        ( unsigned int freq );
unsigned int       timing_get_clock_freq_hz        ( void );

unsigned long long timing_get_tick_count           ( void );
unsigned int       timing_get_tick_period_ns       ( void );

unsigned int       timing_elapsed_time_ns          ( unsigned long long since_tick );
unsigned int       timing_elapsed_time_us          ( unsigned long long since_tick );
unsigned int       timing_elapsed_time_ms          ( unsigned long long since_tick );

unsigned long long timing_get_ticks_ns             ( unsigned long long ns );
unsigned long long timing_get_ticks_us             ( unsigned long long us );
unsigned long long timing_get_ticks_ms             ( unsigned long long ms );

void               timing_delay_ns                 ( unsigned long long ns );
void               timing_delay_us                 ( unsigned long long us );
void               timing_delay_ms                 ( unsigned long long ms );

#endif
