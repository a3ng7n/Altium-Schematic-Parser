/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Timing utility functions
|*
\*****************************************************************************/

#include "util_timing.h"
#include "time.h"

unsigned int proc_clock_freq   = 50 * 1000 * 1000; // in hz
unsigned int proc_clock_period = 20;               // in ns

void timing_set_clock_freq_hz ( unsigned int freq )
{
    proc_clock_freq   = freq;
    proc_clock_period = 1000 * 1000 * 1000 / freq;
}

unsigned int timing_get_clock_freq_hz ( void )
{
    return proc_clock_freq;
}

unsigned long long timing_get_tick_count ( void )
{
    return clock();
}

unsigned int timing_get_tick_period_ns ( void )
{
    return proc_clock_period;
}

unsigned long long timing_get_ticks_ns ( unsigned long long ns )
{
    return ns / proc_clock_period;
}

unsigned long long timing_get_ticks_us ( unsigned long long us )
{
    return timing_get_ticks_ns ( us * 1000 );
}

unsigned long long timing_get_ticks_ms ( unsigned long long ms )
{
    return timing_get_ticks_ns ( ms * 1000 * 1000 );
}

void timing_delay_ns ( unsigned long long ns )
{
    unsigned long long start;
    unsigned long long end;

    start = timing_get_tick_count();

    end = start + (ns / proc_clock_period);

    while (timing_get_tick_count() < end)
    {
    }
}

void timing_delay_us ( unsigned long long us )
{
    timing_delay_ns(us * 1000);
}

void timing_delay_ms ( unsigned long long ms )
{
    timing_delay_ns(ms * 1000 * 1000);
}

unsigned int timing_elapsed_time_ns ( unsigned long long since_tick )
{
    unsigned long long elapsed;
    elapsed = timing_get_tick_count();
    elapsed = (elapsed - since_tick) * proc_clock_period;
    return elapsed;
}

unsigned int timing_elapsed_time_us ( unsigned long long since_tick )
{
    unsigned long long elapsed;
    elapsed = timing_get_tick_count();
    elapsed = (elapsed - since_tick) * proc_clock_period / 1000;
    return elapsed;
}

unsigned int timing_elapsed_time_ms ( unsigned long long since_tick )
{
    unsigned long long elapsed;
    elapsed = timing_get_tick_count();
    elapsed = (elapsed - since_tick) * proc_clock_period / (1000 * 1000);
    return elapsed;
}


