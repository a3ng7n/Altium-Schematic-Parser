/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   TSKNIOS2 specific functions
|*
\*****************************************************************************/

#include "proc_tsknios2.h"
#include "time.h"

void tsknios2_set_status_reg(unsigned int value)
{
    __asm("wrctl ctl0, %0" : "=r"(value) : : );
}

unsigned int tsknios2_get_status_reg(void)
{
    unsigned int result;
    __asm("rdctl %0, ctl0" : "=r"(result) : : );
    return result;
}

void tsknios2_set_vectored_interrupts( unsigned int enabled )
{
    unsigned int status;
    status = tsknios2_get_status_reg();
    if (enabled)
    {
        status |=  TSKNIOS2_STATUS_INTERRUPT_ENABLE;
    }
    else
    {
        status &= !TSKNIOS2_STATUS_INTERRUPT_ENABLE;
    }
    tsknios2_set_status_reg(status);
}

void tsknios2_set_enabled_interrupts(unsigned int value)
{
    __asm("wrctl ctl3, %0" : "=r"(value) : : );
}

unsigned int tsknios2_get_enabled_interrupts(void)
{
    unsigned int result;
    __asm("rdctl %0, ctl3" : "=r"(result) : : );
    return result;
}

unsigned int tsknios2_get_pending_interrupts(void)
{
    unsigned int result;
    __asm("rdctl %0, ctl4" : "=r"(result) : : );
    return result;
}

unsigned int tsknios2_get_timebase_lo(void)
{
    return *((unsigned int*) TSKNIOS2_TIMEBASE_LOW);
}

unsigned int tsknios2_get_timebase_hi(void)
{
    return *((unsigned int*) TSKNIOS2_TIMEBASE_HI);
}

void tsknios2_set_interval_timer(unsigned int value)
{
    *((unsigned int*) TSKNIOS2_TIMEBASE_INTERVAL_TIMER) = value;
}

unsigned long long tsknios2_get_timebase ( void )
{
    unsigned int hiword;
    unsigned int loword;

    loword = tsknios2_get_timebase_lo ();
    hiword = tsknios2_get_timebase_hi ();

    return ((unsigned long long) hiword << 32) + loword;
}

unsigned int tsknios2_get_interval_timer(void)
{
    return *((unsigned int*) TSKNIOS2_TIMEBASE_INTERVAL_TIMER);
}

void tsknios2_reset_interval_timer(void)
{
    *((unsigned int*) TSKNIOS2_TIMEBASE_STATUS) = TSKNIOS2_TIMEBASE_INTERVAL_TIMER_RESET;
}

void tsknios2_enable_interrupts(void)
{
    tsknios2_set_status_reg(tsknios2_get_status_reg() | TSKNIOS2_STATUS_INTERRUPT_ENABLE);
}

void tsknios2_disable_interrupts(void)
{
    tsknios2_set_status_reg(tsknios2_get_status_reg() & (~TSKNIOS2_STATUS_INTERRUPT_ENABLE));
}

void tsknios2_enable_interval_timer(void)
{
    *((unsigned int*) TSKNIOS2_TIMEBASE_STATUS) = TSKNIOS2_TIMEBASE_INTERVAL_TIMER_ENABLE;
}

void tsknios2_disable_interval_timer(void)
{
    *((unsigned int*) TSKNIOS2_TIMEBASE_STATUS) = *((unsigned int*) TSKNIOS2_TIMEBASE_STATUS) & ~TSKNIOS2_TIMEBASE_INTERVAL_TIMER_ENABLE;
}  

//------------------------------------------------------------------------------
// needed for the std libraries
// TODO: ask Tasking to add this into std lib
clock_t clock(void)
{
    return tsknios2_get_timebase();
}
//------------------------------------------------------------------------------

