/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   TSK3000 specific functions
|*
\*****************************************************************************/

#include "proc_tsk3000.h"

void tsk3000_set_status_reg(unsigned int value)
{
    __mtc0(value,TSK3000_COP_Status);
}

unsigned int tsk3000_get_status_reg(void)
{
    return __mfc0(TSK3000_COP_Status);
}

void tsk3000_set_vectored_interrupts( unsigned int enabled )
{
    unsigned int status;
    status = tsk3000_get_status_reg();
    if (enabled)
    {
        status |=  TSK3000_Status_VectorModeEnable;
    }
    else
    {
        status &= !TSK3000_Status_VectorModeEnable;
    }
    tsk3000_set_status_reg(status);
}

void tsk3000_set_enabled_interrupts(unsigned int value)
{
    __mtc0(value,TSK3000_COP_InterruptEnable);
}

unsigned int tsk3000_get_enabled_interrupts(void)
{
    return __mfc0(TSK3000_COP_InterruptEnable);
}

void tsk3000_clear_interrupt_edge_flags(unsigned int value)
{
    __mtc0(value,TSK3000_COP_InterruptPending);
}

unsigned int tsk3000_get_pending_interrupts(void)
{
    return __mfc0(TSK3000_COP_InterruptPending);
}

unsigned int tsk3000_get_highest_pending_interrupt(void)
{
    return (__mfc0(TSK3000_COP_Status) >> 11) & 0x1F;
}

unsigned int tsk3000_get_timebase_lo(void)
{
    return __mfc0(TSK3000_COP_TimebaseLO);
}

unsigned int tsk3000_get_timebase_hi(void)
{
    return __mfc0(TSK3000_COP_TimebaseHI);
}

void tsk3000_set_interval_timer(unsigned int value)
{
    __mtc0(value,TSK3000_COP_Compare);
}

unsigned long long tsk3000_get_timebase ( void )
{
    unsigned int hiword_1;
    unsigned int hiword_2;
    unsigned int loword;

    do
    {
         hiword_1 = tsk3000_get_timebase_hi ();
         loword   = tsk3000_get_timebase_lo ();
         hiword_2 = tsk3000_get_timebase_hi ();
    } while (hiword_1 != hiword_2);

    return ((unsigned long long) hiword_1 << 32) + loword;
}

unsigned int tsk3000_get_interval_timer(void)
{
    return __mfc0(TSK3000_COP_Compare);
}

void tsk3000_reset_interval_timer(void)
{
    tsk3000_set_status_reg(tsk3000_get_status_reg() | ( TSK3000_Status_IntervalTimerReset));
    tsk3000_set_status_reg(tsk3000_get_status_reg() & (~TSK3000_Status_IntervalTimerReset));
}

void tsk3000_set_exception_return(unsigned int value)
{
    __mtc0(value,TSK3000_COP_ExceptionReturn);
}

unsigned int tsk3000_get_exception_return(void)
{
    return __mfc0(TSK3000_COP_ExceptionReturn);
}

void tsk3000_set_exception_base(unsigned int value)
{
    __mtc0(value,TSK3000_COP_ExceptionBase);
}

unsigned int tsk3000_get_exception_base(void)
{
    return __mfc0(TSK3000_COP_ExceptionBase);
}

void tsk3000_set_interrupt_mode(unsigned int value)
{
    __mtc0(value,TSK3000_COP_InterruptMode);
}

unsigned int tsk3000_get_interrupt_mode(void)
{
    return __mfc0(TSK3000_COP_InterruptMode);
}

void tsk3000_enable_interrupts(void)
{
    tsk3000_set_status_reg(tsk3000_get_status_reg() | TSK3000_Status_InterruptEnable);
}

void tsk3000_disable_interrupts(void)
{
    tsk3000_set_status_reg(tsk3000_get_status_reg() & (~TSK3000_Status_InterruptEnable));
}

void tsk3000_enable_interval_timer(void)
{
    tsk3000_set_status_reg(tsk3000_get_status_reg() | TSK3000_Status_IntervalTimerEnable);
}

void tsk3000_disable_interval_timer(void)
{
    tsk3000_set_status_reg(tsk3000_get_status_reg() & (~TSK3000_Status_IntervalTimerEnable));
}


