/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   TSK3000 specific functions
|*
\*****************************************************************************/

#ifndef __TSK3000_H__
#define __TSK3000_H__

#define TSK3000_COP_Status           0
#define TSK3000_COP_InterruptEnable  1
#define TSK3000_COP_InterruptPending 2
#define TSK3000_COP_TimebaseLO       3
#define TSK3000_COP_TimebaseHI       4
#define TSK3000_COP_Compare          5
#define TSK3000_COP_DebugData        6
#define TSK3000_COP_ExceptionReturn  7
#define TSK3000_COP_ExceptionBase    8
#define TSK3000_COP_InterruptMode    9

#define TSK3000_Status_InterruptEnable           0x0001
#define TSK3000_Status_UserMode                  0x0002
#define TSK3000_Status_InterruptEnable_Previous  0x0004
#define TSK3000_Status_UserMode_Previous         0x0008
#define TSK3000_Status_InterruptEnable_Old       0x0010
#define TSK3000_Status_UserMode_Old              0x0020
#define TSK3000_Status_Reserved0                 0x0040
#define TSK3000_Status_IntervalTimerReset        0x0080
#define TSK3000_Status_IntervalTimerEnable       0x0100
#define TSK3000_Status_VectorModeEnable          0x0200
#define TSK3000_Status_WishboneTimeOut           0x0400

void               tsk3000_set_status_reg                  ( unsigned int value );
unsigned int       tsk3000_get_status_reg                  ( void );

void               tsk3000_set_vectored_interrupts         ( unsigned int enabled );

void               tsk3000_set_enabled_interrupts          ( unsigned int value );
unsigned int       tsk3000_get_enabled_interrupts          ( void );

void               tsk3000_clear_interrupt_edge_flags      ( unsigned int value );
unsigned int       tsk3000_get_pending_interrupts          ( void );
unsigned int       tsk3000_get_highest_pending_interrupt   ( void );

unsigned int       tsk3000_get_timebase_lo                 ( void );
unsigned int       tsk3000_get_timebase_hi                 ( void );
unsigned long long tsk3000_get_timebase                    ( void );

void               tsk3000_set_interval_timer              ( unsigned int value );
unsigned int       tsk3000_get_interval_timer              ( void );
void               tsk3000_reset_interval_timer            ( void );

void               tsk3000_set_exception_return            ( unsigned int value );
unsigned int       tsk3000_get_exception_return            ( void );

void               tsk3000_set_exception_base              ( unsigned int value );
unsigned int       tsk3000_get_exception_base              ( void );

void               tsk3000_set_interrupt_mode              ( unsigned int value );
unsigned int       tsk3000_get_interrupt_mode              ( void );

void               tsk3000_enable_interrupts               ( void );
void               tsk3000_disable_interrupts              ( void );
void               tsk3000_enable_interval_timer           ( void );
void               tsk3000_disable_interval_timer          ( void );

#endif
