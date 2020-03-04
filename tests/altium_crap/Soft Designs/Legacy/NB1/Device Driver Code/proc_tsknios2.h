/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   TSKNIOS2 specific functions
|*
\*****************************************************************************/

#ifndef __TSKNIOS2_H__
#define __TSKNIOS2_H__

#define TSKNIOS2_STATUS_INTERRUPT_ENABLE          0x1
#define TSKNIOS2_STATUS_USER_MODE                 0x2

#define TSKNIOS2_TIMEBASE_LOW                     0xFFFFFFE0
#define TSKNIOS2_TIMEBASE_HI                      0xFFFFFFE4
#define TSKNIOS2_TIMEBASE_INTERVAL_TIMER          0xFFFFFFE8
#define TSKNIOS2_TIMEBASE_STATUS                  0xFFFFFFEE

#define TSKNIOS2_TIMEBASE_INTERVAL_TIMER_ENABLE   0x1
#define TSKNIOS2_TIMEBASE_INTERRUPT_PENDING       0x2
#define TSKNIOS2_TIMEBASE_INTERRUPT_CLEAR         0x4
#define TSKNIOS2_TIMEBASE_INTERVAL_TIMER_RESET    0x8

#define TSKNIOS2_EXCEPTION_ADDRESS                0x100

void               tsknios2_set_status_reg                  ( unsigned int value );
unsigned int       tsknios2_get_status_reg                  ( void );

void               tsknios2_set_vectored_interrupts         ( unsigned int enabled );

void               tsknios2_set_enabled_interrupts          ( unsigned int value );
unsigned int       tsknios2_get_enabled_interrupts          ( void );

unsigned int       tsknios2_get_pending_interrupts          ( void );

unsigned int       tsknios2_get_timebase_lo                 ( void );
unsigned int       tsknios2_get_timebase_hi                 ( void );
unsigned long long tsknios2_get_timebase                    ( void );

void               tsknios2_set_interval_timer              ( unsigned int value );
unsigned int       tsknios2_get_interval_timer              ( void );
void               tsknios2_reset_interval_timer            ( void );

void               tsknios2_enable_interrupts               ( void );
void               tsknios2_disable_interrupts              ( void );
void               tsknios2_enable_interval_timer           ( void );
void               tsknios2_disable_interval_timer          ( void );


#endif
