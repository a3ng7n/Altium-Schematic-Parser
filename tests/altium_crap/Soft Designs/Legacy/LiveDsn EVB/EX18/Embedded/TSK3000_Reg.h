#ifndef TSK3000_REG_H
#define TSK3000_REG_H


//..............................................................................
#define COP_Status           0
#define COP_InterruptEnable  1
#define COP_InterruptPending 2
#define COP_TimebaseLO       3
#define COP_TimebaseHI       4
#define COP_Compare          5
#define COP_DebugData        6
#define COP_ExceptionReturn  7
#define COP_ExceptionBase    8
#define COP_InterruptMode    9
//..............................................................................

//..............................................................................
#define Status_InterruptEnable           0x0001
#define Status_UserMode                  0x0002
#define Status_InterruptEnable_Previous  0x0004
#define Status_UserMode_Previous         0x0008
#define Status_InterruptEnable_Old       0x0010
#define Status_UserMode_Old              0x0020
#define Status_Reserved0                 0x0040
#define Status_IntervalTimerReset        0x0080
#define Status_IntervalTimerEnable       0x0100
#define Status_VectorModeEnable          0x0200
#define Status_WishboneTimeOut           0x0400
//..............................................................................

//..............................................................................
void         SetStatusRegister         (unsigned int value);
unsigned int GetStatusRegister         (void);
//..............................................................................

//..............................................................................
void         SetEnabledInterrupts      (unsigned int value);
unsigned int GetEnabledInterrupts      (void);
//..............................................................................

//..............................................................................
void         ClearInterruptEdgeFlags   (unsigned int value);
unsigned int GetPendingInterrupts      (void);
unsigned int GetHighestPendingInterrupt(void);
//..............................................................................

//..............................................................................
unsigned int GetTimeBase_LO            (void);
unsigned int GetTimeBase_HI            (void);
//..............................................................................

//..............................................................................
void         SetIntervalTimer          (unsigned int value);
unsigned int GetIntervalTimer          (void);
void         ResetIntervalTimer        (void);
//..............................................................................

//..............................................................................
void         SetExceptionReturn        (unsigned int value);
unsigned int GetExceptionReturn        (void);
//..............................................................................

//..............................................................................
void         SetExceptionBase          (unsigned int value);
unsigned int GetExceptionBase          (void);
//..............................................................................

//..............................................................................
void         SetInterruptMode          (unsigned int value);
unsigned int GetInterruptMode          (void);
//..............................................................................

//..............................................................................
void         EnableInterrupts          (void);
void         DisableInterrupts         (void);
void         EnableIntervalTimer       (void);
void         DisableIntervalTimer      (void);
//..............................................................................



#endif
