#ifndef __PROC_PPC405CR_H__
#define __PROC_PPC405CR_H__

#if 0
// addresses allowed by cppc.exe (before 1.107)
#define Interrupt_Critical            0x0100
#define Interrupt_MachineCheck        0x0200
#define Interrupt_DataStorage         0x0300
#define Interrupt_InstructionStorage  0x0400
#define Interrupt_ExternalInterrupt   0x0500
#define Interrupt_Alignment           0x0600
#define Interrupt_Program             0x0700
#define Interrupt_SystemCall          0x0C00
#define Interrupt_PIT                 0x1000
#define Interrupt_FIT                 0x1010
#define Interrupt_Watchdog            0x1020
#define Interrupt_DataTLBMiss         0x1100
#define Interrupt_InstructionTLBMiss  0x1200
#define Interrupt_DebugBT             0x2000
#else
// numbers used by newer cppc.exe (1.107 and up)
#define Interrupt_Critical                  0
#define Interrupt_MachineCheck              1
#define Interrupt_DataStorage               2
#define Interrupt_InstructionStorage        3
#define Interrupt_ExternalInterrupt         4
#define Interrupt_Alignment                 5
#define Interrupt_Program                   6
//#define Interrupt_FPU                       7
#define Interrupt_SystemCall                8
//#define Interrupt_APU                       9
#define Interrupt_PIT                       10
#define Interrupt_FIT                       11
#define Interrupt_Watchdog                  12
#define Interrupt_DataTLBMiss               13
#define Interrupt_InstructionTLBMiss        14
#define Interrupt_DebugBT                   15
#endif

void         ppc405cr_set_programmable_interval_timer        (unsigned int value);

void         ppc405cr_enable_interrupts                      (void);
void         ppc405cr_disable_interrupts                     (void);
void         ppc405cr_enable_programmable_interval_timer     (void);
void         ppc405cr_disable_programmable_interval_timer    (void);
void         ppc405cr_clear_programmable_interval_timer_flag (void);

unsigned int ppc405cr_get_exception_vector_prefix            (void);
void         ppc405cr_set_exception_vector_prefix            (unsigned int absolute_address);


#endif
