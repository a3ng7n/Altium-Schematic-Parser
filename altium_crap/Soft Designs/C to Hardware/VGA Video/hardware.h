//..............................................................................
// Automatically generated header file.
// This file should not be edited.
//..............................................................................

#ifndef __HARDWARE_H__
#define __HARDWARE_H__

//..............................................................................
#define Base_FILTER                0xFF020000
#define Size_FILTER                0x00000001
//..............................................................................

//..............................................................................
#define Base_I2C                   0xFF030000
#define Size_I2C                   0x00000008
//..............................................................................

//..............................................................................
#define Base_VGA                   0xFF010000
#define Size_VGA                   0x00001000
#define Intr_VGA_A                 10
#define Intr_VGA_B                 11
#define Intr_VGA_C                 12
//..............................................................................

//..............................................................................
#define Base_VIDEO                 0xFF000000
#define Size_VIDEO                 0x00000008
#define Intr_VIDEO_A               6
#define Intr_VIDEO_B               7
//..............................................................................

//..............................................................................
#define INTERRUPT_CONTROL_CFG      0x00001CDE
#define INTERRUPT_KINDS_CFG        0x00001CDE
#define INTERRUPT_EDGE_KIND_CFG    0x00001CDE
#define INTERRUPT_LVL_KIND_CFG     0x00000000
//..............................................................................

//..............................................................................
#define Base_MCU                   0x00000000
#define Size_MCU                   0x00004000
//..............................................................................

//..............................................................................
#define Base_XRAM1                 0x02000000
#define Size_XRAM1                 0x00100000
//..............................................................................

//..............................................................................
#define Base_XRAM2                 0x01000000
#define Size_XRAM2                 0x00100000
//..............................................................................

//..............................................................................
#define CLOCK_BOARD
#define DIGITAL_IO
#define DIGITAL_IO
#define WB_I2CM
#define VGA32_16BPP
#define BT656
#define WB_PRTIO
#define WB_MEM_CTRL
//..............................................................................

#endif // __HARDWARE_H__
