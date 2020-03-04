/*****************************************************************************
 *      FILE:          tealib_cfg.h
 *
 *      CREATED:       Thu Aug 14 15:19:31 2003
 *
 *      IN PACKAGE:    8051 peripheral library
 *
 *      TARGET:        P89C66x
 *
 *      COPYRIGHT:     Copyright 2001 - 2003 Altium BV
 *
 *      DESCRIPTION:   System describer headerfile.
 *                     
 *                     This file describes the embedded system (what
 *                     peripherals are used, the oscillator frequency), so
 *                     the library headerfile 'tealib.h' automatically
 *                     includes the required files.
 *
 ****************************************************************************/

#ifndef _TEALIB_CFG_H
#define _TEALIB_CFG_H

#define FOSC    44236800L

//#define   UART0
//#define   TMR0
//#define   TMR1
//#define   CNT0
//#define   CNT1
//#define   KEYB0
//#define   MOUSE0
#define LCD0
#define KEYMATRIX0

#endif
