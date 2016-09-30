/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:         Edge detector
|*
|*  COPYRIGHT:          Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:        Interface to edge detection functions
|*
 */

#ifndef _EDGEDET_H
#define _EDGEDET_H

#include <stdint.h>

// Global functions in hardware
extern void hw_find_edges( uint16_t * input, uint16_t * output, uint16_t width, uint16_t height, uint16_t xres, unsigned short divisor );
extern uint16_t hw_luma( uint16_t pixel );

// Global functions in software
extern void sw_find_edges( uint16_t * input, uint16_t * output, uint16_t width, uint16_t height, uint16_t xres, unsigned short divisor );
extern uint16_t sw_luma( uint16_t pixel );

#endif // _EDGEDET_H
