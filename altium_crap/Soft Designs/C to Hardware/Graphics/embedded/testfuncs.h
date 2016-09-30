#ifndef __TESTFUNCS_H
#define __TESTFUNCS_H

#include <stdint.h>

extern void hw_plot( uint16_t * vgamem, uint16_t x, uint16_t y, uint16_t color );
extern void sw_plot( uint16_t * vgamem, uint16_t x, uint16_t y, uint16_t color );
extern void hw_circle( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t radius, uint16_t color );
extern void sw_circle( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t radius, uint16_t color );
extern void hw_line( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1, uint16_t color );
extern void sw_line( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1, uint16_t color );

#endif // __TESTFUNCS.H
