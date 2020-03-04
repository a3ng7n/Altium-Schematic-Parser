///////////////////////////////////////////////////////////////////////////////
// NB2_LedDriver.h
#ifndef __NB2_LED_DRIVER_H
#define __NB2_LED_DRIVER_H

#include <drv_max6966.h>

extern void NB2LD_Init(max6966_t *drv0, max6966_t *drv1);
extern void NB2LD_SetLedRGB(uint8_t ledIdx, uint8_t  r, uint8_t  g, uint8_t  b);
extern void NB2LD_GetLedRGB(uint8_t ledIdx, uint8_t *r, uint8_t *g, uint8_t *b);

#endif // __NB2_LED_DRIVER_H
