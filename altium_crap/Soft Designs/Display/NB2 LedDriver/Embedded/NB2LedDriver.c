///////////////////////////////////////////////////////////////////////////////
// NB2LedDriver.c
////////////////////////////////////////////////////////////////////////////////
//
//  IN PACKAGE:         Software Platform examples
//
//  COPYRIGHT:          Copyright (c) 2008, Altium
//
//  DESCRIPTION:        This example impliments an NB2 Speaker Board LED Driver
//
////////////////////////////////////////////////////////////////////////////////

#include <assert.h>
#include <drv_max6966.h>

#include "NB2LedDriver.h"

///////////////////////////////////////////////////////////////////////////////
typedef struct NB2_RGBLED_T {
    uint8_t drvIdx;

    uint8_t r_chan;
    uint8_t b_chan;
    uint8_t g_chan;

    uint8_t r_cur;
    uint8_t g_cur;
    uint8_t b_cur;

} NB2_RGBLED_T;

///////////////////////////////////////////////////////////////////////////////
NB2_RGBLED_T NB2_RGBLEDS[6] = {
        { 1, 8, 7, 6, 0, 0, 0},
        { 1, 5, 4, 3, 0, 0, 0},
        { 1, 2, 1, 0, 0, 0, 0},
        { 0, 8, 7, 6, 0, 0, 0},
        { 0, 5, 4, 3, 0, 0, 0},
        { 0, 2, 1, 0, 0, 0, 0},
    };

///////////////////////////////////////////////////////////////////////////////
max6966_t *MAX6966Drivers[2] = {0, 0};

///////////////////////////////////////////////////////////////////////////////
void NB2LD_Init(max6966_t *drv0, max6966_t *drv1)
{
    assert(drv0 && drv1);

    MAX6966Drivers[0] = drv0;
    MAX6966Drivers[1] = drv1;
}

///////////////////////////////////////////////////////////////////////////////
void NB2LD_SetLedRGB(uint8_t ledIdx, uint8_t  r, uint8_t  g, uint8_t  b)
{
    assert(ledIdx < (sizeof(NB2_RGBLEDS)/sizeof(NB2_RGBLED_T)));

    max6966_set_channel_duty(MAX6966Drivers[NB2_RGBLEDS[ledIdx].drvIdx], NB2_RGBLEDS[ledIdx].r_chan, r);
    max6966_set_channel_duty(MAX6966Drivers[NB2_RGBLEDS[ledIdx].drvIdx], NB2_RGBLEDS[ledIdx].g_chan, g);
    max6966_set_channel_duty(MAX6966Drivers[NB2_RGBLEDS[ledIdx].drvIdx], NB2_RGBLEDS[ledIdx].b_chan, b);

    NB2_RGBLEDS[ledIdx].r_cur = r;
    NB2_RGBLEDS[ledIdx].g_cur = g;
    NB2_RGBLEDS[ledIdx].b_cur = b;
}

///////////////////////////////////////////////////////////////////////////////
void NB2LD_GetLedRGB(uint8_t ledIdx, uint8_t *r, uint8_t *g, uint8_t *b)
{
    *r = NB2_RGBLEDS[ledIdx].r_cur;
    *g = NB2_RGBLEDS[ledIdx].g_cur;
    *b = NB2_RGBLEDS[ledIdx].b_cur;
}
