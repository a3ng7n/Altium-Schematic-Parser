///////////////////////////////////////////////////////////////////////////////
// NB2CurrentSense.h
////////////////////////////////////////////////////////////////////////////////
//
//  IN PACKAGE:         Software Platform examples
//
//  COPYRIGHT:          Copyright (c) 2008, Altium
//
//  DESCRIPTION:        This example driver impliments a realtime Voltage and Current
//                      monitor for the NB2 development board.
//
////////////////////////////////////////////////////////////////////////////////

#ifndef __NB2_CURRENT_SENSE_H
#define __NB2_CURRENT_SENSE_H

#include <drv_max1229.h>

///////////////////////////////////////////////////////////////////////////////
#define NB2CS_CHAN_PBC3V3  0
#define NB2CS_CHAN_PBC1V8  1
#define NB2CS_CHAN_PBC2V5  2
#define NB2CS_CHAN_PBC1V2  3
#define NB2CS_CHAN_PBC5V0  4
#define NB2CS_CHAN_UserA   5
#define NB2CS_CHAN_UserB   6
#define NB2CS_CHAN_1V2     7
#define NB2CS_CHAN_1V8     8
#define NB2CS_CHAN_2V5     9
#define NB2CS_CHAN_3V3     10
#define NB2CS_CHAN_5V0     11
#define NB2CS_CHAN_PBA1V2  12
#define NB2CS_CHAN_PBA1V8  13
#define NB2CS_CHAN_PBA2V5  14
#define NB2CS_CHAN_PBA3V3  15
#define NB2CS_CHAN_PBA5V0  16
#define NB2CS_CHAN_PBB1V2  17
#define NB2CS_CHAN_PBB1V8  18
#define NB2CS_CHAN_PBB2V5  19
#define NB2CS_CHAN_PBB3V3  20
#define NB2CS_CHAN_PBB5V0  21
#define NB2CS_CHAN_DB5V0   22
#define NB2CS_CHAN_DB3V3   23

///////////////////////////////////////////////////////////////////////////////
typedef enum {
    eNB2Voltage_1V2,
    eNB2Voltage_1V8,
    eNB2Voltage_2V5,
    eNB2Voltage_3V3,
    eNB2Voltage_5V0,
    eNB2Voltage_USR
} NB2CS_VoltageRail_T;

///////////////////////////////////////////////////////////////////////////////
typedef enum {
    eNB2Voltage,
    eNB2Current
} NB2CS_MeasurementType_T;

////////////////////////////////////////////////////////////////////////////////
void        NB2CS_Init                      (max1229_t *drv0, max1229_t *drv1);
double      NB2CS_GetChannelRaw             (uint8_t chanIdx);
double      NB2CS_GetChannelValue           (uint8_t chanIdx);
uint8_t *   NB2CS_GetChannelName            (uint8_t chanIdx);
double      NB2CS_GetChannelRailVoltage     (uint8_t chanIdx);
double      NB2CS_GetBoardTemp              (void);
NB2CS_MeasurementType_T NB2CS_GetChannelType(uint8_t chanIdx);



#endif // __NB2_CURRENT_SENSE_H
