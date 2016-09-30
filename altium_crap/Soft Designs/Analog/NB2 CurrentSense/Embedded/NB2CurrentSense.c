///////////////////////////////////////////////////////////////////////////////
// NB2CurrentSense.c
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

#include <assert.h>

#include "NB2CurrentSense.h"

#define NB2CS_ADC_VREF              2.5
#define NB2CS_VoltageScale_2V5_3V3  2
#define NB2CS_5V0_Res1              10000
#define NB2CS_5V0_Res2              2700
#define NB2CS_SenseResistance       0.047
#define NB2CS_MAX4372TGain          20
#define NB2CS_TemperatureScale      0.125

///////////////////////////////////////////////////////////////////////////////
typedef struct NB2CS_Channel_T {
    uint8_t                 drvIdx;
    uint8_t                 drvChan;
    NB2CS_MeasurementType_T mType;
    uint8_t                 name[16];
    NB2CS_VoltageRail_T     rail;
}NB2CS_Channel_T;

///////////////////////////////////////////////////////////////////////////////
NB2CS_Channel_T NB2CS_ChannelMap[] = {
    { 0,  0, eNB2Current, "PBC3V3", eNB2Voltage_3V3 },
    { 0,  1, eNB2Current, "PBC1V8", eNB2Voltage_1V8 },
    { 0,  2, eNB2Current, "PBC2V5", eNB2Voltage_2V5 },
    { 0,  3, eNB2Current, "PBC1V2", eNB2Voltage_1V2 },
    { 0,  4, eNB2Current, "PBC5V0", eNB2Voltage_5V0 },
    { 0,  5, eNB2Current, "UserA",  eNB2Voltage_USR },
    { 0,  6, eNB2Current, "UserB",  eNB2Voltage_USR },
    { 0,  7, eNB2Voltage, "1V2",    eNB2Voltage_1V2 },
    { 0,  8, eNB2Voltage, "1V8",    eNB2Voltage_1V8 },
    { 0,  9, eNB2Voltage, "2V5",    eNB2Voltage_2V5 },
    { 0, 10, eNB2Voltage, "3V3",    eNB2Voltage_3V3 },
    { 0, 11, eNB2Voltage, "5V0",    eNB2Voltage_5V0 },
    { 1,  0, eNB2Current, "PBA1V2", eNB2Voltage_1V2 },
    { 1,  1, eNB2Current, "PBA1V8", eNB2Voltage_1V8 },
    { 1,  2, eNB2Current, "PBA2V5", eNB2Voltage_2V5 },
    { 1,  3, eNB2Current, "PBA3V3", eNB2Voltage_3V3 },
    { 1,  4, eNB2Current, "PBA5V0", eNB2Voltage_5V0 },
    { 1,  5, eNB2Current, "PBB1V2", eNB2Voltage_1V2 },
    { 1,  6, eNB2Current, "PBB1V8", eNB2Voltage_1V8 },
    { 1,  7, eNB2Current, "PBB2V5", eNB2Voltage_2V5 },
    { 1,  8, eNB2Current, "PBB3V3", eNB2Voltage_3V3 },
    { 1,  9, eNB2Current, "PBB5V0", eNB2Voltage_5V0 },
    { 1, 10, eNB2Current, "DB5V0",  eNB2Voltage_5V0 },
    { 1, 11, eNB2Current, "DB3V3",  eNB2Voltage_3V3 }
};

#define NB2CS_CHANNEL_COUNT (sizeof(NB2CS_ChannelMap)/sizeof(NB2CS_Channel_T))

///////////////////////////////////////////////////////////////////////////////
max1229_t *MAX1229Drivers[2] = {0, 0};

///////////////////////////////////////////////////////////////////////////////
void NB2CS_Init(max1229_t *drv0, max1229_t *drv1)
{
    assert(drv0 && drv1);

    MAX1229Drivers[0] = drv0;
    MAX1229Drivers[1] = drv1;
}

///////////////////////////////////////////////////////////////////////////////
double NB2CS_GetChannelRaw(uint8_t chanIdx)
{
    double voltage = 0;

    assert(MAX1229Drivers[0] && MAX1229Drivers[1]);
    assert(chanIdx < NB2CS_CHANNEL_COUNT);

    voltage = ((double)max1229_ReadChannel(MAX1229Drivers[NB2CS_ChannelMap[chanIdx].drvIdx], NB2CS_ChannelMap[chanIdx].drvChan)) * (NB2CS_ADC_VREF / 4096);

    switch(NB2CS_ChannelMap[chanIdx].rail){
        case eNB2Voltage_2V5:
        case eNB2Voltage_3V3:
            voltage *= NB2CS_VoltageScale_2V5_3V3;
            break;
        case eNB2Voltage_5V0:
            voltage *= ((double)NB2CS_5V0_Res1 / (double)NB2CS_5V0_Res2 + 1);
            break;
        case eNB2Voltage_1V2:
        case eNB2Voltage_1V8:
        case eNB2Voltage_USR:
            if(NB2CS_ChannelMap[chanIdx].mType == eNB2Current){
                voltage *= NB2CS_MAX4372TGain;
            }
            break;
        default:
            break;
    }

    return voltage;
}

///////////////////////////////////////////////////////////////////////////////
double NB2CS_GetChannelValue(uint8_t chanIdx)
{
    double value = NB2CS_GetChannelRaw(chanIdx);

    if(NB2CS_ChannelMap[chanIdx].mType == eNB2Current){
        value *= NB2CS_SenseResistance;
    }

    return value;
}

///////////////////////////////////////////////////////////////////////////////
uint8_t *NB2CS_GetChannelName(uint8_t chanIdx)
{
    assert(chanIdx < NB2CS_CHANNEL_COUNT);
    return NB2CS_ChannelMap[chanIdx].name;
}

///////////////////////////////////////////////////////////////////////////////
NB2CS_MeasurementType_T NB2CS_GetChannelType(uint8_t chanIdx)
{
    assert(chanIdx < NB2CS_CHANNEL_COUNT);
    return NB2CS_ChannelMap[chanIdx].mType;
}

///////////////////////////////////////////////////////////////////////////////
double NB2CS_GetChannelRailVoltage(uint8_t chanIdx)
{
    assert(chanIdx < NB2CS_CHANNEL_COUNT);

    switch(NB2CS_ChannelMap[chanIdx].rail){
        case eNB2Voltage_1V2:
            return NB2CS_GetChannelValue(NB2CS_CHAN_1V2);
        case eNB2Voltage_1V8:
            return NB2CS_GetChannelValue(NB2CS_CHAN_1V8);
        case eNB2Voltage_2V5:
            return NB2CS_GetChannelValue(NB2CS_CHAN_2V5);
        case eNB2Voltage_3V3:
            return NB2CS_GetChannelValue(NB2CS_CHAN_3V3);
        case eNB2Voltage_5V0:
            return NB2CS_GetChannelValue(NB2CS_CHAN_5V0);
        case eNB2Voltage_USR:
        default:
            return 0;
    }
}

///////////////////////////////////////////////////////////////////////////////
double NB2CS_GetBoardTemp(void)
{
    assert(MAX1229Drivers[0] && MAX1229Drivers[1]);
    return ((((double)max1229_ReadTemperature(MAX1229Drivers[0])) + ((double)max1229_ReadTemperature(MAX1229Drivers[1])))/2) / 8;
}
