/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   MAX1104 Codec device driver
|*
\*****************************************************************************/

#ifndef __CODEC_MAX1104_H__
#define __CODEC_MAX1104_H__

// Control Byte Format
// +------+-------+------------------------------------------------------------+
// | BIT# | NAME  |  DESCRIPTION                                               |
// +------+-------+------------------------------------------------------------+
// |7(MSB)| START |  1=New Control Word,                                       |
// |      |       |  0=Control word ignored                                    |
// +------+-------+------------------------------------------------------------+
// |  6   |  A1   |  1=DAC addressed Current byte configures DAC, then DAC Data|
// |      |       |  0=DAC not addressed                                       |
// +------+-------+------------------------------------------------------------+
// |  5   |  A0   |  1=ADC Current byte configuration, next byte returns Data  |
// |      |       |  0=ADC not addressed                                       |
// +------+-------+------------------------------------------------------------+
// |  4   |  C1   |  1=ADC input to Vdd/2   (not recommended for MAX1104)      |
// |      |       |  0=ADC input to AIN                                        |
// +------+-------+------------------------------------------------------------+
// |  3   |  C0   |  1=Continuous conversion until reconfigured                |
// |      |       |  0=Single conversion, controlword reqd. for next conversion|
// +------+-------+------------------------------------------------------------+
// |  2   |  E2   |  1=reference enabled    (don't care for MAX1104)           |
// |      |       |  0=reference disabled                                      |
// +------+-------+------------------------------------------------------------+
// |  1   |  E1   |  1=ADC enabled                                             |
// |      |       |  0=ADC disabled                                            |
// +------+-------+------------------------------------------------------------+
// |  0   |  E0   |  1=DAC enabled                                             |
// |      |       |  0=DAC disabled                                            |
// +------+-------+------------------------------------------------------------+

#define MAX1104_START 0x80   // Start bit (always set for command mode)
#define MAX1104_A1    0x40
#define MAX1104_A0    0x20
#define MAX1104_C1    0x10
#define MAX1104_C0    0x08
#define MAX1104_E2    0x04
#define MAX1104_E1    0x02
#define MAX1104_E0    0x01

//..............................................................................
// Opening the codec puts it into single or continuous mode. Use
// spi_send_receive after opening to send a byte to the dac and / or to
// receive a byte from the adc.
// In single mode only one byte can be sent. In continuous mode multiple
// bytes can be sent.
void max1104_open_single_adcdac  ( unsigned int base );
void max1104_open_single_adc     ( unsigned int base );
void max1104_open_single_dac     ( unsigned int base );
void max1104_open_cont_adcdac    ( unsigned int base );
void max1104_open_cont_adc       ( unsigned int base );
void max1104_open_cont_dac       ( unsigned int base );
void max1104_close               ( unsigned int base );

//..............................................................................
// Use the following to enable or disable the adc and dac. These functions
// are not needed to use the above functions for opening or sending / receiving
// data
void max1104_set_enabled         ( unsigned int base, unsigned int enable_adc, unsigned int enable_dac );

#endif

