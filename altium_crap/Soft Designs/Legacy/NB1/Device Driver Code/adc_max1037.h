/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   MAX1037 ADC device driver
|*
\*****************************************************************************/

#ifndef __ADC_MAX1037_H__
#define __ADC_MAX1037_H__

//..............................................................................
#define  MAX1037_ADDRESS_WRITE        0xC8 //0b1100_1000
#define  MAX1037_ADDRESS_READ         0xC9 //0b1100_1001
//..............................................................................

//..............................................................................
//MAX1037 Setup Register Format
//-----------------------------
//B7 = RegisterBit 1 => Setup Byte     0 => Configuration Byte
//B6 = Sel2        |
//B5 = Sel1        | Ref voltage setup
//B4 = Sel0        |
//B3 = CLK Bit     1 => External Clock 0 => Internal Clock
//B2 = BIP/UNI     1 => Bipolar        0 => Unipolar
//B1 = Reset       1 => No Action      0 => Reset configuration register to default
//B0 = X           Don't Care
//..............................................................................

//..............................................................................
#define  MAX1037_SETUP                0x80 //0b1000_0000
#define  MAX1037_CLOCK_EXTERNAL       0x08 //0b0000_1000
#define  MAX1037_CLOCK_INTERNAL       0x00 //0b0000_0000
#define  MAX1037_MODE_BIPOLAR         0x04 //0b0000_0100
#define  MAX1037_MODE_UNIPOLAR        0x00 //0b0000_0000
#define  MAX1037_NO_RESET             0x02 //0b0000_0010
#define  MAX1037_FORCE_RESET_CONFIG   0x00 //0b0000_0000
//..............................................................................

//...................................................................................................................
//                                                            Ref Volts      AIN/REF       Internal Reference State
//...................................................................................................................
#define  MAX1037_VREF_VDD             0x00 //0b0000_0000   // VDD          Analog Input         Always Off
#define  MAX1037_VREF_EXTERNAL        0x20 //0b0010_0000   // External     Reference Input      Always Off
#define  MAX1037_VREF_AUTOSHUTDOWN    0x40 //0b0100_0000   // Internal     Analog Input         Auto Shutdown
#define  MAX1037_VREF_ALWAYSON        0x50 //0b0101_0000   // Internal     Analog Input         Always On
#define  MAX1037_VREF_GENERATE        0x70 //0b0111_0000   // Internal     Reference Output     Always On
//..............................................................................

//..............................................................................
//MAX1037 Config Register Format
//..............................................................................
//B7 = RegisterBit 1 => Setup Byte    0 => Configuration Byte
//B6 = Scan1       | Scan Select Bits
//B5 = Scan0       |
//B4 = CS3         | Channel select bits - only CS0 and CS1 are used on the 4-Channel MAX1037
//B3 = CS2         |
//B2 = CS1         |
//B1 = CS0         |
//B0 = SGL/DIF     1 => Single-Ended  0 => Pseudo-Differential
//..............................................................................

//..............................................................................
#define  MAX1037_CONFIG               0x60 //0b0110_0000
#define  MAX1037_CHANNEL0             0x00 //0b0000_0000
#define  MAX1037_CHANNEL1             0x02 //0b0000_0010
#define  MAX1037_CHANNEL2             0x04 //0b0000_0100
#define  MAX1037_CHANNEL3             0x06 //0b0000_0110

#define  MAX1037_MODE_SINGLE_ENDED    0x01 //0b0000_0001
#define  MAX1037_MODE_DIFFERENTIAL    0x00 //0b0000_0000
//..............................................................................

//..............................................................................
#define  MAX1037_DEFAULT_SETUP        MAX1037_SETUP  | MAX1037_CLOCK_INTERNAL | MAX1037_MODE_UNIPOLAR | MAX1037_VREF_GENERATE | MAX1037_NO_RESET
#define  MAX1037_DEFAULT_CONFIG       MAX1037_CONFIG | MAX1037_MODE_SINGLE_ENDED
//..............................................................................

//..............................................................................
#define  MAX1037_CONFIG_CHANNEL0      MAX1037_DEFAULT_CONFIG | MAX1037_CHANNEL0
#define  MAX1037_CONFIG_CHANNEL1      MAX1037_DEFAULT_CONFIG | MAX1037_CHANNEL1
#define  MAX1037_CONFIG_CHANNEL2      MAX1037_DEFAULT_CONFIG | MAX1037_CHANNEL2
#define  MAX1037_CONFIG_CHANNEL3      MAX1037_DEFAULT_CONFIG | MAX1037_CHANNEL3
//..............................................................................

//..............................................................................
unsigned int  max1037_read( unsigned int base );
void          max1037_open( unsigned int base, unsigned char channel );
//..............................................................................

#endif
