//-------------------------------------------------------------------------
// (c) 2003 Altium
// Started: 24.12.2003 Ch.Weimann
// PCF8583 RealTime Clock Interface for
// the Altium Nanoboard Tester
//
// These functions are used to access the Philips PCF8583 I2C RTC on the tester
// add-on board that plugs into the I2C/DAC/ADC connector
// It is used to verify the correct crystal frequency on the NanoBoard as
// well as the integrity of the external I2C bus
//-------------------------------------------------------------------------

#ifndef NBT_RTC_H
#define NBT_RTC_H

#define RTC_ADR (0xA0)  // I2C slave address for PCF8583 with A0 Pin tied low
//#define RTC_ADR (0xA2)  // I2C slave address for PCF8583 with A0 Pin tied high

#define RTC_CTRL  (0)  // offset for control/status register
#define RTC_HSEC  (1)  // offset for hundeth of seconds register
#define RTC_SEC   (2)  // offset for seconds register
#define RTC_MIN   (3)  // offset for minutes register
#define RTC_HOUR  (4)  // offset for hours register
#define RTC_YEAR  (5)  // offset for year/date register
#define RTC_DAY   (6)  // offset for weekday/month register
#define RTC_TIMER (7)  // offset for timer register
#define RTC_ACTRL (8)  // offset for alarm control register

//  Control/Status Register
//
//   MSB                         LSB
//  +---+---+---+---+---+---+---+---+
//  | 7 | 6 | 5   4 | 3 | 2 | 1 | 0 |   Reset State: 0x00
//  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
//    |   |   +-+-+   |   |   |   +---- Timer Flag
//    |   |     |     |   |   |
//    |   |     |     |   |   +-------- Alarm Flag
//    |   |     |     |   |
//    |   |     |     |   +------------ Alarm Enable Bit
//    |   |     |     |
//    |   |     |     +---------------- Mask Flag
//    |   |     |
//    |   |     +---------------------- Function Mode: 00 = Clock Mode 32.768kHz
//    |   |                                            01 = Clock Mode 50Hz
//    |   |                                            10 = Event-Counter Mode
//    |   |                                            11 = Test Modes
//    |   |
//    |   +---------------------------- Hold Last Count Flag
//    |
//    +-------------------------------- Stop Counting Flag

// --------------------------------------------
// sets register in PCF8583
// returns: 0 for success
//          non-0 for ack-error
// --------------------------------------------
unsigned char RTC_WriteRegister(unsigned char RegisterOffset, unsigned char Rdata);

// --------------------------------------------
// reads register in PCF8583
// --------------------------------------------
unsigned char RTC_ReadRegister(unsigned char RegisterOffset);

//------------------------------------------------------------
// set time in hundredths of seconds since midnight
// returns 0 on success, 1 on no ACK
//------------------------------------------------------------
unsigned char RTC_SetTimeHundredths(unsigned long Hundredths);

//-------------------------------------------------------------
// get time in hundredths of seconds since midnight
//-------------------------------------------------------------
unsigned long RTC_GetTimeHundredths(void);


#endif

