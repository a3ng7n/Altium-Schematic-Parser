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

#include "nbt_rtc.h"
#include "nbt_I2c.h"

//---------------------------------------------------
// converts binary to BCD
//---------------------------------------------------
unsigned int BinByte2BCD(unsigned char Bin)
{
  unsigned int retval = 0;
  do
  {
    retval += Bin % 10;
    Bin /=10;
  }
  while (0 != Bin);
  return(retval);
}

//-----------------------------------------------------
// converts BCD to Binary
//-----------------------------------------------------
unsigned int BCD2Bin(unsigned int BCD)
{
  unsigned int retval = 0;
  unsigned int temp;
  unsigned char i;
  for(i=0;i<4;i++)
  {
    retval *= 10;
    temp = BCD & 0xF000;
    temp >>=12;
    retval += temp;
    BCD <<= 4;
  }
  return(retval);
}

// --------------------------------------------
// sets register in PCF8583
// returns: 0 for success
//          non-0 for ack-error
// --------------------------------------------
unsigned char RTC_WriteRegister(unsigned char RegisterOffset, unsigned char Rdata)
{
  return I2C_WriteByte(RTC_ADR, 0, RegisterOffset, Rdata);

}

// --------------------------------------------
// reads register in PCF8583
// --------------------------------------------
unsigned char RTC_ReadRegister(unsigned char RegisterOffset)
{
  return I2C_ReadByte(RTC_ADR | 0x01,0,RegisterOffset);
}

// --------------------------------------------
// sets current time in 24hour format
// returns: 0 for success
//          non-0 for ack-error
// --------------------------------------------
unsigned char RTC_SetTime(unsigned char Hour, unsigned char Minute,
                          unsigned char Second, unsigned char Hundredths)
{
  unsigned char retval = 0;  // return value
  retval |= RTC_WriteRegister(RTC_CTRL,0x80);  // STOP RTC
  retval |= RTC_WriteRegister(RTC_HOUR,BinByte2BCD(Hour));
  retval |= RTC_WriteRegister(RTC_MIN,BinByte2BCD(Minute));
  retval |= RTC_WriteRegister(RTC_SEC,BinByte2BCD(Second));
  retval |= RTC_WriteRegister(RTC_HSEC,BinByte2BCD(Hundredths));
  retval |= RTC_WriteRegister(RTC_CTRL,0x00);  // START RTC again
  return retval;
}


//------------------------------------------------------------
// set time in hundredths of seconds since midnight
//------------------------------------------------------------
unsigned char RTC_SetTimeHundredths(unsigned long Hundredths)
{
  unsigned char hs, s, m, h;
  hs = Hundredths % 100;
  Hundredths /= 100;
  s = Hundredths % 60;
  Hundredths /= 60;
  m = Hundredths % 60;
  Hundredths /= 60;
  h = Hundredths % 24;
  return RTC_SetTime(h,m,s,hs);
}

//-------------------------------------------------------------
// get time in hundredths of seconds since midnight
//-------------------------------------------------------------
unsigned long RTC_GetTimeHundredths(void)
{
  unsigned long retval = 0;
  unsigned int temp;
  RTC_WriteRegister(RTC_CTRL,0x40);  // Hold Last Count
  temp = RTC_ReadRegister(RTC_HOUR);
  temp &= 0x3F;
  temp = BCD2Bin(temp);
  retval = (unsigned long) temp;
  temp = RTC_ReadRegister(RTC_MIN);
  temp = BCD2Bin(temp);
  retval *= 60;
  retval += temp;
  temp = RTC_ReadRegister(RTC_SEC);
  temp = BCD2Bin(temp);
  retval *= 60;
  retval += temp;
  temp = RTC_ReadRegister(RTC_HSEC);
  temp = BCD2Bin(temp);
  retval *= 100;
  retval += temp;
  RTC_WriteRegister(RTC_CTRL,0x00);  // Resume normal operation
  return retval;
}
