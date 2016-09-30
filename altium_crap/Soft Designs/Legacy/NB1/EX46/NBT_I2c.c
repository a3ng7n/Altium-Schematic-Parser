//-----------------------------------------------------------
//         _   _  ____  _____     ___  ____    ____     ____
//        | \ | || __ )|_   _|   |_ _||___ \  / ___|   / ___|
//        |  \| ||  _ \  | |      | |   __) || |      | |
//        | |\  || |_) | | |      | |  / __/ | |___  _| |___
//        |_| \_||____/  |_|_____|___||_____| \____|(_)\____|
//                         |_____|
//  I2C.C
//  I2C Routines
//
//  (c) 2001 Ch.Weimann
//-----------------------------------------------------------

#include "nbt_i2c.h"
#include "hware.h"
#include "nbt_lcd.h"        // for delay

void I2C_Init(void)
//-----------------------------------------------------------
// initialises the lines to the idle state:
// SDA and SCL High impedance
{
#ifndef __TURBOC__
  SDA=0;
  SCL=0;
  SDA_CTRL=CTRL_HIZ;
  SCL_CTRL=CTRL_HIZ;
  I2C_Stop();
#endif
}

void I2C_Start(void)
//-----------------------------------------------------------
// creates start condition and leave SCL Low on exit
{
#ifndef __TURBOC__
  SDA_H;
  I2C_DELAY;
  SCL_H;
  I2C_DELAY;
    SDA_L;
    I2C_DELAY;
    SCL_L;
    I2C_DELAY;
#endif
}

void I2C_Stop(void)
//---------------------------------------------------------------
// assumes SCL is low and SDA is HIZ on entry
// creates stop condition and will leave SDA AND SCL HIgh on exit
{
#ifndef __TURBOC__
    SDA_L;   // pull data line low
    I2C_DELAY;
    SCL_H;   // make clock line high
    I2C_DELAY;
    SDA_H;   // create stop condition
    I2C_DELAY;
#endif
}

unsigned char I2C_TxByte(unsigned char c)
//-----------------------------------------------------------
// Assumes SCL=L on entry
// SCL will be L on exit
// returns Acknowledge
{
#ifndef __TURBOC__
  register unsigned char i;
  for(i=0;i<8;i++)
  {
//      SCL_L;            // Bit transfer: Set SCL Low
//      I2C_DELAY;
        if(c&0x80)        // send MSb 1st
          SDA_H;
        else
          SDA_L;
        c<<=1;            // prepare next bit
        I2C_DELAY;
        SCL_H;            // generate clock pulse
        I2C_DELAY;
        I2C_DELAY;        // make clock high time as long as low time
    SCL_L;            // bit is out the door
    I2C_DELAY;
  } 
    SDA_H;              // release data line to check for ack
    I2C_DELAY;
    SCL_H;              // raise clock
    I2C_DELAY;
    I2C_DELAY;
    i = SDA;
//    i=(0!=SDA)?1:0;          // check Ack flag
    SCL_L;              // end of byte transfer
    I2C_DELAY;
  return(i);          // return Ack
#endif
}


unsigned char I2C_RxByte(unsigned int SendAck)
//------------------------------------------------------------
// Assumes SCL=L on entry
// SCL will be L on exit
// if 'SendAck' is non-0 Master will generate Ack
// returns Byte that was read
{
#ifndef __TURBOC__
    register unsigned char c,i;
    c=0;
    SDA_H;          // make sure SDA is high impedance during read
    for(i=0;i<8;i++)
    {
    I2C_DELAY;
    SCL_H;          // set Clock high
    I2C_DELAY;
    I2C_DELAY;
    c<<=1;
    if(SDA!=0)      // sample at the end of Clock high pulse & shift bit at LSb
      c|=1;
    SCL_L;  
    I2C_DELAY;
    }
    if(SendAck)       // set up data line for ack or nack
      SDA_L;
    else
      SDA_H;
    I2C_DELAY;
    SCL_H;            // generate clock pulse for Ack
    I2C_DELAY;
    I2C_DELAY;
  SCL_L;
  I2C_DELAY;
  SDA_H;              // release SDA after ack
  I2C_DELAY; 
    return(c);
#endif
}

unsigned char I2C_WriteByte(unsigned char DevAddr, unsigned char TwoByteAddress, unsigned int Addr, unsigned char Data)
//----------------------------------------------------------------------------------------------------------------------
// Addresses Device 'DevAddr', and writes 'Data' to device internal address 'Addr'
// if 'TwoByteAddress' !=0 two adress bytes are sent
// Returns 'ACK' if success, 'NACK' if not
{
#ifndef __TURBOC__
  unsigned char retval;  // return value
  DevAddr&=(~0x01);      // clear read flag
    I2C_Start();           // create start condition
  retval=I2C_TxByte(DevAddr);   // send device address
  if(NACK==retval)
      goto ABORT;
    if(TwoByteAddress)            // 16 bit device internal address?
    {  
    retval=I2C_TxByte(Addr>>8); // send address MSB
    if(NACK==retval)
      goto ABORT;
  }   
    retval=I2C_TxByte(Addr&0xFF); // send address LSB
  if(NACK==retval)
      goto ABORT;
    retval=I2C_TxByte(Data);      // send data
  ABORT:
  I2C_Stop();
    return(retval);
#endif
}

unsigned char I2C_ReadByte(unsigned char DevAddr, unsigned char TwoByteAddress, unsigned int Addr)
//------------------------------------------------------------------------------------------------
// returns byte at offset 'Addr' in device 'DevAddr'
// if 'TwoByteAddress' !=0 two adress bytes are sent
{
#ifndef __TURBOC__
  unsigned char retval;
    I2C_Start();            // create start condition
  DevAddr&=(~0x01);       // clear read flag to write address
  retval=I2C_TxByte(DevAddr);    // send device address
  if(TwoByteAddress)
  {
    retval=I2C_TxByte(Addr>>8);  // 16 Bit internal address?
  }
  retval=I2C_TxByte(Addr&0xFF);
    I2C_Start();            // create start condition
  DevAddr|=(0x01);        // set read flag
  retval=I2C_TxByte(DevAddr);    // send device address
  retval=I2C_RxByte(0);
  I2C_Stop();            
  return(retval);
#endif
}


