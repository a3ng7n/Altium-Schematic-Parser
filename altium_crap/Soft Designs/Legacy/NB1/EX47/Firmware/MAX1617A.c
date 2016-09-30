#include "max1617a.h"
#include "viimt_i2c.h"

//----------------------------------------------------------------
// read any command register in the MAX1617 and return its value
//----------------------------------------------------------------
unsigned char Max1617_ReadRegister(unsigned char command)
{
    register unsigned char retval;
    retval = I2C_ReadByte(MAX1617_I2C_BASE,0,command);
    return retval;
}

//--------------------------------------------------------------------------
// write 'data' to any command register in the MAX1617
//--------------------------------------------------------------------------
void Max1617_WriteRegister(unsigned char command, unsigned char data)
{
    I2C_WriteByte(MAX1617_I2C_BASE,0,command,data);
}

//---------------------------------------------------------------------------
// Command Byte: Sends Command with no data, usually for one-shot command
// returns: 'ACK' iff success, 'NACK' if not
//---------------------------------------------------------------------------
unsigned char Max1617_SendByte(unsigned char command)
{
  unsigned char retval;                                    // return value
  I2C_Start();                                             // create start condition
  retval=I2C_TxByte(MAX1617_I2C_BASE);                     // send device address
  if(NACK==retval)
      goto ABORT;
  retval=I2C_TxByte(command);                              // send command Byte
  if(NACK==retval)
      goto ABORT;
  ABORT:
  I2C_Stop();                                              // create stop condition
    return(retval);
}

//-----------------------------------------------------
// read temperature from MAX1617
// SensorType = SENSOR_MAX1617 returns local temperature
//              otherwise FPGA temperature
//-----------------------------------------------------
signed char Max1617_GetTemperature(SensorType Sensor)
{
    register unsigned char retval;
    register unsigned char command;
    command = (Sensor==SENSOR_MAX1617) ? RLTS : RRTE;      // determine whether to read Max1717 temperature or FPGA Temperature
    retval = I2C_ReadByte(MAX1617_I2C_BASE,0,command);
    return retval;
}


// -------------------------------------------------------------
// converts Max1617 Temperatures to a string s eg +102
// required are at least 5 bytes for temperature plus trailing 0
// -------------------------------------------------------------
void Max1617_Temperature2String(unsigned char Temperature, char *s)
{
   register unsigned char c;
   register unsigned char Leading0 = 1;
   if(Temperature & 0x80)                                  // negative?
   {
     *s++ = '-';                                           // two's complement
     Temperature = ~Temperature;                           // invert
     Temperature ++;                                       // add one
   }
   else
   {
     *s++ = '+';
   }
   c = Temperature / 100;
   if(c!=0)
   {
     Leading0 = 0;
     *s++ = c+'0';
   }
   Temperature %= 100;
   c = Temperature / 10;
   Temperature %= 10;
   if(c!=0)
   {
     Leading0 = 0;
     *s++ = c+'0';
   }
   else
   if (! Leading0) *s++='0';
   *s++ = Temperature+'0';
   *s++=0;                                                 // terminate string
}
