/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   max1037 ADC device driver
|*
\*****************************************************************************/

//..............................................................................
#include "adc_max1037.h"
#include "wb_i2c.h"
//..............................................................................

//..............................................................................
// Performs single conversion on selects channel and returns value
//..............................................................................
unsigned int max1037_read(unsigned int base)
{
  i2c_send_address(base,MAX1037_ADDRESS_READ);
  return  i2c_get_byte(base,1);
}
//..............................................................................

//..............................................................................
// Sets ADC up for single ended inputs and selects input channel [0..3]
//..............................................................................
void max1037_open( unsigned int base, unsigned char channel )
{
  static unsigned char config_byte;
  static unsigned char setup_byte;

  channel    = channel & 0x03;     // We only have four channels
  channel    = channel <<   1;     // shift into correct bit positions

  config_byte = MAX1037_DEFAULT_CONFIG;
  config_byte = config_byte | channel;

  setup_byte  = MAX1037_DEFAULT_SETUP;

  i2c_init     (base);
  i2c_send_byte(base,MAX1037_ADDRESS_WRITE,0);
  i2c_send_byte(base,setup_byte           ,0);
  i2c_send_byte(base,config_byte          ,1);
}
//..............................................................................

