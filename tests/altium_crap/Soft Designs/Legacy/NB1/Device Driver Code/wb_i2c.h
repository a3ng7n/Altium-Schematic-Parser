/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   I2C Controller core device driver
|*
\*****************************************************************************/

#ifndef __I2C_WB_H__
#define __I2C_WB_H__

//..............................................................................
#define ACK  (0)
#define NACK (1)
//..............................................................................

//..............................................................................
#define I2C_TIMEOUT_TICKS 50000
//..............................................................................

//..............................................................................
void          i2c_init                    ( unsigned int base);
unsigned char i2c_busy                    ( unsigned int base);
unsigned char i2c_wait_while_busy_timeout ( unsigned int base, unsigned int timeout_ticks);
void          i2c_wait_while_busy         ( unsigned int base);
void          i2c_send_stop               ( unsigned int base);
void          i2c_send_start              ( unsigned int base);
//..............................................................................

//..............................................................................
//   Send data byte to i2c slave. If last is set then i2c master will
//   automatically generate STOP condition after sending the data.
//..............................................................................
void i2c_send_byte   ( unsigned int  base, unsigned char data, unsigned char last);
void i2c_send_address( unsigned int  base, unsigned char address );

//..............................................................................

//..............................................................................
//   Get data byte from i2c slave. If last is set then i2c master will
//   automatically generate two STOP conditions on i2c bus after completing
//   data transfer from a slave device.
//   First is not acknowledge and then classic STOP condition.
//..............................................................................
unsigned char i2c_get_byte(unsigned int base, int last );
//..............................................................................

#endif // __I2C_WB_H__

