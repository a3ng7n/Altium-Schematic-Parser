/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   I2C Controller core device driver
|*
\*****************************************************************************/

#include "wb_i2c.h"
#include "util_timing.h"

//..............................................................................
#define I2C_BASE(Base) ((volatile unsigned char*) Base)
#define I2C_CTRL(Base) I2C_BASE(Base)[0]
#define I2C_STAT(Base) I2C_BASE(Base)[1]
#define I2C_CLK0(Base) I2C_BASE(Base)[2]
#define I2C_CLK1(Base) I2C_BASE(Base)[3]
#define I2C_WRIT(Base) I2C_BASE(Base)[4]
#define I2C_READ(Base) I2C_BASE(Base)[5]
//..............................................................................

//..............................................................................
// Control register bits
#define I2C_CTRL_EN     0x01
#define I2C_CTRL_IEN    0x02
#define I2C_CTRL_IACK   0x04
#define I2C_CTRL_WR     0x08
#define I2C_CTRL_RD     0x10
#define I2C_CTRL_STOP   0x20
#define I2C_CTRL_START  0x40
#define I2C_CTRL_NACK   0x80
//..............................................................................

//..............................................................................
// Status register bits
#define I2C_STAT_INT    0x01
#define I2C_STAT_RXACK  0x02
#define I2C_STAT_BUSY   0x04
//..............................................................................

//..............................................................................
void i2c_send_stop(unsigned int base)
{
   unsigned char command = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_STOP;
   I2C_CTRL(base) = command;
}
//..............................................................................

//..............................................................................
void i2c_send_start(unsigned int base)
{
   unsigned char command = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_START;
   I2C_CTRL(base) = command ;
}
//..............................................................................

//..............................................................................
void wait_for_interrupt_set(unsigned int base)
{
    while ((I2C_STAT(base) & I2C_STAT_INT) == 0);
}
//..............................................................................

//..............................................................................
void wait_for_interrupt_clear(unsigned int base)
{
    while ((I2C_STAT(base) & I2C_STAT_INT) != 0);
}
//..............................................................................

//..............................................................................
void clear_interrupt(unsigned int base)
{
    I2C_CTRL(base) = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_IACK;
}
//..............................................................................

//..............................................................................
unsigned char get_ack(unsigned int base)
{
    static unsigned char ack;
    ack = I2C_STAT(base);
    ack = (ack & I2C_STAT_RXACK) >> 1;
    return ack;
}
//..............................................................................

//..............................................................................
unsigned char wait(unsigned int base)
{
    static unsigned char ack;
    wait_for_interrupt_set  (base);
    ack = get_ack           (base);
    clear_interrupt         (base);
    wait_for_interrupt_clear(base);
    return ack;
}
//..............................................................................

#define MAX_TRIES  100
//..............................................................................
void i2c_send_byte(unsigned int  base,
                   unsigned char data,
                   unsigned char last)
{
   unsigned char command;
   static unsigned char TryCount;

   TryCount = 0;

   if (last)
      command = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_WR | I2C_CTRL_STOP;
   else
      command = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_WR;

   do
   {
      I2C_WRIT(base) = data;
      I2C_CTRL(base) = command;
      TryCount++;
   } while ( (wait(base) == 0) && (TryCount < MAX_TRIES));
}
//..............................................................................

//..............................................................................
void i2c_send_address(unsigned int  base,
                      unsigned char address)
{
   unsigned char command = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_WR | I2C_CTRL_START;
   static unsigned char TryCount;
   TryCount = 0;
   do
   {
      I2C_WRIT(base) = address;
      I2C_CTRL(base) = command;
      TryCount++;
   } while ( (wait(base) == 0) && (TryCount < MAX_TRIES));
}
//..............................................................................

//..............................................................................
unsigned char i2c_get_byte(unsigned int base,
                                    int last )
{
   unsigned char command ;

   if (last)
   {
      command = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_RD | I2C_CTRL_NACK | I2C_CTRL_STOP ;
   }
   else
   {
      command = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_RD | I2C_CTRL_NACK;
   }

   I2C_CTRL(base) = command;
   wait(base);

   return I2C_READ(base) ;
}
//..............................................................................

//..............................................................................
void i2c_init(unsigned int base)
{
   static int divider;
   unsigned char div0;
   unsigned char div1;

   divider = (25000000 / (5 * 100000)) - 1;

   div0 = (divider     ) & 0xff;
   div1 = (divider >> 8) & 0xff;

   I2C_CTRL(base) = 0;
   I2C_WRIT(base) = 0;
   I2C_CLK0(base) = div0;
   I2C_CLK1(base) = div1;
}
//..............................................................................


