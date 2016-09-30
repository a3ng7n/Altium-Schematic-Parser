/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   SPI Controller core device driver
|*
\*****************************************************************************/

#include "wb_spi.h"
#include "util_timing.h"

//..............................................................................
#define SPI_BASE(Base) ((volatile unsigned char*) Base)
#define SPI_DATA(Base) SPI_BASE(Base)[0]
#define SPI_CTRL(Base) SPI_BASE(Base)[1]
#define SPI_DIV(Base)  SPI_BASE(Base)[2]
//..............................................................................

//..............................................................................
// The core will require approx 20 SPI cycles to send/receive
// Defaulting to rate of 100KHz we need a clock freq of 2MHz
//..............................................................................
unsigned int  spi_divisor              ( void )
{
    return timing_get_clock_freq_hz() / (20 * 100000);
}
//..............................................................................

//..............................................................................
void spi_mode_lo(unsigned int  base)
{
    static unsigned char c;

    c = SPI_CTRL(base);
    c = c & (~SPI_CTRL_MODE);
    SPI_CTRL(base) = c;
}
//..............................................................................

//..............................................................................
void spi_mode_hi(unsigned int  base)
{
    static unsigned char c;

    c = SPI_CTRL(base);
    c = c | SPI_CTRL_MODE;
    SPI_CTRL(base) = c;
}
//..............................................................................

//..............................................................................
void spi_cs_lo(unsigned int  base)
{
     SPI_CTRL(base) = SPI_CTRL(base) & (~SPI_CTRL_CS);
}
//..............................................................................

//..............................................................................
void spi_cs_hi(unsigned int  base)
{
     SPI_CTRL(base) = SPI_CTRL(base) | SPI_CTRL_CS;
}
//..............................................................................

//..............................................................................
void spi_control_write(unsigned int  base,
                       unsigned char value)
{
     SPI_CTRL(base) = value;
}
//..............................................................................

//..............................................................................
unsigned char spi_control_read(unsigned int  base)
{
     return SPI_CTRL(base);
}
//..............................................................................

//..............................................................................
void spi_data_write(unsigned int  base,
                    unsigned char value)
{
     SPI_DATA(base) = value;
}
//..............................................................................

//..............................................................................
unsigned char spi_busy(unsigned int  base)
{
     return (SPI_CTRL(base) >> 7) & 0x01;
}
//..............................................................................

//..............................................................................
void spi_wait_while_busy(unsigned int  base)
{
    while (spi_busy(base));
}
//..............................................................................

//..............................................................................
unsigned char spi_data_read(unsigned int  base)
{
     return SPI_DATA(base);
}
//..............................................................................

//..............................................................................
void spi_divider_write(unsigned int  base,
                       unsigned char value)
{
     SPI_DIV(base) = value;
}
//..............................................................................

//..............................................................................
unsigned char spi_divider_read(unsigned int  base)
{
     return SPI_DIV(base);
}
//..............................................................................

//..............................................................................
unsigned char spi_send_receive_last(unsigned int base, unsigned char in)
{
    spi_wait_while_busy  (base   );
    spi_data_write       (base,in);
    return spi_data_read (base   );
}
//..............................................................................

//..............................................................................
unsigned char spi_send_wait_receive(unsigned int base, unsigned char in)
{
    spi_wait_while_busy (base   );
    spi_data_write      (base,in);
    spi_wait_while_busy (base   );
    return spi_data_read(base   );
}
//..............................................................................

//..............................................................................
void spi_wait_and_send(unsigned int base, unsigned char in)
{
    spi_wait_while_busy(base   );
    spi_data_write     (base,in);
}
//..............................................................................

//..............................................................................
void spi_open(unsigned int base)
{
  static unsigned char c;

  spi_control_write(base,SPI_CTRL_ENABLE | SPI_CTRL_CS);   //Toggle CS high then low

  c = spi_control_read(base);

  timing_delay_us(20);
  spi_control_write(base,SPI_CTRL_ENABLE);
  timing_delay_us(20);
  spi_divider_write(base,spi_divisor());
  timing_delay_us(20);
}
//..............................................................................

//..............................................................................
void spi_close(unsigned int base)
{
  spi_control_write(base,SPI_CTRL_DISABLE);
}
//..............................................................................


