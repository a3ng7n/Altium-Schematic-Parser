/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   SPI Controller core device driver
|*
\*****************************************************************************/

#ifndef __WB_SPI_H__
#define __WB_SPI_H__

#define SPI_CTRL_ENABLE  0x00
#define SPI_CTRL_DISABLE 0x01

#define SPI_CTRL_CS      0x02
#define SPI_CTRL_MODE    0x04

unsigned int  spi_divisor              ( void );

void          spi_mode_lo              ( unsigned int base );
void          spi_mode_hi              ( unsigned int base );
void          spi_cs_lo                ( unsigned int base );
void          spi_cs_hi                ( unsigned int base );

void          spi_control_write        ( unsigned int base, unsigned char value );
unsigned char spi_control_read         ( unsigned int base );
void          spi_data_write           ( unsigned int base, unsigned char value );
unsigned char spi_data_read            ( unsigned int base );
void          spi_divider_write        ( unsigned int base, unsigned char value );
unsigned char spi_divider_read         ( unsigned int base );
unsigned char spi_busy                 ( unsigned int base );
void          spi_wait_while_busy      ( unsigned int base );

void          spi_open                 ( unsigned int base );
void          spi_close                ( unsigned int base );

//..............................................................................
// spi_wait_and_send will:
//      wait until the device is not busy
//      send 'out' to the spi core, which will initiate the spi send
//      return immediately
void          spi_wait_and_send        ( unsigned int base, unsigned char out );

//..............................................................................
// spi_send_receive_last will:
//      wait until the device is not busy
//      send 'out' to the spi core, which will initiate the spi send
//      read the incoming byte from the *last* spi send
//      return immediately
unsigned char spi_send_receive_last    ( unsigned int base, unsigned char out );

//..............................................................................
// spi_send_wait_receive will:
//      wait until the device is not busy
//      send 'out' to the spi core, which will initiate the spi send
//      wait for the spi core to complete the spi send
//      read the incoming byte from this spi send
//      return
unsigned char spi_send_wait_receive    ( unsigned int base, unsigned char out );

#endif

