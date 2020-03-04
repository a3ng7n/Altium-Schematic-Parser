/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Nanoboard SPI controller interface
|*
\*****************************************************************************/

//..............................................................................
#include "wb_spi.h"
#include "nb_spi.h"
//..............................................................................

//..............................................................................
//  This writes to the SPI bus multiplexer on the Nanobord's FPGA and selects
//  the specific SPI device.
//..............................................................................
void nanoboard_spi_open(unsigned int base, unsigned char device)
{
    device |= 0x80;    //Enable bus request bit in address register

    spi_control_write  (base,SPI_CTRL_ENABLE);
    spi_divider_write  (base,spi_divisor());

    spi_mode_hi        (base); //Access SPI address register in Nanoboard Controller

    spi_send_wait_receive(base,0x80 | device);
    spi_mode_lo          (base);
    spi_cs_lo            (base);
}
//..............................................................................

//..............................................................................
// Close SPI Nanoboard SPI channel and clear the address register
//..............................................................................
void nanoboard_spi_close(unsigned int base)
{
    spi_mode_hi          (base  );
    spi_send_wait_receive(base,0);
    spi_cs_hi            (base  );
}
//..............................................................................


