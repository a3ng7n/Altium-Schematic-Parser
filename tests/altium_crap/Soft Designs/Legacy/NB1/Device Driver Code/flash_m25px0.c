/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   M25PX0 Flash memory controller device driver
|*
\*****************************************************************************/

//..............................................................................
#include "wb_spi.h"
#include "flash_m25px0.h"
//..............................................................................

//..............................................................................
unsigned char m25px0_read_electronic_signature(unsigned int base)
{
    static unsigned char result;

    spi_wait_and_send(base,M25PX0_RES);       // Send RES command
    spi_wait_and_send(base,0);                // send three dummy bytes
    spi_wait_and_send(base,0);
    spi_wait_and_send(base,0);

    result = spi_send_wait_receive(base,0);

    return result;
}
//..............................................................................

//..............................................................................
void m25px0_send_address24(unsigned int base, unsigned int M25PX0_address)
{
    spi_wait_and_send(base, (unsigned char) (M25PX0_address >> 16));  // Send bits 23..16
    spi_wait_and_send(base, (unsigned char) (M25PX0_address >>  8));  // Send bits 15..8
    spi_wait_and_send(base, (unsigned char) (M25PX0_address      ));  // Send bits 7..0
}
//..............................................................................

//..............................................................................
unsigned int  m25px0_is_writing                ( unsigned int base )
{
    unsigned char result;
    spi_cs_hi                      (base);
    spi_cs_lo                      (base);
    spi_wait_and_send              (base, M25PX0_RDSR);
    result = spi_send_wait_receive (base, 0) & 0x01;
    spi_cs_hi                      (base);
    return result;
}
//..............................................................................

//..............................................................................
void m25px0_wait_while_writing (unsigned int base)
{
    spi_cs_hi         (base);
    spi_cs_lo         (base);
    spi_wait_and_send (base, M25PX0_RDSR);
    while (spi_send_wait_receive (base, 0) & 0x01)
    {
    }
    spi_cs_hi            (base);
}
//..............................................................................

//..............................................................................
void m25px0_open_read(unsigned int base, unsigned int address)
{
    spi_cs_hi            (base);
    spi_cs_lo            (base);
    spi_wait_and_send    (base,M25PX0_FastRead);
    m25px0_send_address24(base,address);
    // send extra byte now so core will spi_send_receive_last will have the first
    // byte
    spi_wait_and_send    (base,0);
}
//..............................................................................

//..............................................................................
void m25px0_open_write ( unsigned int base, unsigned int  address )
{
    spi_cs_hi            (base);
    spi_cs_lo            (base);
    spi_wait_and_send    (base, M25PX0_WREN);
    spi_wait_while_busy(base);
    spi_cs_hi            (base);
    spi_cs_lo            (base);
    spi_wait_and_send    (base, M25PX0_PP);
    m25px0_send_address24(base, address);
}
//..............................................................................

//..............................................................................
void          m25px0_close      ( unsigned int base )
{
    spi_wait_while_busy(base);
    spi_cs_hi          (base);
}
//..............................................................................

//..............................................................................
void          m25px0_enter_deep_powerdown      ( unsigned int base )
{
    spi_cs_hi          (base);
    spi_cs_lo          (base);
    spi_wait_and_send  (base, M25PX0_DP);
    spi_wait_while_busy(base);
    spi_cs_hi          (base);
}
//..............................................................................

//..............................................................................
void          m25px0_exit_deep_powerdown       ( unsigned int base )
{
    spi_cs_hi          (base);
    spi_cs_lo          (base);
    spi_wait_and_send  (base, M25PX0_RES);
    spi_wait_while_busy(base);
    spi_cs_hi          (base);
}
//..............................................................................

//..............................................................................
void          m25px0_set_block_protect         ( unsigned int base, unsigned char protection )
{
    spi_cs_hi          (base);
    spi_cs_lo          (base);
    spi_wait_and_send  (base, M25PX0_WREN);
    spi_wait_while_busy(base);
    spi_cs_hi          (base);
    spi_cs_lo          (base);
    spi_wait_and_send  (base, M25PX0_WRSR);
    spi_wait_and_send  (base, protection);
    spi_wait_while_busy(base);
    spi_cs_hi          (base);
}
//..............................................................................

//..............................................................................
void          m25px0_erase_all                 ( unsigned int base )
{
    spi_cs_hi          (base);
    spi_cs_lo          (base);
    spi_wait_and_send  (base, M25PX0_WREN);
    spi_wait_while_busy(base);
    spi_cs_hi          (base);
    spi_cs_lo          (base);
    spi_wait_and_send  (base, M25PX0_BE);
    spi_wait_while_busy(base);
    spi_cs_hi          (base);
}
//..............................................................................

//..............................................................................
void          m25px0_erase_sector              ( unsigned int base, unsigned int address )
{
    spi_cs_hi            (base);
    spi_cs_lo            (base);
    spi_wait_and_send    (base, M25PX0_WREN);
    spi_wait_while_busy  (base);
    spi_cs_hi            (base);
    spi_cs_lo            (base);
    spi_wait_and_send    (base, M25PX0_SE);
    m25px0_send_address24(base,address);
    spi_wait_while_busy  (base);
    spi_cs_hi            (base);
}
//..............................................................................


