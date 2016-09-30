/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Nanoboard SPI controller interface
|*
\*****************************************************************************/

#ifndef __NB_SPI_H__
#define __NB_SPI_H__

//..............................................................................
void nanoboard_spi_open (unsigned int base, unsigned char device);
void nanoboard_spi_close(unsigned int base);
//..............................................................................

//..............................................................................
// The Nanoboard NB-1 SPI devices are all shared between the Daughterboard FPGA
// and the NanoTalk controller which is managed from the host PC through the
// parallel port.
// To access these devices from the daughter board FPGA, and hence the
// embedded software running on an FPGA-based processor, we need to switch the
// SPI multiplexer to the correct device.
// Once this is done then the SPI device can be accessed like any other SPI
// device.
//..............................................................................

//..............................................................................
// SPI Device nubers for SPI_open function
//..............................................................................
#define DEVICE_M25P40_CONFIGURATION 1
#define DEVICE_M25P40_EMBEDDED      2
#define DEVICE_TESTER               3
#define DEVICE_CLOCK                4
//..............................................................................

#endif // __SPI_NANOBOARD_H__

