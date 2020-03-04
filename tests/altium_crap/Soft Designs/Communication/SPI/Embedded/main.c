/********************************************************************\
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    Demonstrate the use of SPI driver
|*
\********************************************************************/


#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

#include <devices.h>
#include <drv_spi.h>
#include <per_ioport.h>

#define NB_RGBLEDS_ADDR_1    0x13
#define NB_RGBLEDS_ADDR_2    0x14

// SW Platform required variables:
spi_t * spi;
volatile uint8_t * red;
volatile uint8_t * green;
volatile uint8_t * blue;

// Local functions:
static void init( void );
static void setleds( uint16_t redval, uint16_t greenval, uint16_t blueval );

/**********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Demonstrate the use of SPI
 */

void main( void )
{
    uint16_t pwm_red, pwm_green, pwm_blue;

    init();

    while( 1 )
    {
        pwm_red = *red;
        if ( pwm_red < 3 ) pwm_red = 0xFF;
        else if ( pwm_red == 0xFF ) pwm_red = 2;

        pwm_green = *green;
        if ( pwm_green < 3 ) pwm_green = 0xFF;
        else if ( pwm_green == 0xFF ) pwm_green = 2;

        pwm_blue = *blue;
        if ( pwm_blue < 3 ) pwm_blue = 0xFF;
        else if ( pwm_blue == 0xFF ) pwm_blue = 2;

        // Get NB multiplexer channel to MAX6966 #1 that controls LED1, LED2 and LED3
        while( !spi_get_bus( spi, NB_RGBLEDS_ADDR_1 ) ) __nop();

        // We got the bus, but it is possible the bus has been reconfigured by the previous owner
        // So, we have to set all the parameters for our communication here!
        spi_set_endianess( spi, true );
        spi_set_mode( spi, SPI_MODE0 );
        spi_set_baudrate( spi, 3000000 );

        // Disable all the LEDs
        setleds( pwm_red, pwm_green, pwm_blue );

        // Done, release the bus to the system
        spi_release_bus( spi );

        // Repeat for the second MAX6966 that controls LED4, LED5 and LED6
        while( !spi_get_bus( spi, NB_RGBLEDS_ADDR_2 ) ) __nop();
        spi_set_endianess( spi, true );
        spi_set_mode( spi, SPI_MODE0 );
        spi_set_baudrate( spi, 3000000 );

        setleds( pwm_red, pwm_green, pwm_blue );

        spi_release_bus( spi );

    }
}

/**********************************************************************
|*
|*  Function    : init
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Initialize the hardware
 */

static void init( void )
{
    // Open the SPI port
    spi = spi_open( DRV_SPI_1 );

    // Open the GPIO port
    red = (volatile uint8_t *)per_ioport_get_base_address( PRTIO );
    green = red + 1;
    blue = red + 2;

}

/**********************************************************************
|*
|*  Function    : setleds
|*
|*  Parameters  : value = value to send to the LEDs
|*
|*  Returns     : None
|*
|*  Description : Send a couple of values to the LEDs
|*                First, set the configuration register to 0x21
|*                Than fill registers 0 - 9 with the given value
|*                Note: transfers require CS to be raised before latching!
 */

static void setleds( uint16_t redval, uint16_t greenval, uint16_t blueval )
{
    // Standard transfer using 8-bit transceive functions. The transceive functions wait until
    // the transmission has finished because they need the MISO result from the SPI transfer.
    spi_cs_lo( spi );
    spi_transceive8( spi, 0x10 );
    spi_transceive8( spi, 0x21 );
    spi_cs_hi( spi );

    // Now send out the individual colors, with CS raised between transfers (the MAX latches on CS rising edge).

    // First transfer using 8-bit mode
    // Transmit waits if necessary before sending new data, but does not wait until the transfer completes
    // Therefore, we have to wait until transmission is complete before raising CS

    spi_cs_lo( spi );
    spi_transmit8( spi, 0x00 );                     // Address is 0 for P0
    spi_transmit8( spi, (uint8_t) greenval );       // P0 is connected to green for LEDs 1 and 4
    while( spi_busy( spi ) ) __nop();               // Wait until transmission is done
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    __nop();


    // Now do the same for P1 (blue), but in a single 16-bit transfer
    spi_cs_lo( spi );
    spi_transmit16( spi, 0x0100 | blueval );        // Send out address and value in a single 16-bit transfer
    while( spi_busy( spi ) ) __nop();               // Wait until transmission is done
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    __nop();

    // Transceive waits until the actual transmission is finished because it returns the value sent by the slave
    spi_cs_lo( spi );
    spi_transceive8( spi, 0x02 );                   // Address is 2 for P2
    spi_transceive8( spi, (uint8_t) redval );       // P2 is connected to red for LEDs 1 & LED4
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    __nop();

    // Or, again, in 16-bit mode:
    spi_cs_lo( spi );
    spi_transceive16( spi, 0x0300 | greenval );     // Address is 3 for P3, P3 is connected to green of LEDs 2 and 5
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    // We started with endianess set to BIG endian, meaning the high byte is transmitted first
    // Let's switch to little endian mode:

    spi_set_endianess( spi, false );

    // In 8-bit transfer modes, there is no difference. So:
    spi_cs_lo( spi );
    spi_transmit8( spi, 0x04 );                     // Address is 4 for P4
    spi_transmit8( spi, (uint8_t) blueval );        // P4 is connected to blue for LEDs 2 and 5
    while( spi_busy( spi ) ) __nop();               // Wait until transmission is done
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    __nop();

    // In 16-bit transfer mode, the least significant byte is shifted out first in little endian mode:
    spi_cs_lo( spi );
    spi_transmit16( spi, (redval << 8) | 0x05 );    // Send out address 5 (P5) and value (red for LED2/LED5) in a single 16-bit transfer
    while( spi_busy( spi ) ) __nop();               // Wait until transmission is done
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    __nop();


    // Endianess is valid for all transfer functions: transmit, transceive (and receive):

    // In 8-bit transfer modes, this doesn't matter. So:
    spi_cs_lo( spi );
    spi_transceive8( spi, 0x06 );                   // Address is 6 for P6
    spi_transceive8( spi, (uint8_t) greenval );     // P6 is connected to green for LEDs 3 and 6
    while( spi_busy( spi ) ) __nop();               // Wait until transmission is done
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    __nop();

    // In 16-bit transfer mode, the LSB is shifted out first in little endian mode:
    spi_cs_lo( spi );
    spi_transceive16( spi, (blueval << 8) | 0x07 ); // Send out address 7 (P7) and value (blue for LED3 and LED6) in a single 16-bit transfer
    while( spi_busy( spi ) ) __nop();               // Wait until transmission is done
    spi_cs_hi( spi );                               // Latch the value in the MAX6966

    __nop();


    // Let's play nice for the next time we need SPI ;)
    spi_set_endianess( spi, true );

    // A 32-bit transfer, first 16 bits are ignored in MAX6966...
    spi_cs_lo( spi );
    spi_transceive32( spi, 0x800 | redval );
    spi_cs_hi( spi );                               // Latch red
}


