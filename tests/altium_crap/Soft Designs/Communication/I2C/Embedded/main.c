/********************************************************************\
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    Show how to use the I2CM_W device driver. Use this
|*                  if there is no standard device driver for the
|*                  connected peripheral.
|*
\********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>

#include <timing.h>
#include <devices.h>

// Include device driver interfaces
#include <drv_i2cm.h>
#include <drv_ioport.h>

static void init( void );
static void memdump( void );
void read_per_byte( void );
void write_per_byte( void );
void write_multiple_bytes( void );
void read_multiple_bytes( void );

i2cm_t * i2c;
ioport_t * gpio;

uint8_t mem[64];
uint8_t device_code = 0xA0;     // Device code of 24C04 is "1010" E2 E1 A8 R/!W
#define I2C_READ    0x01
#define I2C_WRITE   0x00

/**********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Program starts here
 */

void main( void )
{
    init();

    write_per_byte();
    read_per_byte();
    write_multiple_bytes();
    read_multiple_bytes();

    puts( "That's all folks!" );
}

/**********************************************************************
|*
|*  Function    : read_per_byte
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Read from I2C bus on a per-byte basis
 */

void read_per_byte( void )
{
    int i;

    puts( "Read per byte:" );
    if ( i2cm_get_bus( i2c ) == -1 )
    {
        puts( "Unable to get control over the I2C bus" );
        abort();
    }

    if (   i2cm_putchar( i2c, true, device_code | I2C_WRITE )   // Generate start and send device write address
        || i2cm_putchar( i2c, false, 0 )                        // Set address counter to 0
        || i2cm_putchar( i2c, true, device_code | I2C_READ )   // Generate restart and send read address of device
       )
    {
        puts( "Could not write address" );
        i2cm_stop( i2c );
        i2cm_release_bus( i2c );
        abort();
    }

    // From here on, we can do a multibyte read until we're finished
    for ( i= 0; i < sizeof( mem ) - 1; i++ )
    {
        mem[i] = (uint8_t)i2cm_getchar( i2c, false, true );      // Get byte and acknowledge
    }
    mem[i] = (uint8_t)i2cm_getchar( i2c, false, false );         // Last one, NAK
    i2cm_stop( i2c );
    i2cm_release_bus( i2c );

    memdump();
}

/**********************************************************************
|*
|*  Function    : write_per_byte
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Write to I2C bus on a per-byte basis
 */

void write_per_byte( void )
{
    uint8_t i;
    int retry;

    const char * data = "This was written on a per-byte basis";

    printf( "Write per byte: " );
    if ( i2cm_get_bus( i2c ) == -1 )
    {
        puts( "Unable to get control over the I2C bus" );
        abort();
    }

    for ( i = 0; i < strlen( data ); i++ )
    {
        // The 24C04 needs up to 10 msec for a write. During the write cycle, it does not ACK it's
        // device slave address. Thus, we keep trying for 10 msec until we do get an ACK
        for ( retry = 10; retry; retry-- )
        {
            if (  !i2cm_putchar( i2c, true, device_code | I2C_WRITE ))  // Generate start and send device write address
                break;
            if ( retry ) i2cm_stop( i2c );
            for ( clock_t t = clock() + CLOCKS_PER_SEC / 1000; clock() < t; ) __nop();
        }
        if (  !retry                                                // Address acknowledged
            || i2cm_putchar( i2c, false, i )                        // Set address counter
            || i2cm_putchar( i2c, false, data[i] )                  // Send data
           )
        {
            i2cm_stop( i2c );
            printf( "\nUnable to write byte %d\n", i );
            break;
        }
        putchar( data[i] );
        i2cm_stop( i2c );
    }

    i2cm_release_bus( i2c );
    printf( "\nDone writing %d bytes\n", i );

}

/**********************************************************************
|*
|*  Function    : write_multiple_bytes
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Write something using the multibyte transfer functions
 */

void write_multiple_bytes( void )
{
    int retry;
    int address = 0x14;
    int i;
    size_t length;
    char data[] = " bulk write";

    printf( "Write chunks of bytes: " );
    if ( i2cm_get_bus( i2c ) == -1 )
    {
        puts( "Unable to get control over the I2C bus" );
        abort();
    }

    length = strlen( data );
    for ( i = 0; i < strlen( data ); i += 4 )
    {
        // 24C04 in multibyte mode supports 4 byte writes only
        data[i] = address + i;
        for ( retry = 0; retry < 10; retry++ )
        {
            if ( retry ) for ( clock_t t = clock() + CLOCKS_PER_SEC / 1000; clock() < t; ) __nop(); // Wait for 1 msec
            if ( !i2cm_write( i2c, device_code | I2C_WRITE, (void*)(data + i), (length > 4) ? 5 : length ) ) break;
        }
        if ( retry == 10 )
        {
            puts( "Multibyte write failed" );
            abort();
        }
        length -= 4;
    }
    puts( "done." );
    i2cm_release_bus( i2c );
}

/**********************************************************************
|*
|*  Function    : read_multiple_bytes
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Read something using the multibyte transfer functions
 */

void read_multiple_bytes( void )
{
    puts( "Read multibyte:" );
    if ( i2cm_get_bus( i2c ) == -1 )
    {
        puts( "Unable to get control over the I2C bus" );
        abort();
    }

    if (   i2cm_putchar( i2c, true, device_code | I2C_WRITE )   // Generate start and send device write address
        || i2cm_putchar( i2c, false, 0 )                        // Set address counter to 0
       )
    {
        puts( "Could not write address" );
        i2cm_stop( i2c );
        i2cm_release_bus( i2c );
        abort();
    }
    if (i2cm_read( i2c, device_code | I2C_READ, mem, sizeof( mem )) == -1)
    {
        puts( "Unable to read from device" );
        abort();
    }
    i2cm_release_bus( i2c );
    memdump();
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
    // Say hello
    puts( "WB_I2CM 24C04 example compiled " __DATE__ ", " __TIME__ );

    // Open the I2C device driver
    if ( i2c = i2cm_open( DRV_I2CM_1 ), !i2c )
    {
        puts( "Unable to open I2C driver" );
        abort();
    }

    // Open the I/O port

    if ( gpio = ioport_open( DRV_IOPORT_1 ), !gpio )
    {
        puts( "Unable to open IOPORT driver" );
        abort();
    }
    ioport_set_value( gpio, 0, 0x01 );      // Set base address of 24C04 to A0, PRE = 0, MODE = 1 (multibyte write)
    device_code = 0xA0;
}

/**********************************************************************
|*
|*  Function    : memdump
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Nicely formatted dump of memory buffer
 */

static void memdump( void )
{
    char buf[17] = { 0 };
    size_t i;
    for( i = 0; i < sizeof( mem ); i++ )
    {
        if ( (i & 0x0F) == 0 )
        {
            printf( "%04X:", i );
        }
        printf( " %02X", mem[i] );
        buf[i & 0x0F] = (mem[i] < ' ' || mem[i] > 127) ? '.' : mem[i];
        if ( (i & 0x0F) == 0x0F )
        {
            printf( "  %s\n", buf );
        }
    }
}

