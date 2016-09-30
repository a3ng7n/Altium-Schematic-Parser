/********************************************************************\
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    Show how to use the M25PX0 serial flash memory
|*                  device driver
|*
\********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

// Include device driver interfaces
#include <timing.h>
#include <devices.h>
#include <drv_m25px0.h>

// SSAS device driver pointers
m25px0_t * flash;

// Local prototypes
static void init( void );

static void hexdump( uint8_t * memory, size_t bytes );

/**********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Main loop - write compilation time to RTC and loop
|*                printing calendar time from that
 */

void main( void )
{
    unsigned short waitstate = 0;

    clock_t t1, t2;
    uint8_t buf[16] = { 0 };

    init();
    printf( "\nReading..." );
    t1 = clock();
    while( m25px0_read( flash, 0, buf, sizeof( buf )) == 0 ) __nop();
    t2 = clock();
    printf( "done in %lld ticks. Contents of buffer:\n", t2 - t1 );
    hexdump( buf, sizeof( buf ));

    printf( "Read status = %02X\n", m25px0_status( flash ));
    printf( "Disable write protect blocks..." );
    while( m25px0_set_protection( flash, 0 ) < 0 ) __nop();
    printf( " busy..." );
    while (m25px0_status( flash ) & M25PX0_STAT_WIP ) __nop();
    puts( " ready." );

    printf( "Read status = %02X\n", m25px0_status( flash ));
#if 0       // Set to 1 to use sector erasure.
    printf( "Erase sector 0..." );
    while (m25px0_sector_erase( flash, 0 ) < 0 ) __nop();
#else
    printf( "Bulkerase..." );
    while( m25px0_bulk_erase( flash ) < 0 ) __nop();
#endif
    printf( " started...");
    while (m25px0_status( flash ) & M25PX0_STAT_WIP ) __nop();
    puts( " ready." );

    while( m25px0_read( flash, 0, buf, sizeof( buf )) == 0 ) __nop();
    hexdump( buf, sizeof( buf ));

    printf( "Programming... ");
    while( m25px0_program_page( flash, 1, "Hello, world", 12 ) != 12 ) __nop();
    printf( " started...");
    while (m25px0_status( flash ) & M25PX0_STAT_WIP ) __nop();
    puts( " ready." );
    while( m25px0_read( flash, 0, buf, sizeof( buf )) == 0 ) __nop();
    hexdump( buf, sizeof( buf ));
    puts( "Done." );
}


/**********************************************************************
|*
|*  Function    : init
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Initialize hardware & device drivers
 */

static void init( void )
{
    do
    {
        flash = m25px0_open( DRV_M25PX0_1 );
    } while( flash == NULL );

    setbuf( stdout, NULL );
    puts( "File '" __FILE__ "' compiled " __DATE__ ", " __TIME__ );
}


/**********************************************************************
|*
|*  Function    : hexdump
|*
|*  Parameters  : memory = pointer to memory to be shown
|*                bytes = size of memoryblock to be shown
|*
|*  Returns     : None
|*
|*  Description : Show the contents of a part of memory
 */

void hexdump( uint8_t * memory, size_t bytes )
{
    char ascii[17] = { 0 };
    size_t i;
    for ( i = 0; i < bytes; i++ )
    {
        if ( (i & 0x0F) == 0 ) printf( "  %s\n%06X:", ascii, i );
        ascii[i & 0x0F] = ( (memory[i] < 0x20) || (memory[i] > 0x7F) ) ? '\x7F' : memory[i];
        printf( " %02X", memory[i] );
    }
    if ( bytes & 0x0F ) ascii[bytes & 0x0F] = '\0';
    while( i++ & 0x0F) printf( "   " );
    printf( "  %s\n", ascii );
}
