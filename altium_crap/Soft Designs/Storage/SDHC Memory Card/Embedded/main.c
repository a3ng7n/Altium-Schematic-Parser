#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

// Software platform specific headerfiles
#include <timing.h>
#include <fs.h>
#include <swplatform.h>

static void init( void );

uint8_t data[4096];

sdhc_t * sdcard;

void main( void )
{
    int mountflags;
    int err;
    int handle;
    init();

    for ( int i = 0; i < sizeof( data ); i++ )
    {
        data[i] = rand() & 0xFF;
    }

    for (;;)
    {
        puts( "Please insert sd card..." );
        while( !sdhc_card_detect( sdcard ) )   // Wait for card to be inserted
            ;

        puts( "Card detected in SD slot..." );
        if ( sdhc_card_protected( sdcard ) )
        {
            puts( "Card is write protected" );
            mountflags = MOUNT_FLAG_RDONLY;
        }
        else
        {
            puts( "Card is writeable" );
            mountflags = MOUNT_FLAG_RDWR;
        }

        for ( int i = 0; i < 1; i++ )
        {
            puts( "Initializing..." );
            err = sdhc_card_init( sdcard, SDHC_INIT_POWERON );
            if ( sdhc_is_memcard(err) ) break;
        }

        if ( !sdhc_is_memcard(err) )
        {
            puts( "Card init failed (not a memory card)" );
            printf( "Remove card: ");
            while ( sdhc_card_detect( sdcard ) ) ;
            puts( "OK" );
            continue;
        }
        else
        {
            puts( "Card init OK" );
        }

        printf( "Mounting %s...\n", (mountflags == MOUNT_FLAG_RDONLY) ? "read only" : "read/write" );
        /* Try to mount partition #1 */
        err = mount( "/dev/BLOCKIO_1" ,"/sdcard", "fatfs", 1, mountflags);
        if (err != 0)
        {
            /* ... and if that fails try the entire disk */
            err = mount("/dev/BLOCKIO_1", "/sdcard", "fatfs", 0, mountflags );
        }
        if ( err != 0 )
        {
            puts( "Mount failed, please remove card!" );
        }
        else
        {
            puts( "File system mounted" );

            if ( handle = open( "/sdcard/random.bin", O_RDWR|O_CREAT ), handle < 0 )
            {
                puts( "Unable to create 'random.bin'" );
            }
            else
            {

                puts( "Writing to disk..." );
                write( handle, data, 4000 );
                puts( "Close file..." );
                close( handle );

            }

            // Do something useful here, than unmount the card:

            if ( sdhc_card_removed( sdcard ) )
            {
                unmount( "/sdcard", MOUNT_FLAG_FORCE );
                puts( "Forced unmount" );
            }
            else
            {
                umount( "/sdcard" );
                puts( "File system unmounted, please remove card" );
            }
        }

        while( sdhc_card_detect( sdcard ) )
            ;
        puts( "Card removed" );
    }
}

static void init( void )
{
    puts( "SDHC test, compiled " __DATE__ ", " __TIME__ );

    if ( sdcard = sdhc_open( DRV_SDHC_1 ), !sdcard )
    {
        puts( "Unable to open SDHC driver" );
        abort();
    }

}


