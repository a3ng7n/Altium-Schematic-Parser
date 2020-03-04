/*****************************************************************************\
|*
|*  IN PACKAGE:         Software Platform Builder
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:        Shows how to use the generic Audio driver
|*
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

// Application Stack interface
#include <devices.h>
#include <timing.h>
#include <drv_spimux.h>
#include <audio.h>

#include "wave.h"

// Audio file 'sound.bin' is defined compiletime in native endianness:
#include "sound_bin.h"
const int16_t sound_bin[] = const_sound_bin;

// Audio file 'sound.wav' is linked at compiletime:
extern __no_sdata uint8_t _lc_ub_sound_wav;
extern __no_sdata uint8_t _lc_ue_sound_wav;

audio_t * audio;
spimux_t * spimux;

static void init( void );
void close_spibus( void );


/**********************************************************************
|*
|*  FUNCTION    : main
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : main program
 */

void main( void )
{
    int res;
    size_t  samples;
    const int16_t * buf;
    wav_header_t info;

    init();

    for (;;)
    {
        info = wav_parseheader( (uintptr_t) &_lc_ub_sound_wav, (uintptr_t) &_lc_ue_sound_wav );
        if (wav_testsupported( info ))
        {
            printf( "Play 'sound.wav'\n-> " );
            wav_playfile( audio, info, (uintptr_t) &_lc_ub_sound_wav, (uintptr_t) &_lc_ue_sound_wav );
        }

        printf( "Play 'sound.bin'\n-> " );
        audio_set_format( audio, 22050, 1, 16 );    // 22 kHz, mono, 16 bits/sample
        audio_set_volume( audio, 255 );             // Volume fully open

        samples = sizeof(sound_bin) / sizeof(sound_bin[0]);
        printf( "Playing %d samples...", samples );

        for ( buf = sound_bin; samples; samples -= res )
        {
            res = audio_play( audio, buf, samples );
            buf += res;
        }
        puts( " done!" );
    }
}

/**********************************************************************
|*
|*  FUNCTION    : close_spibus
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Closes the SPI bus on exit
 */


void close_spibus( void )
{
    spi_release_bus( spimux );
}

/**********************************************************************
|*
|*  FUNCTION    : init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize hardware and drivers
 */

static void init( void )
{
    // Say hello to the user
    puts( "Audio service example, " __FILE__ " compiled " __DATE__ ", " __TIME__ );

    printf( "OK\nGetting control over SPI bus... " );
    // Get control over the bus
    if ( spimux = spimux_open( DRV_SPIMUX_1 ), !spimux )
    {
        puts( "Fail" );
        abort();
    }

    while( !spi_get_bus( spimux, 11 ) ) __nop();

    // Initialize audio
    printf( "OK\nInitializing audio... " );
    if ( audio = audio_open( AUDIO_1 ), !audio )
    {
        puts( "Fail" );
        abort();
    }
    puts( "OK\nInit Ready." );
    close_spibus();
}


