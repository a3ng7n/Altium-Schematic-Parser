#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <swplatform.h>
#include "wave.h"

static void init( void );

spdif_t * spdif;

int32_t sound[64];
uint16_t lstat[24] = { 0x0000 } ;
uint16_t rstat[24] = { 0x0000 } ;

extern __no_sdata uint8_t _lc_ub_gitar_wav;
extern __no_sdata uint8_t _lc_ue_gitar_wav;

int main( void )
{
    wav_header_t header;

    init();

    lstat[0] = SPDIF_CSTAT0_CONSUMER | SPDIF_CSTAT0_PCM_AUDIO | SPDIF_CSTAT0_COPY_PERMITTED | SPDIF_CSTAT0_2_CHANNELS;
    rstat[0] = SPDIF_CSTAT0_CONSUMER | SPDIF_CSTAT0_PCM_AUDIO | SPDIF_CSTAT0_COPY_PERMITTED | SPDIF_CSTAT0_2_CHANNELS;
    lstat[1] = SPDIF_CSTAT1_STEREO_LEFT | SPDIF_CSTAT1_44KHZ | SPDIF_CSTAT1_CLK_DEF;
    rstat[1] = SPDIF_CSTAT1_STEREO_RIGHT| SPDIF_CSTAT1_44KHZ | SPDIF_CSTAT1_CLK_DEF;
    lstat[2] = SPDIF_CSTAT2_WORD_MAX24 | SPDIF_CSTAT2_WORD_SUBTRACT_0 | SPDIF_CSTAT2_ORGFREQ_44KHZ | SPDIF_CSTAT2_COPY_DENIED;
    rstat[2] = SPDIF_CSTAT2_WORD_MAX24 | SPDIF_CSTAT2_WORD_SUBTRACT_0 | SPDIF_CSTAT2_ORGFREQ_44KHZ | SPDIF_CSTAT2_COPY_DENIED;
    spdif_write_status_bitstream( spdif, lstat, rstat, sizeof( lstat ) * 16);
    spdif_tx_start( spdif );

    header = wav_parseheader( (uintptr_t) &_lc_ub_gitar_wav, (uintptr_t) &_lc_ue_gitar_wav );
    if ( wav_testsupported( header ) )
    {
        printf( "WAVE channels   = %d\n", header.channels );
        printf( "WAVE samplerate = %d Hz\n", header.samplerate );
        printf( "WAVE samplesize = %d bits/sample\n", header.samplesize );
        printf( "WAVE duration   = %d sec.\n", header.playtime );
        wav_playfile( spdif, header, (uintptr_t) &_lc_ub_gitar_wav, (uintptr_t) &_lc_ue_gitar_wav );
    }
    else
    {
        puts( "Unsupported WAVE format" );
    }

    puts( "Done." );
    return 0;
}


static void init( void )
{
    puts( "SPDIF test compiled " __DATE__ ", " __TIME__ );

    printf( "OK\nSPDIF device driver:  " );
    if ( spdif = spdif_open( DRV_SPDIF_1 ), !spdif )
    {
        puts( "Fail" );
        abort();
    }

    puts( "OK" );
}


