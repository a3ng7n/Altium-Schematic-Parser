/*****************************************************************************
|*  SwPlatform Threads Example
|*
|* Devices:
|*  - LEDs (WB_PRTIO)
|*  - Terminal
|*
|* Services used:
|*  - MULTITHREADING
|*
|* Description:
|*      This example shows extremely basic functionality of threads creation.
|*      The main thread creates eight flashing led threads. The id of the led
|*      to flash is given as an argument. The main thread joins then all the
|*      created threads. These threads run for (+-) 10scs.
|*      It is also shown how threads can pass exit information to each other
|*      with their return values.
|*
\*****************************************************************************/

#include <timing.h>
#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>
#include <devices.h>
#include <per_ioport.h>
#include <stdio.h>

#define  NTHREADS                                    8
#define  RUNTIME_SCS                                 10
#define  FLASH_INTERVAL_MSCS                         100


static volatile uint8_t    *ba_leds;

void *flash_led_thread( void *arg)
{
    uint64_t mark = clock_ms();
    uint8_t  led  = (1<<(int)arg);
    do
    {
        struct timespec ts = { 0, (((int)(arg)+1)*FLASH_INTERVAL_MSCS)*1000L*1000L };
        if ( *ba_leds & led )
        {
            *ba_leds &= ~led;
        }
        else
        {
            *ba_leds |= led;
        }
        nanosleep(&ts,NULL);

    } while (elapsed_time_ms(mark) < RUNTIME_SCS*1000L);

    return arg;
}

int main(int argc, char* argv[])
{
     int i;
     int ret;
     void *     retval[NTHREADS];
     pthread_t  ids[NTHREADS];

    (void)argc;(void)argv;

    if ( ba_leds = (void *)per_ioport_get_base_address(PRTIO), !ba_leds )
    {
       return -1;
    }
    *ba_leds = 0x0;

    // Create flashing led threads
    printf("-------------------------------------\n");
    printf("Creating %d flashing led threads ... \n", NTHREADS);
    for( i = 0; i < NTHREADS; i++ )
    {
        ret = pthread_create( &ids[i], NULL, flash_led_thread, (void *)(i));
        if ( ret != 0 ) {
            return -1;
        }
    }

    // join with threads
    printf("Joining with created threads ( ~ %d scs ) ... \n", RUNTIME_SCS);
    for( i = 0; i < NTHREADS; i++ )
    {
        ret = pthread_join( ids[i], &retval[i] );
        if ( ret != 0 ){
            return -1;
        }
    }

    // checked joinee's return values
    printf("Checking joinee threads return values ... \n");
    for( i = 0; i < NTHREADS; i++ )
    if( retval[i] != (void*)i)
    {
        return -1;
    }
    printf("main() thread exits ... END !\n");
    printf("-------------------------------------\n");

    return 0;
}


