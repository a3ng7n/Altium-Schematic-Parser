////////////////////////////////////////////////////////////////////////////////
// main.c
//
//  IN PACKAGE:         Software Platform examples
//
//  COPYRIGHT:          Copyright (c) 2008, Altium
//
//  DESCRIPTION:        This example shows how to enumerate and access one wire
//                      device(s). In this specific case DS2406 device(s).


#include <stdlib.h>
#include <stdio.h>

#include <textdisplay.h>
#include <devices.h>
#include <timing.h>
#include <graphics.h>

#include <drv_owm.h>
#include <drv_owd_ds2406.h>

#define CLEAR_SCREEN printf( "\x1B[2J")

////////////////////////////////////////////////////////////////////////////////
void main( void )
{
    uint32_t i,j;
    owm_t *owm;
    uint8_t romBuf[128];

    CLEAR_SCREEN;

    owm = owm_open(DRV_OWM_1);

    while(1){

        owm_search_devices(owm);

        if(owm->device_count <= 0){
            printf("No devices found!");
            delay_ms( 1000 );
        } else {
            for(i=0;i<owm->device_count;i++){
                printf( "\x1B[1mDumping Device: %i of %i\n", i+1, owm->device_count );

                printf("\nSerial: ");
                for(j=0;j<SIZE_SN;j++){
                    printf("%02X", owm->device_list[i].serial_number[j]);
                }

                if(owm->device_list[i].family == 0x12){
                    printf("\n\nRAM contents:\n");
                    ds2406_get_memory(owm, (uint8_t)i, romBuf, 128, 0);
                    for(j=1;j<sizeof(romBuf);j++){
                        printf("%02X", romBuf[j-1]);
                        if(!(j%10)){
                            printf(" -%i\n",j);
                        }
                    }
                } else {
                    printf("\n UnKnown Device!");
                }

                delay_ms( 1000 );
                CLEAR_SCREEN;
            }
        }
        CLEAR_SCREEN;
    }
}


