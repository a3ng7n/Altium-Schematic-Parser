////////////////////////////////////////////////////////////////////////////////
// main.c
//
//  IN PACKAGE:         Software Platform examples
//
//  COPYRIGHT:          Copyright (c) 2008, Altium
//
//  DESCRIPTION:        This example demonstrates the MAX1229 peripheral driver

#include <stdlib.h>
#include <stdio.h>

#include <textdisplay.h>
#include <devices.h>
#include <timing.h>
#include <graphics.h>

#include <drv_max1229.h>

#define CLEAR_SCREEN printf( "\x1B[2J")

////////////////////////////////////////////////////////////////////////////////
void main( void )
{
    uint8_t i;
    max1229_t *myDevice;

    CLEAR_SCREEN;

    myDevice = max1229_open(DRV_MAX1229_1);

    while(1){

        CLEAR_SCREEN;

        printf("Device Temp: %.2fC\n\n", ((double)max1229_ReadTemperature(myDevice))/8);
        for(i=0;i<12;i++){
            printf("Chan[%2i]: %.2fV\n", i, ((double)max1229_ReadChannel(myDevice, i))*(2.5/4096));
        }

        delay_ms( 2000 );

    }

}


