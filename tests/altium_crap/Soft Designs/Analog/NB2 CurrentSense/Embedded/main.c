////////////////////////////////////////////////////////////////////////////////
// main.c
////////////////////////////////////////////////////////////////////////////////
//
//  IN PACKAGE:         Software Platform examples
//
//  COPYRIGHT:          Copyright (c) 2008, Altium
//
//  DESCRIPTION:        This example demonstrates the MAX1229 via a custom NB2
//                      Board driver.
//
////////////////////////////////////////////////////////////////////////////////
#include <stdlib.h>
#include <stdio.h>

#include <textdisplay.h>
#include <devices.h>
#include <timing.h>
#include <graphics.h>

#include <drv_max1229.h>

#include "NB2CurrentSense.h"

////////////////////////////////////////////////////////////////////////////////
#define CLEAR_SCREEN printf( "\x1B[2J")

////////////////////////////////////////////////////////////////////////////////
void main( void )
{
    uint8_t i;

    CLEAR_SCREEN;

    NB2CS_Init(max1229_open(DRV_MAX1229_1), max1229_open(DRV_MAX1229_2));

    while(1){

        CLEAR_SCREEN;
        
        printf("NB2 Board Temp: %.2fC\n\n", NB2CS_GetBoardTemp());

        for(i=0;i<24;i+=2){
            printf("%6s: %.2f%c",       NB2CS_GetChannelName(i),    NB2CS_GetChannelValue(i),   NB2CS_GetChannelType(i)==eNB2Voltage?'V':'A');
            printf("\t%6s: %.2f%c\n",   NB2CS_GetChannelName(i+1),  NB2CS_GetChannelValue(i+1), NB2CS_GetChannelType(i)==eNB2Voltage?'V':'A');
        }

        delay_ms( 2000 );

    }
}


