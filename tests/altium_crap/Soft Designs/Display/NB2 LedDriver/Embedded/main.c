////////////////////////////////////////////////////////////////////////////////
// main.c
////////////////////////////////////////////////////////////////////////////////
//
//  IN PACKAGE:         Software Platform examples
//
//  COPYRIGHT:          Copyright (c) 2008, Altium
//
//  DESCRIPTION:        This example demonstrates the MAX6966 via a custom
//                       NB2 Led Driver
////////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <stdio.h>

#include <textdisplay.h>
#include <devices.h>
#include <timing.h>
#include <graphics.h>

#include <drv_max6966.h>

#include "NB2LedDriver.h"

#define CLEAR_SCREEN printf( "\x1B[2J")

////////////////////////////////////////////////////////////////////////////////
void main( void )
{
    uint8_t ledIdx;
    uint8_t rr,gg,bb;

    CLEAR_SCREEN;

    printf("This example demonstrates an\n"
           "NB2 Speaker Board LED driver\n\n");

    NB2LD_Init(max6966_open(DRV_MAX6966_1), max6966_open(DRV_MAX6966_2));

    while(1){
        rr = gg = bb = 0;

        for(rr=0;rr<255;rr+=5)
            for(ledIdx=0;ledIdx<6;ledIdx++){
                NB2LD_SetLedRGB(ledIdx, rr, gg, bb);
                delay_ms( 5 );
            }

        for(gg=0;gg<255;gg+=5)
            for(ledIdx=0;ledIdx<6;ledIdx++){
                NB2LD_SetLedRGB(ledIdx, rr, gg, bb);
                delay_ms( 5 );
            }

        for(bb=0;bb<255;bb+=5)
            for(ledIdx=0;ledIdx<6;ledIdx++){
                NB2LD_SetLedRGB(ledIdx, rr, gg, bb);
                delay_ms( 5 );
            }
    }

}


