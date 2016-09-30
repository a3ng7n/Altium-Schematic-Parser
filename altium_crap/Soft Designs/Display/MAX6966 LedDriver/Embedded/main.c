////////////////////////////////////////////////////////////////////////////////
// main.c
//
//  IN PACKAGE:         Software Platform examples
//
//  COPYRIGHT:          Copyright (c) 2008, Altium
//
//  DESCRIPTION:        This example demonstrates the MAX6966 peripheral driver

#include <stdlib.h>
#include <stdio.h>

#include <textdisplay.h>
#include <devices.h>
#include <timing.h>
#include <graphics.h>

#include <drv_max6966.h>

#define CLEAR_SCREEN printf( "\x1B[2J")

////////////////////////////////////////////////////////////////////////////////
void main( void )
{
    uint8_t		i, j;
    max6966_t	*myDevice;

    CLEAR_SCREEN;

    myDevice = max6966_open(DRV_MAX6966_1);

    while(1){
        CLEAR_SCREEN;

        printf("This example uses the NB2\n"
               "speaker board LEDs as PWM\n"
               "outputs.\n\n");

        printf("All channels on...\n");
        for(i=0; i<10; i++){
            // Turn on channel
            max6966_set_channel_duty(myDevice, i, 0xFF);
        }
        delay_ms( 1000 );

        printf("All channels off...\n");
        // Reinitialize the device reseting all channels
        max6966_Reset(myDevice);
        delay_ms( 1000 );

        printf("Ascending PWM duty...");
        for(j=0; j<10; j++){
            for(i=3; i<254; i++){
                // Set duty of current channel
                max6966_set_channel_duty(myDevice, j, i);
                delay_ms( 5 );
            }
            // Turn off channel
            max6966_set_channel_duty(myDevice, j, 0);
        }
    }
}


