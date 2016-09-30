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
#include <drv_ioport.h>

#define CLEAR_SCREEN printf( "\x1B[2J")

////////////////////////////////////////////////////////////////////////////////
void main(void)
{
    uint32_t     i, j, k;
    owm_t      * owm;
    ioport_t   * buttons;
    graphics_t * display;
    uint8_t      bval = 0x00;
    uint8_t      romBuf[128];
    uint8_t      byte;
    char         outstring[80] = "\0";

    CLEAR_SCREEN;

    owm = owm_open(DRV_OWM_1);
    buttons = ioport_open(DRV_IOPORT_1);
    display = graphics_open(GRAPHICS_1);

    while (1)
    {

        owm_search_devices(owm);
        graphics_set_rotation(display, 90);
        if (owm->device_count <= 0)
        {
            printf("No devices found!");
            delay_ms(1000);
        }
        else
        {
            for (i = 0; i < owm->device_count; i++)
            {
                printf("\x1B[1mDumping Device: %i of %i\n", i + 1, owm->device_count);

                sprintf(outstring, "Serial: ");
                for (j = 0; j < SIZE_SN; j++)
                {
                    sprintf(& outstring[8 + j * 2], "%02X", owm->device_list[i].serial_number[j]);
                }

                if (owm->device_list[i].family == 0x09)
                {
                    ds2406_get_memory(owm, (uint8_t) i, romBuf, 128, 0);
                    if (bval == 0x01)
                    {
                        printf("\n\nROM contents in HEX for \n");
                        printf(outstring);
                        printf("\n");
                        for (j = 1; j < sizeof(romBuf); j++)
                        {
                            printf("%02X", romBuf[j - 1]);
                            if (!(j % 10))
                            {
                                printf(" -%i\n", j);
                            }
                        }
                    }
                    else
                    {
                        if (bval == 0x02)
                        {
                            printf("\n\nROM contents in ASCII for \n");
                            printf(outstring);
                            printf("\n");
                            for (j = 1; j < sizeof(romBuf); j++)
                            {
                                printf("%c", romBuf[j - 1]);
                                if (!(j % 10))
                                {
                                    printf(" -%i\n", j);
                                }
                            }
                        }
                        else
                        {
                            graphics_set_rotation(display, 0);
                            printf("\x1B[2J\x1B[0m\x1B[40mROM contents for ");
                            printf(outstring);
                            printf("\n");
                            k = 1;
                            for (j = 1; j < sizeof(romBuf); j++)
                            {
                                byte = romBuf[j - 1];
                                printf("\x1B[%i;%iH\x1B[35m%02X \x1B[%i;%iH\x1B[36m%c",
                                       k, ((j - 1) * 2) % 20, byte,
                                       k,  (j - 1) % 10 + 22,
                                       (char)byte < ' ' ? '.' : byte );
                                if (!(j % 10))
                                {
                                    printf("\x1B[%i;33H\x1B[37m:\x1B[33m%i", k, j);
                                    k++;
                                }
                            }
                        }
                    }
                }
                else
                {
                    printf("\n UnKnown Device!");
                }
            }
            printf("\x1B[37m\x1B[40m\nUser button 1 for HEX, 2 for ASCII\n3 for refresh >");
            while (!(bval = (~ioport_get_value(buttons, 0) & 0x1F)));
        }
        CLEAR_SCREEN;
    }
}
