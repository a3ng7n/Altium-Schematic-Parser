/*****************************************************************************\
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:        EMAC32 ethernet driver engineering example
|*
|*                      This file shows demonstrates a very basic
|*                      client able to answer to ping (echo) request
 */

#include <stdio.h>

#include <devices.h>
#include <drv_emac32.h>

#include "pingstack.h"

//*** demo settings ****************************************************

// maximum supported framesize
#define ETHBUF_SIZE 512

// our MAC address
static uint8_t my_mac[6] = { 0x11, 0x22, 0x33, 0x44, 0x55, 0x66 };

// our IP number
static const uint8_t my_ip[4] = { 192, 168, 100, 111 };


//*** SSAS drivers *****************************************************

emac32_t *emac32;


/**********************************************************************
|*
|*  FUNCTION    : sys_init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize system
 */
void sys_init(void)
{
    emac32 = emac32_open(DRV_EMAC32_0);

    printf("SSAS EMAC32 ICMP echo ('ping') demonstration\n\n");

    printf("MAC: %02X-%02X-%02X-%02X-%02X-%02X\n", my_mac[0], my_mac[1], my_mac[2], my_mac[3], my_mac[4], my_mac[5]);
    printf("IP: %i.%i.%i.%i\n", my_ip[0], my_ip[1], my_ip[2], my_ip[3]);
    printf("(set MAC and IP in main.c)\n\n");

    // set MAC in source instead of in SwPlatform
    emac32_setmac(emac32, my_mac);
    emac32_setmode(emac32, EMAC32_RX_CMD_ENABLE | EMAC32_RX_CMD_UNICAST | EMAC32_RX_CMD_BROADCAST, EMAC32_TX_CMD_ENABLE);

    eth_init(my_mac, my_ip);
}


/**********************************************************************
|*
|*  FUNCTION    : main
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Main loop
 */
void main(void)
{
    uint32_t ethbuf32[ETHBUF_SIZE / sizeof(uint32_t)];
    uint8_t *ethbuf = (uint8_t*) ethbuf32;
    int size;

    sys_init();

    for (;;)
    {
        size = emac32_receive(emac32, ethbuf, ETHBUF_SIZE);

        if (size > 0)
        {
           if (size > ETHBUF_SIZE) size = ETHBUF_SIZE;

           size = eth_process(ethbuf, size);

           if (size > 0)
           {
               emac32_send(emac32, ethbuf, size);
           }
        }
        else if (size < 0)
        {
            printf("received frame with error or framebuffer overrun\n");
        }

    }
}


