/*****************************************************************************\
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:        ETHERNET ethernet driver engineering example
|*
|*                      This file shows demonstrates a very basic
|*                      client able to answer to ping (echo) request
 */

#include <stdio.h>

#include <devices.h>
#include <ethernet.h>

#include "pingstack.h"

//*** demo settings ****************************************************

// maximum supported framesize
#define ETHBUF_SIZE 512

// our MAC address
static uint8_t my_mac[6] = { 0x02, 0x00, 0x01, 0x02, 0x03, 0x04 };

// our IP number
static const uint8_t my_ip[4] = { 192, 168, 100, 111 };


//*** SSAS drivers *****************************************************

ethernet_t *ethernet;


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
    ethernet = ethernet_open(ETHERNET_0);

    printf("SSAS ETHERNET ICMP echo ('ping') demonstration\n\n");

    printf("MAC: %02X-%02X-%02X-%02X-%02X-%02X\n", my_mac[0], my_mac[1], my_mac[2], my_mac[3], my_mac[4], my_mac[5]);
    printf("IP: %i.%i.%i.%i\n", my_ip[0], my_ip[1], my_ip[2], my_ip[3]);
    printf("(set MAC and IP in main.c)\n\n");

    ethernet_setmac(ethernet, my_mac);

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
        size = ethernet_receive(ethernet, ethbuf, ETHBUF_SIZE);

        if (size > 0)
        {
           if (size > ETHBUF_SIZE) size = ETHBUF_SIZE;

           size = eth_process(ethbuf, size);

           if (size > 0)
           {
               ethernet_send(ethernet, ethbuf, size);
           }
        }
        else if (size < 0)
        {
            printf("received frame with error or framebuffer overrun\n");
        }

    }
}


