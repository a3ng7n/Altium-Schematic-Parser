/*************************************************************************
**
**  VERSION CONTROL:    
**
**  IN PACKAGE:     Embedded TCPIP
**
**  COPYRIGHT:      Copyright (c) 2002 Altium
**
**  DESCRIPTION:    VHDL driver
**
**************************************************************************/

#include "vhdl_emac.h"

#include "tcpipset.h"
#include "../common/ethernet.h"

#include "../common/drivers/eth_driver.h"

#include <string.h>

//**************************************************************************

#if !defined(OPTION_ETHERNET)
#ifndef TCPIPDEBUG
#error OPTION_ETHERNET not defined, but eth_smsc91.c is included in project
#endif
#endif

#if defined(OPTION_ETHERNET)

//**************************************************************************

#if DEBUG_ETH >= 1
#define debug_eth1(x) (x)
#else
#define debug_eth1(x) {}
#endif
#if DEBUG_ETH >= 2
#define debug_eth2(x) (x)
#else
#define debug_eth2(x) {}
#endif
#if DEBUG_ETH >= 3
#define debug_eth3(x) (x)
#else
#define debug_eth3(x) {}
#endif


/*****************************************************************************\
 *  FUNCTION:       eth_drv_init
 *
 *  Initialize the ethernet chip.
 */
void eth_drv_init(void)
{
    // write MAC address into the Individual Address Registers
    *( __xdata unsigned char *)(EMAC_MAC_ADDR) = tcpip_settings.mac[0];
    *( __xdata unsigned char *)(EMAC_MAC_ADDR+1) = tcpip_settings.mac[1];
    *( __xdata unsigned char *)(EMAC_MAC_ADDR+2) = tcpip_settings.mac[2];
    *( __xdata unsigned char *)(EMAC_MAC_ADDR+3) = tcpip_settings.mac[3];
    *( __xdata unsigned char *)(EMAC_MAC_ADDR+4) = tcpip_settings.mac[4];
    *( __xdata unsigned char *)(EMAC_MAC_ADDR+5) = tcpip_settings.mac[5];

    // turn interrupts on
    EMAC_INT = 7;

    // turn receiver on
    EMAC_COMMAND = 0x02;
}


/*****************************************************************************\
 *  FUNCTION:       eth_drv_send
 *  Send data through the ethernet chip.
 *
 *  Uses global buffer ethbuf_aligned, returns 0 when OK, <> 0 otherwise
 */
Uint8 eth_drv_send(Uint16 data_length)
{
    debug_eth2(printf("ETH: sending %u bytes\n", data_length));

    if (!(EMAC_STATUS & 0x01))
    {
        debug_eth2(printf("ETH: busy, cannot send\n"));
        return -1;
    }

    // copy global buffer to vhdl
    memcpy(EMAC_BUF, ethbuf_aligned, data_length);

    // start vhdl transmission
    EMAC_MESS_LEN = data_length;
    EMAC_COMMAND = 0x01;


    return 0;
}


/*****************************************************************************\
 *  FUNCTION:       eth_drv_recv
 *  Receive data from the ethernet chip.
 *
 *  Uses global buffer ethbuf_aligned
 *  returns length of data received, 0 id nothing received, 0xFFFF when error
 */
Uint16 eth_drv_recv(void)
{
    unsigned int len;

    if (!(EMAC_STATUS & 0x02))
    {
        return 0;
    }

    // copy vhdl to global buffer
    len = EMAC_MESS_LEN;

    memcpy(ethbuf_aligned, EMAC_BUF, len);
    EMAC_COMMAND = 0x02;

    // return message length
    return len;
}

//**************************************************************************

#endif


