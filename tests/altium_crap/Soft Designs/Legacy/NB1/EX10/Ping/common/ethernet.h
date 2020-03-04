/*************************************************************************
**
**  VERSION CONTROL:	@(#)ethernet.h	1.2	03/10/31
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	ETHERNET / ARP related functions
**
**************************************************************************/

#ifndef _ETHERNET_H_
#define _ETHERNET_H_

#include "tcpipset.h"
#include "common/tcpip.h"

#if defined(OPTION_ETHERNET)

//**************************************************************************

#define IP_MAXLEN		1500
#define ETH_BUFLEN		(IP_MAXLEN + ETHHEADER_LEN)

#define ARP_HARDWARE	0x0001
#define ARP_REQUEST		0x01
#define ARP_REPLY		0x02

#define ETH_TYPE_ARP	0x0806
#define ETH_TYPE_IP		0x0800


//**************************************************************************

// no special aligned versions are needed,
// all bounderies are Uint16-aligned by themselves

typedef struct
{
    Uint8 destmac[6];
    Uint8 sourcemac[6];
    Uint16 type;
}
ETHHEADER;
#define ETHHEADER_LEN  sizeof(ETHHEADER)
#define ETH ((ETHHEADER*) ethbuf_aligned)
#define ETHDATA ((char*) ethbuf_aligned + ETHHEADER_LEN)



typedef struct
{
    Uint16 hardware;
    Uint16 protocol;
    Uint8 hardwarelength;
    Uint8 protocollength;
    Uint16 operation;
    Uint8 senderhardware[6];
    Uint8 senderprotocol[4];
    Uint8 targethardware[6];
    Uint8 targetprotocol[4];
}
ARPFRAME;
#define ARPFRAME_LEN  sizeof(ARPFRAME)
#define ARP ((ARPFRAME*) ETHDATA)

typedef struct
{
    Uint8 ip[4];
    Uint8 mac[6];
}
ARPCACHE;

//**************************************************************************

extern Uint16 ethbuf_aligned[];
#define IPBUF ETHDATA

extern void tcpipstack_init(void);
extern Uint8 tcpipstack_main(void);
extern void tcpipstack_timertick(void);

extern void eth_init(void);
extern Uint8 eth_action(void);

extern Uint8 *arp_resolve(Uint8 * ip);
extern void arp_store_sender(Uint8 * ip);
extern void arp_send_request(Uint8 * ip);

extern char *framebuf_init_ip(void);
extern void framebuf_send(Uint16 len);

extern Uint8 *memcchr(Uint8 *s, Sint16 c, Uint16 n);

//**************************************************************************

#endif
#endif
