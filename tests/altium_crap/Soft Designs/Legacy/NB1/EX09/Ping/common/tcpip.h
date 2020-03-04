/*************************************************************************
**
**  VERSION CONTROL:	@(#)tcpip.h	1.3	03/10/31
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	TCP, UDP, ICMP, IP related functions
**
**************************************************************************/

#ifndef _TCPIP_H_
#define _TCPIP_H_

#include "tcpipset.h"
#include "common/tcpip_global.h"

// transport layer exports the global buffer
#ifdef OPTION_ETHERNET
#include "common/ethernet.h"
#else
#include "common/serial.h"
#endif

//**************************************************************************

// supported IP version is only IP4 for now
#define IP_VERSION_IP4  	4

// supported protocols on top of IP
#define IP_PROTOCOL_ICMP	1
#define IP_PROTOCOL_UDP		17
#define IP_PROTOCOL_TCP		6

// IP bitmask fragmentation flags
// (not supported, only used when debugging to check we don't get any...)
#define IP_FLAG_DF		0x4000
#define IP_FLAG_MF		0x8000

// ICMP type & codes for supported mesages
#define ICMP_TYPE_ECHOREPLY	0x00
#define ICMP_TYPE_UNREACH	0x03
#define ICMP_TYPE_ECHO		0x08
#define ICMP_CODE_UNREACH_PORT	0x03

// TCP option for receiving window size (only option we need)
#define TCP_RECVWIN_OPTION	 2
#define TCP_RECVWIN_OPTION_LEN   4

#define TCP_DATA_MAXLEN	    (IP_MAXLEN - sizeof(IPHEADER) - sizeof(TCPHEADER))

//**************************************************************************

typedef struct
{
    Uint8 version;
    Uint8 service;
    Uint16 packetlength;
    Uint16 ident;
    Uint16 offset;
    Uint8 timetolive;
    Uint8 protocol;
    Uint16 checksum;
    Uint32 sourceip;
    Uint32 destip;
}
IPHEADER;
#define IPHEADER_LEN  sizeof(IPHEADER)
#define IP ((IPHEADER*) IPBUF)
#define IPDATA ((char*) IPBUF + IPHEADER_LEN)

typedef struct
{
    Uint8 type;
    Uint8 code;
    Uint16 checksum;
    Uint16 ident;
    Uint16 sequence;
}
ICMPHEADER;
#define ICMPHEADER_LEN  sizeof(ICMPHEADER)
#define ICMP ((ICMPHEADER*) (IPBUF + IPHEADER_LEN))
#define ICMPDATA ((char*) (IPBUF + IPHEADER_LEN + ICMPHEADER_LEN))

typedef struct
{
    Uint16 sourceport;
    Uint16 destport;
    Uint32 sequence;
    Uint32 acknowledge;
    Uint8 headerinfo;
    Uint8 flags;
    Uint16 window;
    Uint16 checksum;
    Uint16 urgent;
}
TCPHEADER;
#define TCPHEADER_LEN  sizeof(TCPHEADER)
#define TCP ((TCPHEADER*) (IPBUF + IPHEADER_LEN))
#define TCPDATA ((char*) (IPBUF + IPHEADER_LEN + TCPHEADER_LEN))

typedef struct
{
    Uint16 sourceport;
    Uint16 destport;
    Uint16 length;
    Uint16 checksum;
}
UDPHEADER;
#define UDPHEADER_LEN  sizeof(UDPHEADER)
#define UDP ((UDPHEADER*) (IPBUF + IPHEADER_LEN))
#define UDPDATA ((char*) (IPBUF + IPHEADER_LEN + UDPHEADER_LEN))

//**************************************************************************

extern void tcpip_init(void);
extern void tcp_resetall(void);

extern void tcp_timertick(void);
extern Uint8 tcp_retries(void);

extern Uint16 ip_process(void);

#ifdef OPTION_IP_ROUTING
extern Uint16 ip_routing(void);
#endif

extern char *ip_create(Uint32 destip);

#ifdef OPTION_UDP
extern Uint16 udp_sendprep(Uint16 ip_datalength);
extern void udp_create(Uint16 sourceport, Uint16 destport);
#endif

#ifdef DEBUG_TCP
extern void tcp_sessionstatus(void);
#endif

//**************************************************************************

#endif
