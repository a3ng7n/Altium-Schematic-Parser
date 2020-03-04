/*
 * Copyright (c) 2001-2003 Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * This file is part of the lwIP TCP/IP stack.
 *
 * Author: Adam Dunkels <adam@sics.se>
 *
 */
#ifndef __LWIPOPTS_USER_H__
#define __LWIPOPTS_USER_H__

//#define LWIP_RAW                        1

#ifdef NDEBUG
#define LWIP_NOASSERT 1
#endif /* NDEBUG */

/* ---------- NETIF options ---------- */

/**  * LWIP_NETIF_STATUS_CALLBACK==1: Support a callback function whenever an interface
* changes its up/down status (i.e., due to DHCP IP acquistion)  */
#define LWIP_NETIF_STATUS_CALLBACK      1

/**  * LWIP_NETIF_LINK_CALLBACK==1: Support a callback function from an interface
* whenever the link changes (i.e., link down)  */
#define LWIP_NETIF_LINK_CALLBACK        1

#define mem_realloc(mem, size)  (mem)

void *memcpyl( void * restrict s, const void * restrict ct, size_t n );
#define MEMCPY(dst,src,len)     memcpyl(dst,src,len)
//#define LWIP_ERROR(a,b,c)

/* ---------- Memory options ---------- */
/* MEM_ALIGNMENT: should be set to the alignment of the CPU for which
lwIP is compiled. 4 byte alignment -> define MEM_ALIGNMENT to 4, 2
byte alignment -> define MEM_ALIGNMENT to 2. */
#define MEM_ALIGNMENT           8

/* MEM_SIZE: the size of the heap memory. If the application will send
a lot of data that needs to be copied, this should be set high. */
#define MEM_SIZE                (64 * 1024)

/* MEMP_NUM_PBUF: the number of memp struct pbufs. If the application
sends a lot of data out of ROM (or other static memory), this    should be set high. */
#define MEMP_NUM_PBUF           32
/* MEMP_NUM_UDP_PCB: the number of UDP protocol control blocks. One
per active UDP "connection". */
#define MEMP_NUM_UDP_PCB        4
/* MEMP_NUM_TCP_PCB: the number of simulatenously active TCP
connections. */
#define MEMP_NUM_TCP_PCB        5
/* MEMP_NUM_TCP_PCB_LISTEN: the number of listening TCP
connections. */
#define MEMP_NUM_TCP_PCB_LISTEN 8
/* MEMP_NUM_TCP_SEG: the number of simultaneously queued TCP
segments. */
#define MEMP_NUM_TCP_SEG        128


/* The following four are used only with the sequential API and can be
set to 0 if the application only will use the raw API. */
/* MEMP_NUM_NETBUF: the number of struct netbufs. */
#define MEMP_NUM_NETBUF         2
/* MEMP_NUM_NETCONN: the number of struct netconns. */
#define MEMP_NUM_NETCONN        4
/* MEMP_NUM_TCPIP_MSG: the number of struct tcpip_msg, which is used
 for sequential API communication and incoming packets. Used in
 src/api/tcpip.c. */
#define MEMP_NUM_TCPIP_MSG_API   8
#define MEMP_NUM_TCPIP_MSG_INPKT 8

/* These two control is reclaimer functions should be compiled
in. Should always be turned on (1). */
#define MEM_RECLAIM             1
#define MEMP_RECLAIM            1

/* ---------- Pbuf options ---------- */
/* PBUF_POOL_SIZE: the number of buffers in the pbuf pool. */
#define PBUF_POOL_SIZE          50


/* PBUF_POOL_BUFSIZE: the size of each pbuf in the pbuf pool. */
#define PBUF_POOL_BUFSIZE       512

/* PBUF_LINK_HLEN: the number of bytes that should be allocated for a
link level header. */
#define PBUF_LINK_HLEN          16

/* ---------- TCP options ---------- */
#define LWIP_TCP                1
#define TCP_TTL                 255

/* Controls if TCP should queue segments that arrive out of
order. Define to 0 if your device is low on memory. */
#define TCP_QUEUE_OOSEQ         0

/* TCP Maximum segment size. */
#define TCP_MSS                 1460

/* TCP sender buffer space (bytes). */
#define TCP_SND_BUF             (16 * TCP_MSS)

/* TCP sender buffer space (pbufs). This must be at least = 2 *
TCP_SND_BUF/TCP_MSS for things to work. */
#define TCP_SND_QUEUELEN        (4 * TCP_SND_BUF/TCP_MSS)

/* TCP receive window. */
#define TCP_WND                 (16 * 1024)

/* Maximum number of retransmissions of data segments. */
#define TCP_MAXRTX              12

/* Maximum number of retransmissions of SYN segments. */
#define TCP_SYNMAXRTX           4

/* ---------- ARP options ---------- */
#define ARP_TABLE_SIZE 10
#define ARP_QUEUEING 1

/* ---------- IP options ---------- */
/* Define IP_FORWARD to 1 if you wish to have the ability to forward
IP packets across network interfaces. If you are going to run lwIP
on a device with only one network interface, define this to 0. */
#define IP_FORWARD              1

/* If defined to 1, IP options are allowed (but not parsed). If
defined to 0, all packets with IP options are dropped. */
#define IP_OPTIONS              1

/* ---------- ICMP options ---------- */
#define ICMP_TTL                255


/* ---------- DHCP options ---------- */

/* LWIP_DHCP is set by a plugin option */

/* 1 if you want to do an ARP check on the offered address (recommended). */
#define DHCP_DOES_ARP_CHECK     LWIP_DHCP

/* 1 if you want to specify a hostname for the interface */
// #define LWIP_NETIF_HOSTNAME 1 not working yet
/* ---------- DNS options ---------- */

/* LWIP_DNS is set by a plugin option */

/* MEMP_NUM_SYS_TIMEOUT: the number of simulateously active timeouts. */
#if LWIP_DHCP && LWIP_DNS
#define MEMP_NUM_SYS_TIMEOUT 6
#elif LWIP_DHCP
#define MEMP_NUM_SYS_TIMEOUT 5
#elif LWIP_DNS
#define MEMP_NUM_SYS_TIMEOUT 4
#else
#define MEMP_NUM_SYS_TIMEOUT 3
#endif

/* ---------- UDP options ---------- */
#define LWIP_UDP                1
#define UDP_TTL                 255


/* ---------- Statistics options ---------- */
/*#define LWIP_STATS 1*/

#ifdef LWIP_STATS
#define LINK_STATS 1
#define IP_STATS 1
#define ICMP_STATS 1
#define UDP_STATS 1
#define TCP_STATS 1
#define MEM_STATS 1
#define MEMP_STATS 1
#define PBUF_STATS 1
#define SYS_STATS 1
#else
#define LINK_STATS 0
#define IP_STATS 0
#define ICMP_STATS 0
#define UDP_STATS 0
#define TCP_STATS 0
#define MEM_STATS 0
#define MEMP_STATS 0
#define PBUF_STATS 0
#define SYS_STATS 0
#endif /* STATS */


//#undef LWIP_DEBUG

/**
* LWIP_DBG_MIN_LEVEL: After masking, the value of the debug is
* compared against this value. If it is smaller, then debugging
* messages are written.  */
#ifndef LWIP_DBG_MIN_LEVEL
//#define LWIP_DBG_MIN_LEVEL              LWIP_DBG_LEVEL_WARNING
#define LWIP_DBG_MIN_LEVEL              LWIP_DBG_LEVEL_OFF
#endif

/**  * LWIP_DBG_TYPES_ON: A mask that can be used to globally enable/disable
* debug messages of certain types.  */
#ifndef LWIP_DBG_TYPES_ON
//#define LWIP_DBG_TYPES_ON               (~LWIP_DBG_TRACE)
#define LWIP_DBG_TYPES_ON               (~LWIP_DBG_TRACE)
#endif

/**  * ETHARP_DEBUG: Enable debugging in etharp.c.  */
#define ETHARP_DEBUG                    LWIP_DBG_OFF

/**  * NETIF_DEBUG: Enable debugging in netif.c.  */
#define NETIF_DEBUG                     LWIP_DBG_OFF

/**  * PBUF_DEBUG: Enable debugging in pbuf.c.  */
#define PBUF_DEBUG                      LWIP_DBG_OFF

/**  * API_LIB_DEBUG: Enable debugging in api_lib.c.  */
#define API_LIB_DEBUG                   LWIP_DBG_OFF

/**  * API_MSG_DEBUG: Enable debugging in api_msg.c.  */
#define API_MSG_DEBUG                   LWIP_DBG_OFF

/**  * SOCKETS_DEBUG: Enable debugging in sockets.c.  */
#define SOCKETS_DEBUG                   LWIP_DBG_OFF

/**  * ICMP_DEBUG: Enable debugging in icmp.c.  */
#define ICMP_DEBUG                      LWIP_DBG_OFF

/**  * IGMP_DEBUG: Enable debugging in igmp.c.  */
#define IGMP_DEBUG                      LWIP_DBG_OFF

/**  * INET_DEBUG: Enable debugging in inet.c.  */
#define INET_DEBUG                      LWIP_DBG_OFF

/**  * IP_DEBUG: Enable debugging for IP.  */
#define IP_DEBUG                        LWIP_DBG_OFF

/**  * IP_REASS_DEBUG: Enable debugging in ip_frag.c for both frag & reass.  */
#define IP_REASS_DEBUG                  LWIP_DBG_OFF

/**  * RAW_DEBUG: Enable debugging in raw.c.  */
#define RAW_DEBUG                       LWIP_DBG_OFF

/**  * MEM_DEBUG: Enable debugging in mem.c.  */
#define MEM_DEBUG                       LWIP_DBG_OFF

/**  * MEMP_DEBUG: Enable debugging in memp.c.  */
#define MEMP_DEBUG                      LWIP_DBG_OFF

/**  * SYS_DEBUG: Enable debugging in sys.c.  */
#define SYS_DEBUG                       LWIP_DBG_OFF

/**  * TCP_DEBUG: Enable debugging for TCP.  */
#define TCP_DEBUG                       LWIP_DBG_OFF

/**  * TCP_INPUT_DEBUG: Enable debugging in tcp_in.c for incoming debug.  */
#define TCP_INPUT_DEBUG                 LWIP_DBG_OFF

/**  * TCP_FR_DEBUG: Enable debugging in tcp_in.c for fast retransmit.  */
#define TCP_FR_DEBUG                    LWIP_DBG_OFF

/**  * TCP_RTO_DEBUG: Enable debugging in TCP for retransmit  * timeout.  */
#define TCP_RTO_DEBUG                   LWIP_DBG_OFF

/**  * TCP_CWND_DEBUG: Enable debugging for TCP congestion window.  */
#define TCP_CWND_DEBUG                  LWIP_DBG_OFF

/**  * TCP_WND_DEBUG: Enable debugging in tcp_in.c for window updating.  */
#define TCP_WND_DEBUG                   LWIP_DBG_OFF

/**  * TCP_OUTPUT_DEBUG: Enable debugging in tcp_out.c output functions.  */
#define TCP_OUTPUT_DEBUG                LWIP_DBG_OFF

/**  * TCP_RST_DEBUG: Enable debugging for TCP with the RST message.  */
#define TCP_RST_DEBUG                   LWIP_DBG_OFF

/**  * TCP_QLEN_DEBUG: Enable debugging for TCP queue lengths.  */
#define TCP_QLEN_DEBUG                  LWIP_DBG_OFF

/**  * UDP_DEBUG: Enable debugging in UDP.  */
#define UDP_DEBUG                       LWIP_DBG_OFF

/**  * TCPIP_DEBUG: Enable debugging in tcpip.c.  */
#define TCPIP_DEBUG                     LWIP_DBG_OFF

/**  * PPP_DEBUG: Enable debugging for PPP.  */
#define PPP_DEBUG                       LWIP_DBG_OFF

/**  * SLIP_DEBUG: Enable debugging in slipif.c.  */
#define SLIP_DEBUG                      LWIP_DBG_OFF

/**  * DHCP_DEBUG: Enable debugging in dhcp.c.  */
#define DHCP_DEBUG                      LWIP_DBG_OFF

/**  * AUTOIP_DEBUG: Enable debugging in autoip.c.  */
#define AUTOIP_DEBUG                    LWIP_DBG_OFF

/**  * SNMP_MSG_DEBUG: Enable debugging for SNMP messages.  */
#define SNMP_MSG_DEBUG                  LWIP_DBG_OFF

/**  * SNMP_MIB_DEBUG: Enable debugging for SNMP MIBs.  */
#define SNMP_MIB_DEBUG                  LWIP_DBG_OFF

/**  * DNS_DEBUG: Enable debugging for DNS.  */
#define DNS_DEBUG                       LWIP_DBG_OFF


#endif /* __LWIPOPTS_USER_H__ */
