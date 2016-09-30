/*************************************************************************
**
**  VERSION CONTROL:	@(#)ethernet.c	1.2	03/04/24
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	ETHERNET / ARP related functions
**
**************************************************************************/

#include <string.h>

#include "ethernet.h"
#include "drivers/eth_driver.h"

#ifdef OPTION_DHCP
#include "dhcp.h"
#endif
#if defined(OPTION_DNS_CLIENT) || defined(OPTION_DNS_SERVER)
#include "dns.h"
#endif

//**************************************************************************

#if !defined(OPTION_ETHERNET)
#ifndef TCPIPDEBUG
#error OPTION_ETHERNET not defined, but ethernet.c is included in project
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

//**************************************************************************

#ifndef ARP_CACHE_ENTRIES
#define ARP_CACHE_ENTRIES 8
#endif

// if over 256 the arptags mechanism breaks apart
#if ARP_CACHE_ENTRIES > 256
#error ARP_CACHE_ENTRIES maximal 256
#endif

//**************************************************************************

// dirty: HTTP needs to be sure it can cat a default filename to the buffer
#if defined(OPTION_HTTP) && defined(HTTP_FILESYS_DEFAULT) && defined(HTTP_DIRINDEX)
#define HTTP_SLACK sizeof(HTTP_DIRINDEX)
#else
#define HTTP_SLACK 0
#endif

Uint16 ethbuf_aligned[(ETH_BUFLEN + HTTP_SLACK) / 2];	// ETHERNET frame buffer

static ARPCACHE arpcache[ARP_CACHE_ENTRIES];	// cache for resolved IP/MAC combinations
static Uint8 arptags[ARP_CACHE_ENTRIES];	// lastused tags for arpcache

static void arp_store(Uint8 * ip, Uint8 * mac);
static void arp_process(void);
static void arp_tag(Uint8 nr);


/***************************************************************************
 * FUNCTION:	tcpipstack_init
 *
 * called one to initialize all modules,
 * global tcpip_settings must be filled beforehand
 */
void tcpipstack_init(void)
{
	eth_init();
#ifdef OPTION_DHCP
	dhcp_init();
#endif
	tcpip_init();
#if defined(OPTION_DNS_CLIENT) || defined(OPTION_DNS_SERVER)
	dns_init();
#endif
#ifdef OPTION_FTP_SERVER
	ftp_init();
#endif
#ifdef OPTION_TELNET_SERVER
	telnet_init();
#endif
}

/***************************************************************************
 * FUNCTION:	tcpipstack_main
 *
 * In case of using this linedriver this function must be called by the
 * user application on a regular basis. Returns 1 if any action was taken,
 * or 0 if none (can be used to sleep if multitasking)
 */
Uint8 tcpipstack_main(void)
{
	if (eth_action())
	{
	}
	else if (tcp_retries())
	{
	}
	else
#ifdef OPTION_DHCP
	if (dhcp_action())
	{
	}
	else
#endif
	{
		return 0;
	}

	return 1;
}

/***************************************************************************
 * FUNCTION:	tcpipstack_timertick
 *
 * In case of using this linedriver this function must be called every
 * retry-timer unit. Retry-values default in TCP module are
 * based on one tick each second.
 */
void tcpipstack_timertick(void)
{
	tcp_timertick();
#ifdef OPTION_DHCP
#ifndef DHCP_TIMERTICK_MANUAL
	dhcp_timertick();
#endif
#endif
}


/***************************************************************************
 * FUNCTION:	eth_init
 *
 * Initialize the ETHERNET module
 */
void eth_init(void)
{
	Uint8 i;

	memset(arpcache, 0, sizeof(ARPCACHE) * ARP_CACHE_ENTRIES);

	for (i = 0; i < ARP_CACHE_ENTRIES; ++i)
	{
		arptags[i] = i;
	}

	eth_drv_init();
}


Uint8 eth_action(void)
{
	Uint16 len;

	// check if the driver has a new frame for us
	len = eth_drv_recv();

	if (len == (Uint16) - 1)
	{
		// error in receiver
		// TODO: what?
		return 0;
	}

	if (!len)
	{
		return 0;
	}

	switch (ntohw(ETH->type))
	{
	case ETH_TYPE_ARP:
		arp_process();
		return 1;

	case ETH_TYPE_IP:
		// we only accept our own IP...
		if ((memcmp(&IP->destip, tcpip_settings.self_ip, 4) == 0)
#ifdef OPTION_DHCP
		    // if DHCP didn't lease an IP number for us yet we accept
		    // non-broadcast DHCP_CLIENT UDP-port packets as an exception to the rule
		    // (we brutally peek inside the packet to verify the IP protocol and UDP destport)
		    || ((memcchr(tcpip_settings.self_ip, 0, 4) == NULL) && memcchr(ETH->destmac, 0xFF, 6) &&
			(IP->protocol = IP_PROTOCOL_UDP) && (ntohw(UDP->destport) == UDP_PORT_DHCP_CLIENT))
#endif
			)
		{
			// store incoming IP/MAC in arpcache
			debug_eth2(printf("store mac for incoming ip\n"));
			arp_store((Uint8 *) & IP->sourceip, ETH->sourcemac);

			// process IP packet, returns length of optional reply
			len = ip_process();
			if (len)
			{
				// transmit the reply
				framebuf_send(len);
			}
		}
		else
		{
			// IP number not for us, just drop
			debug_eth1(printf
				   ("dropped incoming for ip %u.%u.%u.%u\n", ((Uint8 *) & IP->destip)[0],
				    ((Uint8 *) & IP->destip)[1], ((Uint8 *) & IP->destip)[2],
				    ((Uint8 *) & IP->destip)[3]));
		}
		return 1;

	default:
		// unsupported frametype, just drop
		debug_eth2(printf("dropped unsupported frametype %04x\n", ntohw(ETH->type)));
		break;
	}

	return 0;
}


static void arp_process(void)
{
	// we only support normal requests and answers
	if ((ARP->operation != htonw(ARP_REQUEST)) && (ARP->operation != htonw(ARP_REPLY)))
	{
		debug_eth1(printf("ARP received unsupported operation %04X at %u\n", ntohw(ARP->operation)));
		return;
	}

	if (memcchr(ARP->targetprotocol, 0, 4) == NULL)
	{
		debug_eth1(printf("ARP received invalid request for 0.0.0.0\n"));
		return;
	}

	// we check the ip-number to be verified first,
	// biggest change this doesn't match and the sooner we are finished
	if (memcmp(ARP->targetprotocol, tcpip_settings.self_ip, 4) == 0)
	{
		// ok
	}
	else
	{
		debug_eth2(printf
			   ("ARP received request for unknown ip %u.%u.%u.%u\n", ARP->targetprotocol[0],
			    ARP->targetprotocol[1], ARP->targetprotocol[2], ARP->targetprotocol[3]));
		return;
	}

	// check if all non-address stuff matches
	if ((ARP->hardware != htonw(ARP_HARDWARE))
	    || (ARP->protocol != htonw(ETH_TYPE_IP)) || (ARP->hardwarelength != 0x06) || (ARP->protocollength != 0x04))
	{
		debug_eth1(printf("ARP received error in hardware/protocol/lengths\n"));
		return;
	}

	// all's well, let's process

	switch (ARP->operation)
	{
	case htonw(ARP_REQUEST):
		debug_eth1(printf("ARP received request, send reply\n"));

		// send a reply
		memcpy(ETH->destmac, ARP->senderhardware, 6);
		memcpy(ETH->sourcemac, tcpip_settings.mac, 6);
		ETH->type = htonw(ETH_TYPE_ARP);

		ARP->operation = htonw(ARP_REPLY);

		// swap request/answerdata, and use our own mac for the requested hardware address
		memcpy(ARP->targethardware, ARP->senderhardware, 6);
		memcpy(ARP->senderhardware, tcpip_settings.mac, 6);
		swapdw(*(Uint32 *) ARP->targetprotocol, *(Uint32 *) ARP->senderprotocol);

		eth_drv_send(ETHHEADER_LEN + ARPFRAME_LEN);

		break;

	case htonw(ARP_REPLY):
		debug_eth1(printf("ARP received reply\n"));
		// process the answer we requested
		arp_store(ARP->senderprotocol, ARP->senderhardware);
		break;

	}

	return;
}


static void arp_store(Uint8 * ip, Uint8 * mac)
{
	Uint8 i;
	ARPCACHE *arpentry;

	debug_eth2(printf
		   ("store ip=%u.%u.%u.%u mac=%u.%u.%u.%u.%u.%u\n", ip[0], ip[1], ip[2], ip[3], mac[0], mac[1], mac[2],
		    mac[3], mac[4], mac[5]));

	for (i = 0, arpentry = arpcache; i < ARP_CACHE_ENTRIES; ++i, ++arpentry)
	{
		if (memcmp(arpentry->ip, ip, 4) == 0)
		{
			// already in cache, update mac
			debug_eth2(printf("found in cache, updated\n"));
			memcpy(arpentry->mac, mac, 6);
			arp_tag(i);
			return;
		}
	}

	// overwrite last-used entry
	debug_eth2(printf("not found in cache, inserted as %u\n", arptags[0]));
	arpentry = arpcache + arptags[0];
	memcpy(arpentry->ip, ip, 4);
	memcpy(arpentry->mac, mac, 6);
	arp_tag(arptags[0]);
}


static void arp_tag(Uint8 nr)
{
	Uint8 *pos;
	Uint8 i;

	debug_eth3(printf("tag %u\n", nr));

	debug_eth3(printf("pre: "));
	for (i = 0; i < ARP_CACHE_ENTRIES; ++i)
	{
		debug_eth3(printf("%u ", arptags[i]));
	}
	debug_eth3(printf("\n"));

	if ((pos = memchr(arptags, nr, ARP_CACHE_ENTRIES)) == NULL)
	{
		// in this case memchr could never return 0, this just protects us against our own errors
		pos = arptags;
	}

	// slide right-part of the list to move over this entry...
	memcpy(pos, pos + 1, ARP_CACHE_ENTRIES - (pos - arptags) - 1);

	// ...and put the entry at the right end of the list
	arptags[ARP_CACHE_ENTRIES - 1] = nr;
	debug_eth3(printf("post: "));
	for (i = 0; i < ARP_CACHE_ENTRIES; ++i)
	{
		debug_eth3(printf("%u ", arptags[i]));
	}
	debug_eth3(printf("\n"));
}


Uint8 *arp_resolve(Uint8 * ip)
{
	Uint8 i;
	ARPCACHE *arpentry;

	for (i = 0, arpentry = arpcache; i < ARP_CACHE_ENTRIES; ++i, ++arpentry)
	{
		if (memcmp(arpentry->ip, ip, 4) == 0)
		{
			return arpentry->mac;
		}
	}

	return NULL;
}


void arp_send_request(Uint8 * ip)
{
	debug_eth1(printf("ARP send request for %u.%u.%u.%u\n", ip[0], ip[1], ip[2], ip[3]));

	// broadcast ethernet frame
	memset(ETH->destmac, 0xFF, 6);
	memcpy(ETH->sourcemac, tcpip_settings.mac, 6);
	ETH->type = htonw(ETH_TYPE_ARP);

	ARP->hardware = htonw(ARP_HARDWARE);
	ARP->protocol = htonw(ETH_TYPE_IP);
	ARP->hardwarelength = 0x06;
	ARP->protocollength = 0x04;
	ARP->operation = htonw(ARP_REQUEST);
	memcpy(ARP->senderhardware, tcpip_settings.mac, 6);
	memcpy(ARP->senderprotocol, tcpip_settings.self_ip, 4);

	memset(ARP->targethardware, 0, 6);
	memcpy(ARP->targetprotocol, ip, 4);

	eth_drv_send(ETHHEADER_LEN + ARPFRAME_LEN);
}


char *framebuf_init_ip(void)
{
	return IPBUF;
}


void framebuf_send(Uint16 len)
{
	Uint8 *mac;
	Uint8 *ip;

	ip = (Uint8 *) & IP->destip;
	if (memcchr(ip, 0xFF, 4) == NULL)
	{
		// IP 255.255.255.255 is turned into broadcast
		memset(ETH->destmac, 0xFF, 6);
	}
	else
	{
		if ((mac = arp_resolve((Uint8 *) & IP->destip)) == NULL)
		{
			// should not have happened, the user is reponsible the MAC is availabe, we can only discard now
			// other cause could be overflow of the ARP cache, most likely
			// cause by an intentional attack
			debug_global(printf
				     ("ARP error: unresolvable IP address %u.%u.%u.%u\n", ip[0], ip[1], ip[2], ip[3]));
			return;
		}
		memcpy(ETH->destmac, mac, 6);
	}

	memcpy(ETH->sourcemac, tcpip_settings.mac, 6);
	ETH->type = htonw(ETH_TYPE_IP);

	eth_drv_send(ETHHEADER_LEN + len);
}


/***************************************************************************
 * FUNCTION:	memcchr
 *
 * inverse of ANSI memchr (checks for first _not_ equal to c)
 */
Uint8 *memcchr(Uint8 *s, Sint16 c, Uint16 n)
{
    for (; n; ++s, --n)
    {
        if (*s != c)
        { return s; }
    }

    return NULL;
}

//**************************************************************************

#endif
