/*************************************************************************
**
**  VERSION CONTROL:	@(#)tcpip.c	1.11	04/02/26
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	TCP, UDP, ICMP, IP related functions
**
**************************************************************************/

#include <string.h>
#include <ctype.h>

#include "tcpip.h"

//**************************************************************************

// portrange to use for sessions we initiate
#ifndef TCP_PORT_CLIENT_START
#define TCP_PORT_CLIENT_START	1000
#endif
#ifndef TCP_PORT_CLIENT_END
#define TCP_PORT_CLIENT_END	2000
#endif

// time before retrying TCP transmits
#ifndef TCP_RETRY_TICKS
#define TCP_RETRY_TICKS		5
#endif

// number of times to retry before resetting session
#ifndef TCP_RETRY_MAX
#define TCP_RETRY_MAX		3
#endif

// time we accept without any activity on a session before resetting
#ifndef TCP_WATCHDOG_TICKS
#define TCP_WATCHDOG_TICKS		300
#endif

// time we accept incoming packets on a closed link before returning a RESET
#ifndef TCP_LASTACK_TICKS
#define TCP_LASTACK_TICKS		(TCP_RETRY_TICKS * TCP_RETRY_MAX)
#endif

// used for each incoming/outgoing tcp connection, maximum 254 sessions,
// the cost is one TCP_SESSION structure per session
#ifndef TCP_SESSIONS
#define TCP_SESSIONS		16
#endif

//**************************************************************************

TCPIP_SETTINGS tcpip_settings;	// global settings for all tcpip stack modules

static Uint16 ip_ident_next;	// IP identifier to be used next
static Uint32 tcp_sequence_next;	// TCP sequence to be used next (for new sessions)

static Uint16 tcp_port_next;	// TCP next portnumber to be used (our side)

static TCP_SESSION tcp_sessions[TCP_SESSIONS];	// all concurrent sessions

//**************************************************************************

static Uint16 ip_sendprep(Uint16 ip_datalength);

static Uint16 tcp_process(Uint16 ip_datalength);
static Uint16 tcp_sendprep(TCP_SESSION * session, Uint8 optionslength, Uint16 datalength);

#ifdef OPTION_ICMP_ECHO
static Uint16 icmp_process(Uint16 ip_datalength);
#endif
static Uint16 icmp_sendprep(Uint16 ip_datalength);

#ifdef OPTION_UDP
static Uint16 udp_process(Uint16 ip_datalength);
#endif

static Uint16 std_checksum(Uint16 ip_datalength);
static Uint32 checksum(Uint8 *cdata, Sint16 length);
static Uint16 addcarry16(Uint32 sum);

//**************************************************************************

#if defined(DEBUG_IP) || defined(DEBUG_ICMP) || defined(DEBUG_UDP)  || defined(DEBUG_TCP)
#include "debug\tcpip_debug.c"
#endif

#ifdef DEBUG_IP
#define debug_ip(x) x
#else
#define debug_ip(x) {}
#endif
#ifdef DEBUG_ICMP
#define debug_icmp(x) x
#else
#define debug_icmp(x) {}
#endif
#ifdef DEBUG_UDP
#define debug_udp(x) x
#else
#define debug_udp(x) {}
#endif
#ifdef DEBUG_TCP
#define debug_tcp(x) x
#else
#define debug_tcp(x) {}
#endif

/***************************************************************************
 * FUNCTION:	tcpip_init
 *
 * Initialize the TCP/UDP/IP modules
 */
void tcpip_init(void)
{
    Uint8 i;
    TCP_SESSION *session;

    // initialize all static number generators
    tcp_sequence_next = 1;
    ip_ident_next = 0;
    tcp_port_next = TCP_PORT_CLIENT_START;

    // mark all TCP sessions as closed
    for (i = 0, session = tcp_sessions; i < TCP_SESSIONS; ++i, ++session)
    {
	session->state = TCP_STATE_CLOSED;
	session->retries = 0;
	session->wdtimer = 0;
    }
}


/***************************************************************************
 * FUNCTION:	tcp_resetall
 *
 * Reset all tcpip-sessions
 */
void tcp_resetall(void)
{
    Uint8 i;
    TCP_SESSION *session;

    for (i = 0, session = tcp_sessions; i < TCP_SESSIONS; ++i, ++session)
    {
	if ((session->state != TCP_STATE_CLOSED) && (session->state != TCP_STATE_LASTACK))
	{
	    session->state = TCP_STATE_RESET;
	    session->retries = -1;
	    session->timer = 0;
	}
    }
}

/***************************************************************************
 * FUNCTION:	ip_process
 *
 * Process the incoming IP packet present in the buffer indicated by
 * IPBUF, depending on the protocol the packet is dispatched to
 * the relevant ***_process function
 */
Uint16 ip_process(void)
{
    Uint16 ip_datalength;

#ifdef OPTION_IP_ROUTING
    if (memcmp(&IP->destip, tcpip_settings.self_ip, 4))
    {
        return ip_routing();
    }
#endif

    debug_ip(ip_show(s("received")));

    if ((IP->version >> 4) != IP_VERSION_IP4)
    {
	debug_ip(printf(s("IP VERSION UNKNOWN\n")));
	return 0;
    }

    if (addcarry16(checksum((Uint8*) IPBUF, IPHEADER_LEN)))
    {
	debug_ip(printf(s("IP CHECKSUM ERROR\n")));
	return 0;
    }

    // IP number is not checked, often (with PPP connections) we presume
    // only relevant traffic is coming our way, or we serve different ports on
    // different IP addresses (think HTML server combined with a DNS server).
    // in which case the servers/clients aplpications should verify the IP number.
    // In case of ethernet the driver itself should filter all traffic to
    // minimize systemload.

    ip_datalength = ntohw(IP->packetlength) - IPHEADER_LEN;

    switch (IP->protocol)
    {
#ifdef OPTION_ICMP_ECHO
    case IP_PROTOCOL_ICMP:
	return icmp_process(ip_datalength);
#endif

#ifdef OPTION_UDP
    case IP_PROTOCOL_UDP:
	return udp_process(ip_datalength);
#endif

    case IP_PROTOCOL_TCP:
	return tcp_process(ip_datalength);

    }

    // unknown protocol, don't respond
    return 0;
}


#ifdef OPTION_ICMP_ECHO
/***************************************************************************
 * FUNCTION:	icmp_process
 *
 * Process incoming ICMP packet present in the buffer indicated by IPBUF
 * for length ip_datalength
 * Only replies on PING requests, other packets are dropped
 */
static Uint16 icmp_process(Uint16 ip_datalength)
{
    debug_icmp(icmp_show(s("received")));

    // verify checksum
    if (addcarry16(checksum((Uint8*) IPDATA, ip_datalength)))
    {
	debug_icmp(printf(s("ICMP CHECKSUM ERROR\n")));
	return 0;
    }

    // process depending on ICMP type & code
    if ((ICMP->type == ICMP_TYPE_ECHO) && (ICMP->code == 0))
    {
	// ping packet, change it into it's own reply
	ICMP->type = ICMP_TYPE_ECHOREPLY;
	ICMP->code = 0;
	return icmp_sendprep(ip_datalength);
    }

    // unsupported type and code, ignore
    return 0;
}
#endif // OPTION_ICMP_ECHO


#ifdef OPTION_UDP
/***************************************************************************
 * FUNCTION:	udp_process
 *
 * Process incoming UDP packet present in the buffer indicated by IPBUF
 * for length ip_datalength.
 * If the packet is for a UDP-port defined in the udp_clientservers list,
 * it is send to the application belonging to that port, otherwise the
 * packet is dropped.
 */
static Uint16 udp_process(Uint16 ip_datalength)
{
    Uint16 port;
    UDP_CLIENTSERVER *clientserver;

    debug_udp(udp_show(s("received")));

    // verify checksum
    if (std_checksum(ip_datalength))
    {
	debug_udp(printf(s("UDP CHECKSUM ERROR\n")));
	return 0;
    }

    port = ntohw(UDP->destport);

    // check if belongs to known port of client or server
    for (clientserver = tcpip_settings.udpclientservers; clientserver->clientserverfunction; ++clientserver)
    {
	if (clientserver->port == port)
	{
	    return clientserver->clientserverfunction(port, ip_datalength);
	}
    }

    // just ignore unknown portnumbers
    return 0;
}
#endif // OPTION_UDP


/***************************************************************************
 * FUNCTION:	tcp_timertick
 *
 * Decrement all non-zero retry-timers of all TCP sessions.
 * This function should be called every time-unit, the TCP_RETRY_TICKS
 * defines how many time-units before a resend takes place whenever a
 * send packed has not been acknowledged.
 * Suitable time-unit is 1 second, with a TCP_RETRY_TICKS of 10 for a
 * 38kbaud serial link.
 */
void tcp_timertick(void)
{
    Uint8 i;
    TCP_SESSION *session;

    for (i = 0, session = tcp_sessions; i < TCP_SESSIONS; ++i, ++session)
    {
	if (session->retries && session->timer)
	{
	    --session->timer;
	}

	if (session->wdtimer)
	{
	    if (--session->wdtimer == 0)
	    {
		session->state = TCP_STATE_RESET;
		session->retries = -1;
		session->timer = 0;
	    }
	}
    }
}


#ifdef DEBUG_TCP
void tcp_sessionstatus(void)
{
    Uint8 i;
    TCP_SESSION *session;

    printf(s("sessions (state/retries/timer/watchdog): "));
    for (i = 0, session = tcp_sessions; i < TCP_SESSIONS; ++i, ++session)
    {
	printf(s("%u/%i/%u/%u  "), session->state, session->retries, session->timer, session->wdtimer);
    }
    printf(s("\n"));
}
#endif


/***************************************************************************
 * FUNCTION:	tcp_retries
 *
 * Check all sessions if we have to resend any packet
 * NB: with each call to this function one session is checked
 * function returns 1 if a resend was generated, 0 if not
 */
Uint8 tcp_retries(void)
{
    static Uint8 nr = 0;
    TCP_SESSION *session;
    Uint8 tcp_optionlength;
    Uint16 tcp_datalength;

    // every call to this function we check one of our available sessions
    session = &tcp_sessions[nr];

    // check if the session is active and a retry is in order
    if (session->retries == -1) { session->timer = 0; }
    if ((session->state != TCP_STATE_CLOSED) && (session->retries != 0) && (session->timer == 0))
    {
	if (session->state == TCP_STATE_LASTACK)
	{
	    // not a real retry, but move this session out of LASTACK state
	    session->state = TCP_STATE_CLOSED;
	    session->retries = 0;
	}
	else
	{
	    // create a new IP/TCP header in the buffer for this session
	    if (!ip_create(session->remoteip))
	    {
		return 0;
	    }

	    IP->protocol = IP_PROTOCOL_TCP;

	    // source/dest are reversed as the tcp_sendprep swaps them
	    TCP->destport = htonw(session->localport);
	    TCP->sourceport = htonw(session->remoteport);

	    tcp_optionlength = 0;
	    tcp_datalength = 0;

	    if (session->state == TCP_STATE_RESET || ((session->retries != -1) && (session->retries > TCP_RETRY_MAX)))
	    {
		debug_tcp((session->state ==
			   TCP_STATE_RESET) ? ((session->wdtimer ==
						0) ? printf(s("TCP watchdog forced reset\n")) :
					       printf(s("TCP external source forced reset\n"))) :
			  printf(s("TCP max retries forced reset\n")));

		// reset session
		session->state = TCP_STATE_CLOSED;
		TCP->flags = TCP_FLAG_RESET;
		// inform application about reset
		// (TCP_STATE_CLOSED indicates we initiated the reset)
		session->application(0, session, 0, &(TCP->flags));
	    }
	    else
	    {
		debug_tcp((session->retries == -1) ?
			  printf(s("TCP external source triggered new transmission\n")) :
			  printf(s("TCP acknowledge timeout, retry %u\n"), session->retries));

		TCP->flags = 0;
		switch (session->state)
		{
		case TCP_STATE_SYNACK:
		    TCP->flags = TCP_FLAG_ACK;
		    // fallthrough to TCP_STATE_SYN

		case TCP_STATE_SYN:
		    TCP->flags |= TCP_FLAG_SYN;
		    // pass our receiving windowsize as an option
		    TCPDATA[0] = (char) TCP_RECVWIN_OPTION;
		    TCPDATA[1] = (char) TCP_RECVWIN_OPTION_LEN;
		    TCPDATA[2] = (char) highbyte(TCP_DATA_MAXLEN);
		    TCPDATA[3] = (char) lowbyte(TCP_DATA_MAXLEN);
		    tcp_optionlength = 4;
		    break;

		case TCP_STATE_FINACK:
		    TCP->flags = TCP_FLAG_ACK;
		    // fallthrough to TCP_STATE_FIN

		case TCP_STATE_FIN:
		    TCP->flags |= TCP_FLAG_FIN;

		    // check if all we transmitted last time was a FIN flag
		    if (ndwaddhw(session->sendnext, 1) == session->acknext)
		    {
			break;
		    }

		    // there was data included with the transmission of the last FIN,
		    // fallthrough to TCP_STATE_CONNECTED

		case TCP_STATE_CONNECTED:
		    // let the application rebuild the last frame
		    TCP->flags = TCP_FLAG_ACK;
		    tcp_datalength =
			session->application(session->retries, session, 0, &(TCP->flags));


		    break;

		default:
		    // this line should never be reached
		    break;

		}
		// tcp_sendprep will recalculate the expected acknowledge
		session->acknext = session->sendnext;

		if (session->retries == -1)
		{
		    // -1 is special case: is used as a dummy-retry (sender initiated communication from server)
		    session->retries = 1;
		    session->wdtimer = TCP_WATCHDOG_TICKS;
		}
	    }
	    framebuf_send(tcp_sendprep(session, tcp_optionlength, tcp_datalength));

	    // report back we generated something to send
	    return 1;
	}
    }

    // next time around try another session
    if (++nr >= TCP_SESSIONS)
    {
	nr = 0;
    }

    return 0;
}


/***************************************************************************
 * FUNCTION:	tcp_process
 *
 * Process incoming TCP packet present in the buffer indicated by IPBUF
 * for length ip_datalength.
 * Accept SYN packets on TCP-ports defined in the tcp_servers list,
 * it is send to the application belonging to that port.
 * Other SYN packets are replied to with a ICMP port unreachable.
 * Other packets are accepted if they belong to an open connection
 * (dest/source port and source IP matching).
 * Other packets are dropped or replied to with a RESET.
 */
static Uint16 tcp_process(Uint16 ip_datalength)
{
    TCP_SESSION *session;
    TCP_SERVER *server;
    Uint16 tcp_destport;
    Uint16 tcp_sourceport;
    Uint16 tcp_optionslength;
    Uint16 tcp_datalength;
    Uint8 received_emptyack;
    Uint8 i;

    debug_tcp(tcp_show(s("received")));

    // some redundancy
    tcp_destport = ntohw(TCP->destport);
    tcp_sourceport = ntohw(TCP->sourceport);
    tcp_optionslength = (TCP->headerinfo >> 2) - TCPHEADER_LEN;
    tcp_datalength = ip_datalength - TCPHEADER_LEN - tcp_optionslength;

    // verify checksum (over TCP segment and IP-pseudoheader)
    if (std_checksum(ip_datalength))
    {
	debug_tcp(printf(s("TCP CHECKSUM ERROR\n")));
	return 0;
    }

    // check if frame belongs to existing session
    for (i = 0, session = tcp_sessions; i < TCP_SESSIONS; ++i, ++session)
    {
	if ((session->state != TCP_STATE_CLOSED) &&
	    (session->localport == tcp_destport) &&
	    (session->remoteport == tcp_sourceport) && (session->remoteip == IP->sourceip))
	{
	    if (TCP->flags & TCP_FLAG_RESET)
	    {
		// reset session, drop frame
		debug_tcp(printf(s("TCP session reset by remote\n")));

		session->state = TCP_STATE_CLOSED;
		session->retries = 0;
		session->wdtimer = 0;

		// inform application
		session->application(0, session, 0, &(TCP->flags));

		// no reply needed
		return 0;
	    }

	    if (!(TCP->flags & TCP_FLAG_ACK))
	    {
		// drop frame, except first SYN every one should have ACK set
		debug_tcp(printf(s("TCP dropped non-ACK-ed non-SYN frame\n")));
		return 0;
	    }

	    // check if:
	    // (1) received data (if any) is in sequence
	    // (2) they acknowledged all our data before sending any
	    //     of their ownm, this is a bit simplistic on our side,
	    //     but makes the implementation of our server/client
	    //     applications a lot easier
	    if (((tcp_datalength != 0) && (TCP->sequence != session->recvnext)) ||
		(TCP->acknowledge != session->acknext))
	    {
		if (TCP->acknowledge == session->sendnext)
		{
		    // they acknowledged our previous packet, let the retry-code
		    // resend it, just drop this packet
		    session->timer = 0;
		    debug_tcp(printf(s("TCP received ACK of previous frame, resend our data\n")));
		    return 0;
		}

		// we received an acknowledge our implementation can't handle
		session->state = TCP_STATE_CLOSED;
		TCP->flags = TCP_FLAG_RESET;
		debug_tcp(printf
			  (s
			   ("TCP received SEQ/ACK we cannot handle, sendnext=%02X%02X%02X%02X recvnext=%02X%02X%02X%02X acknext=%02X%02X%02X%02X\n"),
			   printfnetdw(session->sendnext), printfnetdw(session->recvnext),
			   printfnetdw(session->acknext)));

		// inform application about reset
		session->application(0, session, tcp_datalength, &(TCP->flags));

		return tcp_sendprep(session, 0, 0);
	    }

	    // all send data is acknowledged, and we possibly received new data
	    session->retries = 0;
	    session->sendnext = session->acknext;

	    // we are accepting all data they send us (received SYN & FIN count for one Uint8)
	    session->recvnext =
		ndwaddhw(session->recvnext,
			 (Uint16) (tcp_datalength + ((TCP->flags & (TCP_FLAG_SYN | TCP_FLAG_FIN)) ? 1 : 0)));

	    switch (session->state)
	    {
	    case TCP_STATE_SYN:
		if (TCP->flags != (TCP_FLAG_SYN | TCP_FLAG_ACK))
		{
		    // weird pakket, just drop
		    debug_tcp(printf(s("TCP dropped non-SYN-ACK response on SYN\n")));
		    return 0;
		}

		// received an ACK of our SYN, handshaking now complete
		// and we know what sequence they will be using
		session->recvnext = ndwaddhw(TCP->sequence, 1);
		TCP->flags &= (Uint16) ~ TCP_FLAG_SYN;

		// fallthrough to TCP_STATE_SYNACK

	    case TCP_STATE_SYNACK:
		// received an ACK of our SYNACK, handshaking now complete
	    case TCP_STATE_CONNECTED:
		if (TCP->flags & TCP_FLAG_FIN)
		{
		    // remote site wants to close the connection
		    session->state = TCP_STATE_FINACK;
		}

		// valid frame received, reset the inactivity watchdog
		session->wdtimer = TCP_WATCHDOG_TICKS;

		// call application to process incoming data and/or generate
		// new data, application can set the RESET or FIN flag in the
		// buffer and should return the tcp_datalength
		received_emptyack = (((session->state == TCP_STATE_SYNACK) || (session->state == TCP_STATE_CONNECTED)) &&
		                     ((TCP->flags & ~TCP_FLAG_PUSH) == TCP_FLAG_ACK) && !tcp_datalength);
		tcp_datalength =
		    session->application(0, session, tcp_datalength, &(TCP->flags));

		if (TCP->flags & TCP_FLAG_RESET)
		{
		    // application wants to abort the session
		    session->state = TCP_STATE_CLOSED;
		}
		else if ((session->state == TCP_STATE_SYN) || (session->state == TCP_STATE_SYNACK))
		{
		    // handshake complete, connection established
		    session->state = TCP_STATE_CONNECTED;
		}

		if ((session->state == TCP_STATE_CONNECTED) && (TCP->flags & TCP_FLAG_FIN))
		{
		    // application wants to close the session
		    session->state = TCP_STATE_FIN;
		}

		if (received_emptyack && ((TCP->flags & ~TCP_FLAG_PUSH) == TCP_FLAG_ACK) && !tcp_datalength)
		{
		    // server has nothing to say and we just received an empty ACK
		    return 0;
		}
		return tcp_sendprep(session, 0, tcp_datalength);

	    case TCP_STATE_FIN:
		if (TCP->flags & TCP_FLAG_FIN)
		{
		    // our FIN is ACK-ed, send the final empty ack
		    session->state = TCP_STATE_LASTACK;

		    // valid frame received, reset the inactivity watchdog
		    session->wdtimer = TCP_WATCHDOG_TICKS;

		    // inform the application the session is closed
		    session->application(0, session, 0, &(TCP->flags));

		    break;
		}

		debug_tcp((tcp_datalength != 0) ?
			  printf(s("TCP dropped unexpected frame while we already have send FIN\n")) : 0);

		// just drop whatever they send us if not a FIN
		return 0;

	    case TCP_STATE_FINACK:
		// our FINACK is ACK-ed, the session is over, no answer needed
		session->state = TCP_STATE_CLOSED;

		// no inactivity watchdog needed anymore
		session->wdtimer = 0;

		// inform the application the session is closed
		session->application(0, session, 0, &(TCP->flags));

		return 0;

	    case TCP_STATE_LASTACK:
		// should not have received anything after we send our last ACK,
		// but they probably missed it, just ignore
		debug_tcp(printf(s("TCP dropped response on last ACK\n")));
		return 0;

	    default:
		// should never reach this line
		break;

	    }

	    // acknowledge any data or send an empty ACK
	    TCP->flags = TCP_FLAG_ACK;
	    return tcp_sendprep(session, 0, 0);
	}
    }

    // if we are here the frame did not belong to an existing connection

    if (TCP->flags & TCP_FLAG_SYN)
    {
	// received request for a new connection
	// check if we have a server on the given port
	for (server = tcpip_settings.tcpservers; server->serverfunction; ++server)
	{
	    if (server->port == tcp_destport)
	    {
		// found a listening server, find an available (CLOSED) sessionslot
		for (i = 0, session = tcp_sessions;
		     (i < TCP_SESSIONS) && (session->state != TCP_STATE_CLOSED); ++i, ++session)
		{
		}

		if (i == TCP_SESSIONS)
		{
		    // no CLOSED session available, just use any LASTACK slot
		    for (i = 0, session = tcp_sessions;
			 (i < TCP_SESSIONS) && (session->state != TCP_STATE_LASTACK); ++i, ++session)
		    {
		    }
		}

		if (i == TCP_SESSIONS)
		{
		    // really no slot available, drop the frame and hope
		    // their retry will arrive at a more convenient moment
		    debug_tcp(printf(s("TCP dropped SYN frame, no sessions available\n")));
		    return 0;
		}

		// initialize our session
		session->state = TCP_STATE_SYNACK;
		tcp_sequence_next += 0x01000000;
		session->sendnext = session->acknext = htondw(tcp_sequence_next);
		session->recvnext = ndwaddhw(TCP->sequence, 1);
		session->localport = server->port;
		session->remoteport = tcp_sourceport;
		session->remoteip = IP->sourceip;
		session->maxdatalength = TCP_DATA_MAXLEN;
		session->application = server->serverfunction;
		memset(&session->cargo, 0, sizeof(session->cargo));

		// pass our receiving windowsize as an option
		TCPDATA[0] = (char) TCP_RECVWIN_OPTION;
		TCPDATA[1] = (char) TCP_RECVWIN_OPTION_LEN;
		TCPDATA[2] = (char) highbyte(TCP_DATA_MAXLEN);
		TCPDATA[3] = (char) lowbyte(TCP_DATA_MAXLEN);

		TCP->flags |= TCP_FLAG_ACK;

		// inform application
//              session->application(0, session, 0, &(TCP->flags));

		session->retries = 0;

		// start the inactivity watchdog
		session->wdtimer = TCP_WATCHDOG_TICKS;

		return tcp_sendprep(session, 4, 0);
	    }
	}

	// send back ICMP message "port unreachable":
	// move original IP-header and 8 bytes of the received data behind the ICMP header
	memcpy(ICMPDATA, IPBUF, IPHEADER_LEN + 8);

	ICMP->type = ICMP_TYPE_UNREACH;
	ICMP->code = ICMP_CODE_UNREACH_PORT;
	ICMP->ident = 0;
	ICMP->sequence = 0;

	IP->protocol = IP_PROTOCOL_ICMP;

	return icmp_sendprep(ICMPHEADER_LEN + IPHEADER_LEN + 8);
    }

    if (TCP->flags & TCP_FLAG_RESET)
    {
	// received an old reset, just drop
	return 0;
    }

    // we received something we shouldn't have, send a reset
    TCP->flags = TCP_FLAG_RESET;
    debug_tcp(printf("TCP received something unexpected, returned reset\n"));
    return tcp_sendprep(NULL, 0, 0);
}


/***************************************************************************
 * FUNCTION:	tcp_sendprep
 *
 * Prepare TCP packet (with incoming header) for transmission.
 * If building a packet from scratch keep in mind the ports & addresses
 * will be reversed by this function.
 *
 * session		session to which the packet belongs
 			if session == null it's a packet not related to a
                        specific session (normally a RESET reply to something
                        unexpected)
 * tcp_optionslength    number of optionbytes present after header
 * tcp_datalength	number of databytes present after header and options
 *
 * returns the total length of the packet
 */
static Uint16 tcp_sendprep(TCP_SESSION * session, Uint8 tcp_optionslength, Uint16 tcp_datalength)
{
    Uint16 ip_datalength;

    ip_datalength = (TCPHEADER_LEN + tcp_optionslength + tcp_datalength);

    if (session != NULL)
    {
	// calculate the next acknowledge expected
	// (transmitted SYN & FIN count for one Uint8)
	session->acknext =
	    ndwaddhw(session->acknext,
		     (Uint16) (tcp_datalength + ((TCP->flags & (TCP_FLAG_SYN | TCP_FLAG_FIN)) ? 1 : 0)));

	// initialize retry-timer
	if (session->state == TCP_STATE_LASTACK)
	{
	    // special "retry", will move session into state CLOSED, no inactivity watchdog needed
	    session->retries = -1;
	    session->timer = TCP_LASTACK_TICKS;
	    session->wdtimer = 0;
	}
	else if ((tcp_optionslength + tcp_datalength == 0) && (TCP->flags == TCP_FLAG_ACK))
	{
	    // no retries needed for an empty ACK
	    session->retries = 0;
	}
	else
	{
	    ++session->retries;
	    session->timer = TCP_RETRY_TICKS;
	}

	if (TCP->flags & TCP_FLAG_RESET)
	{
	    session->retries = 0;
	    session->timer = 0;
	    session->wdtimer = 0;
	}

	TCP->sequence = session->sendnext;
	TCP->acknowledge = session->recvnext;
    }
    else
    {
	// no related session, probably a reset on an unexpected packet
	swapdw(TCP->sequence, TCP->acknowledge);
    }

    // put the dataoffset in DWORDS into the upper 4 bits of headerinfo
    TCP->headerinfo = (TCPHEADER_LEN + tcp_optionslength) << 2;

    // change packet to be send back, fast but messy
    // (means self-generated frames have to be filled in reverse)
    swapw(TCP->sourceport, TCP->destport);

    // set out windowsize to the maximum data in a single frame
    // to get every single frame ACK-ed
    TCP->window = htonw(TCP_DATA_MAXLEN);

    // recalculate checksum (over TCP segment and IP-pseudoheader)
    TCP->checksum = 0;
    TCP->checksum = std_checksum(ip_datalength);
    return ip_sendprep(ip_datalength);
}


/***************************************************************************
 * FUNCTION:	tcp_create
 *
 * Create a new session to a given address/port, create a TCP SYN packet
 * to start the new connection.
 *
 * sourceport   TCP port our side
 * destip	IP address remote side
 * destport     TCP port remote side
 * application  callback function for client who will process the incoming data
 * 		and will generate outgoing data
 * 		parameters for callback:
 * 		session		active TCP session
 * 		buf       	start of data in TCP frame
 * 		datalength      number of incoming databytes
 * 		tcp_flags	pointer to Uint8 with TCP flags in the buffer
 *		returns number of outgoing databytes
 *
 * returns the new session, or NULL if no free session available.
 */
TCP_SESSION *tcp_create(Uint16 sourceport, Uint32 destip, Uint16 destport,
			Uint16 CALLBACKMEMSPEC(*application) (Sint8 resend, TCP_SESSION * session,
							      Uint16 datalength, Uint8 * tcp_flags))
{
    TCP_SESSION *session;
    Uint8 i;

    // find CLOSED session we can use
    for (i = 0, session = tcp_sessions; (i < TCP_SESSIONS) && (session->state != TCP_STATE_CLOSED); ++i, ++session)
    {
    }

    if (i == TCP_SESSIONS)
    {
	// no CLOSED session available, just find a LASTACK slot
	for (i = 0, session = tcp_sessions; (i < TCP_SESSIONS) && (session->state != TCP_STATE_LASTACK); ++i, ++session)
	{
	}
    }

    if (i == TCP_SESSIONS)
    {
	// really no slot available
	debug_tcp(printf(s("TCP cannot send SYN, no sessions available\n")));
	return NULL;
    }

    // initialize our session
    session->state = TCP_STATE_SYN;
    tcp_sequence_next += 0x01000000;
    session->sendnext = session->acknext = htondw(tcp_sequence_next);
    session->recvnext = 0;
    if (sourceport)
    {
	session->localport = sourceport;
    }
    else
    {
	// use the next port in our range
	if (++tcp_port_next > TCP_PORT_CLIENT_END)
	{
	    tcp_port_next = TCP_PORT_CLIENT_START;
	}
	session->localport = tcp_port_next;
    }
    session->remoteport = destport;
    session->remoteip = destip;
    session->maxdatalength = TCP_DATA_MAXLEN;
    session->application = application;

    // initialize the retries so the SYN will be send as soon as we have time available
    session->retries = -1;
    session->timer = 0;
    session->wdtimer = TCP_WATCHDOG_TICKS;

    return session;
}


#ifdef OPTION_UDP
/***************************************************************************
 * FUNCTION:	udp_sendprep
 *
 * Prepare UDP packet (with incoming header) for transmission.
 * If building a packet from scratch keep in mind the ports & addresses
 * will be reversed by this function.
 *
 * ip_datalength	number of bytes present after IP-header
 *
 * returns the total length of the packet
 */
Uint16 udp_sendprep(Uint16 ip_datalength)
{
    // update the length of the UDP frame
    UDP->length = htonw(ip_datalength);

    // change packet to be send back, fast but messy
    // (means self-generated frames have to be filled in reverse)
    swapw(UDP->sourceport, UDP->destport);

    // recalculate checksum (over UDP segment and IP-pseudoheader)
    UDP->checksum = 0;
    UDP->checksum = std_checksum(ip_datalength);
    return ip_sendprep(ip_datalength);
}


/***************************************************************************
 * FUNCTION:	udp_create
 *
 * Initialize a new UDP packet.
 *
 * sourceport	local UDP port
 * destport	remote UDP port
 */
void udp_create(Uint16 sourceport, Uint16 destport)
{
    IP->protocol = IP_PROTOCOL_UDP;

    // source/dest are reversed as the ip_sendprep swaps them
    UDP->sourceport = htonw(destport);
    UDP->destport = htonw(sourceport);
}

#endif // OPTION_UDP


/***************************************************************************
 * FUNCTION:	icmp_sendprep
 *
 * Prepare ICMP packet for transmission.
 *
 * ip_datalength	number of bytes present after IP-header
 *
 * returns the total length of the packet
 */
static Uint16 icmp_sendprep(Uint16 ip_datalength)
{
    ICMP->checksum = 0;
    ICMP->checksum = addcarry16(checksum((Uint8*) IPDATA, ip_datalength));
    return ip_sendprep(ip_datalength);
}


/***************************************************************************
 * FUNCTION:	ip_sendprep
 *
 * Prepare IP packet (with incoming header) for transmission.
 * If building a packet from scratch keep in mind the addresses
 * will be reversed by this function.
 *
 * ip_datalength	number of bytes present after IP-header
 *
 * returns the total length of the packet
 */
static Uint16 ip_sendprep(Uint16 ip_datalength)
{
    Uint16 ip_packetlength;

    ip_packetlength = IPHEADER_LEN + ip_datalength;

    IP->packetlength = htonw(ip_packetlength);

    // change packet to be send back, fast but messy
    // (means self-generated frames have to be filled in reverse)
    swapdw(IP->sourceip, IP->destip);

    IP->checksum = 0;
    IP->checksum = addcarry16(checksum((Uint8*) IPBUF, IPHEADER_LEN));

    debug_icmp((IP->protocol == IP_PROTOCOL_ICMP) ? icmp_show(s("send")) : 0);
    debug_udp((IP->protocol == IP_PROTOCOL_UDP) ? udp_show(s("send")) : 0);
    debug_tcp((IP->protocol == IP_PROTOCOL_TCP) ? tcp_show(s("send")) : 0);
    debug_ip(ip_show(s("send")));

    return ip_packetlength;
}


/***************************************************************************
 * FUNCTION:	ip_create
 *
 * Initialize a new IP packet to the given IP address destip.
 * returns a pointer to the IP-header created, or NULL if the
 * linebuffer was in use and no packet was created.
 */
char *ip_create(Uint32 destip)
{
    if (!framebuf_init_ip())
    {
        return NULL;
    }

    IP->version = (IP_VERSION_IP4 << 4) | (IPHEADER_LEN >> 2);
    IP->service = 0;
    IP->ident = ++ip_ident_next;
    IP->offset = 0;
    IP->timetolive = 32;
    // source/dest are reversed as the ip_sendprep swaps them
    IP->sourceip = destip;
    IP->destip = *(Uint32 *) tcpip_settings.self_ip;

    return IPBUF;
}


/***************************************************************************
 * FUNCTION:	std_checksum
 *
 * Calculate checksum over imaginary header & data (used for UDP, DNS, TCP)
 * NB: checksum calculations are carried out in little-endian mode whatever
 * the processor.
 *
 * ip_datalength	number of bytes located after IP header
 *
 * returns checksum
 */
static Uint16 std_checksum(Uint16 ip_datalength)
{
    Uint32 sum;

    // calculate checksum over imaginary header
    sum = (IP->sourceip & 0xffff);
    sum += ((IP->sourceip >> 16) & 0xffff);
    sum += (IP->destip & 0xffff);
    sum += ((IP->destip >> 16) & 0xffff);
    sum += htonw((Uint16) IP->protocol);
    sum += htonw(ip_datalength);

    sum += checksum((Uint8*) IPDATA, ip_datalength);
    return addcarry16(sum);
}


/***************************************************************************
 * FUNCTION:	addcarry16
 *
 * Convert a Uint32 checksum to a Uint16 by recursively adding all bits over 16,
 * and finally inverting it.
 */
static Uint16 addcarry16(Uint32 sum)
{
    // add any carry
    while (sum >> 16)
    {
	sum = (sum & 0xffff) + (sum >> 16);
    }

    return ~(Uint16) (sum & 0xffff);
}


/***************************************************************************
 * FUNCTION:	checksum
 *
 * Simple checksum over block of data (start at chkdata for length bytes,
 * returns the checksum)
 * NB: checksum calculations are carried out in little-endian mode whatever
 * the processor.
 */
static Uint32 checksum(Uint8* chkdata, Sint16 length)
{
    Uint32 sum, add;

    for (sum = 0; length > 1; length -= 2)
    {
#ifdef LITTLE_ENDIAN
	add = (Uint32) ((((Uint16) chkdata[1]) << 8) + *chkdata);
#endif
#ifdef BIG_ENDIAN
	add = (Uint32) ((((Uint16) * chkdata) << 8) + chkdata[1]);
#endif
	sum = sum + add;
	chkdata += 2;
    }

    /* add up any odd Uint8 */
    if (length == 1)
    {
#ifdef LITTLE_ENDIAN
	add = (Uint32) * chkdata;
#endif
#ifdef BIG_ENDIAN
	add = (Uint32) (((Uint16) *chkdata) << 8);
#endif
	sum = sum + add;
    }

    return sum;
}

//*****************************************************************************
