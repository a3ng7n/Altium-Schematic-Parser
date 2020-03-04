/*************************************************************************
**
**  VERSION CONTROL:	@(#)tcpip_global.h	1.6	04/03/17
**
**  IN PACKAGE:		Embedded TCPIP
**
**  COPYRIGHT:		Copyright (c) 2002 Altium
**
**  DESCRIPTION:	global definitions for TCP
**
**************************************************************************/

#ifndef _TCPIP_GLOBAL_H_
#define _TCPIP_GLOBAL_H_

#include "tcpipset.h"

typedef struct TCP_SESSION TCP_SESSION;
typedef struct TCPIP_SETTINGS TCPIP_SETTINGS;

#ifdef OPTION_FTP_SERVER
#include "common/servers/ftp_server.h"
#endif
#ifdef OPTION_HTTP_SERVER
#include "common/servers/http_server.h"
#endif
#ifdef OPTION_SMTP_CLIENT
#include "common/servers/smtp_client.h"
#endif
#ifdef OPTION_TELNET_SERVER
#include "common/servers/telnet_server.h"
#endif
#ifdef USER_CLIENTSERVER1_H
#include USER_CLIENTSERVER1_H
#endif
#ifdef USER_CLIENTSERVER2_H
#include USER_CLIENTSERVER2_H
#endif
#ifdef USER_CLIENTSERVER3_H
#include USER_CLIENTSERVER3_H
#endif

//**************************************************************************

// TCP portnumbers for supported services
#define TCP_PORT_FTPDATA	20
#define TCP_PORT_FTP		21
#define TCP_PORT_TELNET		23
#define TCP_PORT_SMTP		25
#define TCP_PORT_HTTP		80

// TCP session state
#define TCP_STATE_CLOSED        0x01
#define TCP_STATE_SYN           0x02
#define TCP_STATE_SYNACK        0x03
#define TCP_STATE_CONNECTED     0x04
#define TCP_STATE_FIN           0x05
#define TCP_STATE_FINACK        0x06
#define TCP_STATE_LASTACK       0x07
#define TCP_STATE_RESET         0x08

// TCP bitmap flags
#define TCP_FLAG_FIN		0x01
#define TCP_FLAG_SYN		0x02
#define TCP_FLAG_RESET		0x04
#define TCP_FLAG_PUSH		0x08
#define TCP_FLAG_ACK		0x10
#define TCP_FLAG_URGENT		0x20

//**************************************************************************

// used to keep track of a connection
struct TCP_SESSION
{
    Uint16 localport;
    Uint16 remoteport;
    Uint32 remoteip;
    Uint32 sendnext;
    Uint32 acknext;
    Uint32 recvnext;
    Uint16 maxdatalength;
    Uint16 CALLBACKMEMSPEC(*application) (Sint8 resend, TCP_SESSION * session,
                                          Uint16 datalength, Uint8 * tcp_flags);
    Uint8 state;
    Sint8 retries;
    Uint8 timer;
    Uint16 wdtimer;
    union CARGO
    {
	Uint8 dummybyte;
#ifdef OPTION_FTP_SERVER
	FTP_SERVER_CARGO ftp_server;
#endif
#ifdef OPTION_HTTP_SERVER
	HTTP_SERVER_CARGO http_server;
#endif
#ifdef OPTION_SMTP_CLIENT
	SMTP_CLIENT_CARGO smtp_client;
#endif
#ifdef OPTION_TELNET_SERVER
	TELNET_SERVER_CARGO telnet_server;
#endif
#ifdef USER_CLIENTSERVER1_CARGO
        USER_CLIENTSERVER1_CARGOTYPE USER_CLIENTSERVER1_CARGO;
#endif
#ifdef USER_CLIENTSERVER2_CARGO
        USER_CLIENTSERVER2_CARGOTYPE USER_CLIENTSERVER2_CARGO;
#endif
#ifdef USER_CLIENTSERVER3_CARGO
        USER_CLIENTSERVER3_CARGOTYPE USER_CLIENTSERVER3_CARGO;
#endif
    }
    cargo;
};


// TCP server function
typedef Uint16 CALLBACKMEMSPEC (*TCP_SERVERFUNCTION) (Sint8 resend, TCP_SESSION * session,
                                                      Uint16 datalength, Uint8 * tcp_flags);

// TCP client function (identical to serverfunction, but handshaking differs slightly)
typedef TCP_SERVERFUNCTION TCP_CLIENTFUNCTION;

// used for the array with implemented servers and clients,
typedef struct
{
    // TCP port the application is listening on
    Uint16 port;
    // callbackfunc to server for given tcpport
	TCP_SERVERFUNCTION serverfunction;
}
TCP_SERVER;


// UDP server function
typedef Uint16 CALLBACKMEMSPEC (*UDP_SERVERFUNCTION) (Uint16 port, Uint16 datalength);

// used for the array with implemented servers and clients,
typedef struct
{
    // UDP port the application is listening on
    Uint16 port;
    // callbackfunc to client or server for given udpport
	UDP_SERVERFUNCTION clientserverfunction;
}
UDP_CLIENTSERVER;



// all global definitions, used for initializing and cross-module housekeeping
// just one global instance tcpip_settings (in tcpip.c)
typedef struct TCPIP_SETTINGS
{
    // our own IP-address, can be overruled by PPP negotiation
    // TODO: in fact we just react to all traffic coming out way, change that?
    Uint8 self_ip[4];

    // array with TCP-port / TCP-server applications
    // (last element should have serverfunction NULL)
    TCP_SERVER *tcpservers;

#ifdef OPTION_UDP
    // array with UDP-port / UDP-server/client applications
    // (last element should have serverfunction NULL)
    UDP_CLIENTSERVER *udpclientservers;
#endif

#ifdef OPTION_DNS_SERVER
    char *dns_self_name;	// our own name
#endif
#ifdef OPTION_DNS_CLIENT
    Uint8 dns_world_ip[4];	// the IP address of the DNS server in the world we know about
    void CALLBACKMEMSPEC(*dns_portip) (Uint16 port, Sint8 *ip);	// callback function for incoming DNS ip answers
#endif
#ifdef OPTION_DHCP
    Uint8 dhcp_server_ip[4];	// IP address of DHCP server
    Uint8 dhcp_state;	// state for DHCP
#ifdef DHCP_TIMERTICK_MANUAL
    volatile
#endif
    Uint16 dhcp_timer;	// timer
#endif

#ifdef OPTION_SERIAL
    // pointer to function which will be called whenever a major
    // status change occurs in the status of the connection
    void CALLBACKMEMSPEC(*serial_statuschange) (Uint16 state);

#ifdef SERIAL_MODEM
    char *modem_init;	// initstring for modem
#ifdef SERIAL_CALLOUT
    char *modem_dialoutnr;	// telephonenumber we use for dial-out
#endif
#endif

#ifdef SERIAL_PPP
    // func to call for user/pwd check
    Uint8 CALLBACKMEMSPEC(*ppp_usercheck) (char *user, char *password);

    // the IP address we suggest for the other side of a PPP connection
    Uint8 ppp_client_ip[4];
#ifdef SERIAL_CALLOUT
    char *ppp_out_user;	// PPP username for call-out
    char *ppp_out_password;	// PPP password for call-out
#endif // SERIAL_CALLOUT
#endif // SERIAL_PPP
#endif // SERIAL

#ifdef OPTION_ETHERNET
    Uint8 mac[6];
#endif

#ifdef OPTION_FTP_SERVER
    // func to call for user/pwd check
    Uint8 CALLBACKMEMSPEC(*ftp_server_usercheck) (char *user, char *password, char *rootdir);
#endif

#ifdef OPTION_TELNET_SERVER
    // func to call for user/pwd check
    Uint8 CALLBACKMEMSPEC(*telnet_server_usercheck) (char *user, char *password,
                                                     Uint8 CALLBACKMEMSPEC(**commandprocessor)(char *cmd, char *buf, char *user, Uint8 userid),
						     Uint8 *userid);
#endif
};

//**************************************************************************

extern TCPIP_SETTINGS tcpip_settings;

extern TCP_SESSION *tcp_create(Uint16 sourceport, Uint32 destip, Uint16 destport, TCP_CLIENTFUNCTION clientfunction);

//**************************************************************************

#endif
