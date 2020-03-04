/*************************************************************************
**
**  VERSION CONTROL:  @(#)main_dumbclient.c   1.2   04/04/08
**
**  IN PACKAGE:    Embedded TCPIP
**
**  COPYRIGHT:     example source
**
**  DESCRIPTION:   C166 demo for user-specific client
**                      (talks over ethernet or SLIP to userserver demo)
**                      This is a 'dumb' implementation of the client,
**                      the main loop triggers to client into all actions
**
**************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "tcpipset.h"
#include "common/tcpip.h"
#include "common/tcpip_global.h"
#include "tealib/timer0.h"

//**************************************************************************

// our static IP address
#define SELF_STATIC_IP      {192, 168, 100, 100}

// out MAC address
#define SELF_MAC            { 0xDE, 0xAD, 0xBE, 0xEF, 0x00, 0x01 }

//**************************************************************************

static Uint8 self_ip[] = SELF_STATIC_IP;

const Uint8 mac[] = SELF_MAC;

// list of available TCP servers
static TCP_SERVER tcpservers[] = {
    {0, NULL}
};

/***************************************************************************
 * FUNCTION: main
 */
Sint16 main(void)
{
    long lasttimer_tcpiptick = 0;

    // fill some settings
    tcpip_settings.tcpservers = tcpservers;
    memcpy(tcpip_settings.self_ip, self_ip, 4);
    memcpy(tcpip_settings.mac, mac, 6);

    sys_init();

    tcpipstack_init();

    for (;;)
    {
    long sysclocktick = tmr0_getclock();

    // call timer entry of tcpip stack every second
    if ((sysclocktick / 100) != lasttimer_tcpiptick)
    {
        lasttimer_tcpiptick = sysclocktick / 100;

        tcpipstack_timertick();
    }

    // call main entry of tcpip stack as often as possible
    if (tcpipstack_main() == 0)
    {
        // tcpip had nothing to do
    }
    }
}


//*****************************************************************************
