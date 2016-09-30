#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <lwip/netdb.h>
#include <time.h>

#include "swplatform.h"
#include "ping/ping.h"

#define PING_TARGET "google.com"
#define NSEC_PER_MSEC 1000000
static struct timespec half_second      = {0, 500 * NSEC_PER_MSEC};

extern bool init(modem_t ** modem, lwip_t ** lwip);

void main(void)
{
    modem_t        * modem;
    lwip_t         * lwip;
    struct ip_addr * ipaddr;
    struct hostent * he;

    puts("PPP 2G/3G Mobile Ping Example.");

    init(&modem, &lwip);
    lwip_print_addrs(lwip);

    puts("Resolving " PING_TARGET "...");
    while (!(he = lwip_gethostbyname(PING_TARGET)))
        nanosleep(&half_second,NULL);

    ipaddr = (struct ip_addr *) he->h_addr_list[0];
    printf("Pinging ");
    ip_addr_print(ipaddr);
    puts("...");

    ping_init(*ipaddr);
}


