#include <stdio.h>
#include <time.h>
#include <lwip.h>
#include "devices.h"

extern int dns_loop(void);

void print_dhcp_info(lwip_t *lwip)
{
    printf("Waiting for DHCP response..");
    const struct timespec ts = { .tv_sec = 1, .tv_nsec = 0 };
    for(;;)
    {
        struct ip_addr addr = lwip_get_local_addr(lwip);
        if (!ip_addr_isany(&addr))
        {
            break;
        }
        nanosleep(&ts, NULL);
        putchar('.');
    }
    printf("Ok\n");
    lwip_print_addrs(lwip);
}

void main(void)
{
    lwip_t * lwip;
    printf("Starting DhcpDns...\n");
    lwip = lwip_open(LWIP_1);
    if (!lwip)
    {
        printf("LWIP open failed.\n");
        return;

    }
    if (lwip_start(lwip)!=ERR_OK)
    {
        printf("LWIP start failed.\n");
        return;

    }

    print_dhcp_info(lwip);
    dns_loop();
    /* never reached */
}
