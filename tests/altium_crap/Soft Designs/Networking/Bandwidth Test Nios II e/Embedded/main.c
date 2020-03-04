#include <string.h>
#include <ctype.h>
#include <pthread.h>

#include <lwip.h>
#include <lwip/opt.h>
#include <lwip/tcp.h>
#include <lwip/tcpip.h>
#include <lwip/init.h>

#include "swplatform.h"
#include "alt_bench.h"


void lwip_setup(void)
{
    lwip_t * lwip;
    printf("Starting Altium Benchmark example running on Nios II e...\n\r");
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
}


void main(void)
{
    uart8_open(DRV_UART8_1);
    lwip_setup();
    start_benchmark_daemon();
}


