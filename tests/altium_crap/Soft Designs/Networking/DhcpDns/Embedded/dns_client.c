#define LWIP_DEBUG     // so it will print ip addrs
#define LWIP_DBG_ALL   0x83U

#include <lwip.h>
#include <lwip/netif.h>
#include <lwip/tcpip.h>
#include <lwip/api.h>
#include <lwip/dns.h>

#include <string.h>
#include <stdbool.h>

#define DNS_TIMEOUT    5000
#define DNS_WAIT_UNIT  100
#define DNS_RETRY_TIME 5

/* timespec utility functions */
#include <sys/types.h>
#include <stdint.h>


#define MSEC_PER_SEC   1000
#define NSEC_PER_MSEC (1000 * 1000)
#define NSEC_PER_SEC  (1000 * 1000 * 1000)

inline void
timespec_add(struct timespec * ts_a, struct timespec * ts_b)
{
    ts_a->tv_sec  += ts_b->tv_sec;
    ts_a->tv_nsec += ts_b->tv_nsec;
    while (ts_a->tv_nsec > NSEC_PER_SEC)
    {
        ts_a->tv_sec++;
        ts_a->tv_nsec -= NSEC_PER_SEC;
    }
}

inline void
timespec_from_ms(struct timespec * ts, int32_t ms)
{
    ts->tv_sec  =  ms / MSEC_PER_SEC;
    ts->tv_nsec = (ms % MSEC_PER_SEC) * NSEC_PER_MSEC;
}

inline int32_t
timespec_delta_ms(struct timespec * ts_a, struct timespec * ts_b)
{
    int32_t delta_ms;

    delta_ms  = (ts_a->tv_sec  - ts_b->tv_sec)  * MSEC_PER_SEC;
    delta_ms += (ts_a->tv_nsec - ts_b->tv_nsec) / NSEC_PER_MSEC;
    return delta_ms;
}
/* END timespec utility functions */

char * strnbs(char * str, size_t len)
{
     int in=0;
     int out=0;
     do
     {
         switch (str[in])
         {
         case '\b':
             if (out>0) --out;
             in++;
             break;
         case '\n':
         case '\0':
            str[out] = '\0';
            break;
         default:
            str[out++] = str[in++];
            break;
         }
     }
     while (in < len && str[out]);
     return str;
}

struct dns_result {
    int ready;
    int retried;
    struct ip_addr addr;
};

void dns_found(const char * name, struct ip_addr * ipaddr, void * arg)
{
    struct dns_result * result = (struct dns_result *) arg;
    result->addr.addr = ipaddr ? ipaddr->addr : 0;

    result->ready = true;
}

    struct ip_addr host_addr[7];

bool dns_test(const char * host, struct dns_result * result)
{
    int ticks = 0;
    struct ip_addr host_addr;
    struct timespec ts;

    timespec_from_ms(&ts, DNS_WAIT_UNIT);
    dns_gethostbyname(host,&host_addr,&dns_found,result);

    while(!result->ready && ++ticks < DNS_TIMEOUT / DNS_WAIT_UNIT) {
        nanosleep(&ts, NULL);
    }

    if (!result->ready)
    {
        struct ip_addr addr = dns_getserver(0);
        printf("No response from server (");
        ip_addr_debug_print(LWIP_DBG_ALL,&addr);
        printf(")\n");
        return false;
    }
    else if (result->addr.addr)
    {
        printf("%s = ",host);
        ip_addr_debug_print(LWIP_DBG_ALL,&result->addr);
        printf("\n");
        return true;
    }
    else if (!result->retried)
    {
        printf("%s not found locally, retrying...\n",host);
        ts.tv_sec = DNS_RETRY_TIME;
        nanosleep(&ts, NULL);
        ts.tv_sec = 0;
        memset(result,0,sizeof(*result));
        result->retried = true;
        return dns_test(host,result);
    }
    else
    {
        printf("%s NOT FOUND.\n",host);
        return false;
    }

}

#define BUFSIZE 256

void dns_loop(void)
{
    struct timespec ts = { 0, 300 * NSEC_PER_MSEC};
    struct dns_result result = { 0, {0}};
    char host[BUFSIZE];

    memset(host, 0, sizeof(host));

    while(1) {
        nanosleep(&ts, NULL);

        printf("Enter a host name: ");
        scanf("%s", host);
        strnbs(host,BUFSIZE);

        memset(&result,0,sizeof(result));
        dns_test(host,&result);
    }
}

