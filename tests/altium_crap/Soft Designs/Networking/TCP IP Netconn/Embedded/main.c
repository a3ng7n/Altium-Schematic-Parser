
#include <string.h>
#include <ctype.h>
#include <pthread.h>

#include "devices.h"

#include <per_ioport.h>

#include <lwip.h>
#include <lwip/netif.h>
#include <lwip/tcpip.h>
#include "lwip/api.h"

#define LEDS (*IOPORT_BASE8(per_ioport_get_base_address(PRTIO)))

#define LED_ON(nr) LEDS = (LEDS & ~(1 << nr)) | (1 << nr)
#define LED_OFF(nr) LEDS = (LEDS & ~(1 << nr))
#define LED_STATUS(nr) (LEDS & (1 << nr))
#define LED_INVERT(nr) { if (LED_STATUS(nr)) LED_OFF(nr); else LED_ON(nr); }

const static char http_html_index[] = "HTTP/1.1 200 OK\r\nContent-type: text/html\r\n\r\n"
                                      "<html><head><title>Hello</title></head><body>"
                                      "Hello World..."
                                      "</body></html>";
const static char http_html_error[] = "HTTP/1.1 404 Not Found\r\nContent-type: text/html\r\n\r\n"
                                      "<html><head><title>Error</title></head><body>"
                                      "<H2>HTTP 404 - File not found</H2>"
                                      "</body></html>";



pthread_mutex_t led_mutex = PTHREAD_MUTEX_INITIALIZER;

/* the ethernet interface */
struct netif netif;

void *blink1(void *arg)
{
    struct timespec ts;

    for (;;)
    {
        ts.tv_sec = 0;
        ts.tv_nsec = 1000 * 1000 * 250;
        nanosleep(&ts, NULL);

        pthread_mutex_lock(&led_mutex);
        LED_INVERT(0);
        pthread_mutex_unlock(&led_mutex);
    }

    /* never reached */
}


void *blink2(void *arg)
{
    struct timespec ts;

    for (;;)
    {
        ts.tv_sec = 0;
        ts.tv_nsec = 1000 * 1000 * 225;
        nanosleep(&ts, NULL);

        pthread_mutex_lock(&led_mutex);
        LED_INVERT(1);
        pthread_mutex_unlock(&led_mutex);
    }

    /* never reached */
}


int urlcmp(const char *req, const char *url)
{
    for (;; ++req, ++url)
    {
        if ((*url == 0) && (*req == ' '))
        {
            return 1;
        }

        if (toupper(*url) != toupper(*req))
        {
            return 0;
        }
    }
}


void http_server_serve(struct netconn *conn)
{
    struct netbuf *inbuf;
    char *buf;
    u16_t buflen;

    /* Read the data from the port, blocking if nothing yet there.
       We assume the request (the part we care about) is in one netbuf */
    // Set timeout to 5 Minutes
    conn->recv_timeout = 300000;
    netconn_recv(conn, &inbuf);

    if (netconn_err(conn) == ERR_OK)
    {
        netbuf_data(inbuf, (void **) &buf, &buflen);

        // check for HTTP GET (very primitive)
        if ((buflen >= 5) && (strncmp(buf, "GET ", 4) == 0))
        {
            if (urlcmp(buf + 4, "/index.html"))
            {
                // index page
                netconn_write(conn, http_html_index, sizeof(http_html_index) - 1, NETCONN_NOCOPY);
            }
            else
            {
                // page not found
                netconn_write(conn, http_html_error, sizeof(http_html_error) - 1, NETCONN_NOCOPY);
            }
        }
    }

    /* Close the connection (server closes in HTTP) */
    netconn_close(conn);

    /* Delete the buffer (netconn_recv gives us ownership,
       so we have to make sure to deallocate the buffer) */
    netbuf_delete(inbuf);
}




int http_server(void)
{
    struct netconn *conn, *newconn;

    /* Create a new TCP connection handle */
    conn = netconn_new(NETCONN_TCP);
    LWIP_ERROR("http_server: invalid conn", (conn != NULL), return -1; );

    /* Bind to port 80 (HTTP) with default IP address */
    netconn_bind(conn, NULL, 80);

    /* Put the connection into LISTEN state */
    netconn_listen(conn);

    while (1)
    {
        netconn_accept(conn, &newconn);
        http_server_serve(newconn);
        netconn_delete(newconn);
    }

    /* never reached */
}


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

void lwip_setup(void)
{
    lwip_t * lwip;
    printf("Starting TCP/IP example server...\n");
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
}




void main(void)
{
    pthread_t thread;
    pthread_attr_t attr;

    LEDS = 0x00;

    printf("Kernel LWIP HTTP server test\n");

    pthread_attr_init(&attr);
    pthread_create(&thread, &attr, blink1, NULL);
    pthread_create(&thread, &attr, blink2, NULL);
    printf("threads running\n");

    lwip_setup();

    http_server();

    /* never reached */
}


