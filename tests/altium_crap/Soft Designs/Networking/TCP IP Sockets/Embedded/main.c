
#include <string.h>
#include <ctype.h>
#include <pthread.h>

#include "devices.h"

#include <per_ioport.h>

#include <lwip.h>
#include <lwip/netif.h>
#include <lwip/tcpip.h>
#include <lwip/sockets.h>
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


void http_server_serve_sock(int sock)
{
    char buff[512];
    int size;
    int timeout = 10000;

    // set timeout to 10 seconds
    lwip_setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    // prepare buffer
    memset(buff, 0, sizeof(buff));
    // read data from socket
    size = lwip_read(sock, buff, sizeof(buff)-1);
    // printf("%s\n", buff);
    // check for HTTP GET (very primitive)
    if ((size >= 5) && (strncmp(buff, "GET ", 4) == 0))
    {
        if (urlcmp(buff + 4, "/index.html"))
        {
            // index page
            lwip_write(sock, http_html_index, sizeof(http_html_index) - 1);
        }
        else
        {
            // page not found
            lwip_write(sock, http_html_error, sizeof(http_html_error) - 1);
        }
    }
}


int http_server_sock(void)
{
    int sock, newsock;
    socklen_t cli_addr_len;
    struct sockaddr_in serv_addr, cli_addr;

    /* Create a new TCP connection handle */
    sock = lwip_socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0)
    {
        printf("http_server: Error opening socket");
        return -1;
    }

    /* Bind to port 80 (HTTP) with default IP address */
    memset(&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = INADDR_ANY;
    serv_addr.sin_port = htons(80);
    if (lwip_bind(sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr)) < 0)
    {
        printf("http_server: Error on binding");
        return -1;
    }

    /* Put the connection into LISTEN state */
    lwip_listen(sock, 5);

    while (1)
    {
        newsock = lwip_accept(sock, (struct sockaddr*)&cli_addr, &cli_addr_len);
        if (newsock < 0)
        {
            perror("http_server: Error on accept");
            return -1;
        }
        http_server_serve_sock(newsock);
        lwip_close(newsock);
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

    http_server_sock();

    /* never reached */
}


