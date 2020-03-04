////////////////////////////////////////////////////////////////////////////////
// altBench.c

#include <stdint.h>
#include <stdbool.h>

#include <lwip/opt.h>
#include <lwip/arch.h>
#include <lwip/sockets.h>

#include <timing.h>
#include "alt_bench.h"

#if __POSIX_KERNEL__
#include <pthread.h>
#endif

////////////////////////////////////////////////////////////////////////////////
#define DEFAULTPORT 0x494F /* "IO" */

////////////////////////////////////////////////////////////////////////////////
#define TMAXSIZE 65536
#define INTERVAL (6 * CLOCKS_PER_SEC)

////////////////////////////////////////////////////////////////////////////////
typedef struct _netio_prot_t{
    uint32_t cmd;
    uint32_t data;
}netio_prot_t;

////////////////////////////////////////////////////////////////////////////////
#define CMD_QUIT  0
#define CMD_C2S   1
#define CMD_S2C   2
#define CMD_RES   3

#define NETIO_PROT_SIZE sizeof(netio_prot_t)

////////////////////////////////////////////////////////////////////////////////
char    buffer[TMAXSIZE];
short   port = DEFAULTPORT;
struct  in_addr addr_local;
//int     bTimeOver;

////////////////////////////////////////////////////////////////////////////////
static int send_data(int socket, void * buf, size_t size, int flags)
{
    int rc = send(socket, buf, size, flags);

    if (rc < 0)     return -1;
    if (rc != size) return  1;

    return 0;
}

////////////////////////////////////////////////////////////////////////////////
static int recv_data(int socket, void * buf, size_t size, int flags)
{
    ssize_t rc = recv(socket, buf, size, flags);

    if (rc < 0)     return -1;
    if (rc != size) return  1;

    return 0;
}

////////////////////////////////////////////////////////////////////////////////
static void altium_benchmark_server(void)
{
    netio_prot_t         prot;
    long long            n_data;
    struct sockaddr_in   sa_server, sa_client;
    int                  server, client;
    size_t               length;
    int                  rc = 0;
    int                  n_byte;
    int                  err_count;

    clock_t              stop_send;

    if ((server = socket(PF_INET, SOCK_STREAM, 0)) < 0) return;

    sa_server.sin_family = AF_INET;
    sa_server.sin_port   = htons(port);
    sa_server.sin_addr   = addr_local;

    if (bind(server, (struct sockaddr *) &sa_server, sizeof(sa_server)) < 0){
        closesocket(server);
        return;
    }

    if (listen(server, 2) != 0){
        closesocket(server);
        return;
    }

    while(1){
        length = sizeof(sa_client);

        if ((client = accept(server, (struct sockaddr *) &sa_client, &length)) == - 1) continue;

        while(1){
            if (recv_data(client, (void *) &prot, NETIO_PROT_SIZE, 0)) break;

            prot.cmd  = ntohl(prot.cmd);
            prot.data = ntohl(prot.data);

            if (prot.cmd == CMD_C2S){
                n_data    = 0;
                err_count = 0;
                do {
                    for (n_byte = 0; n_byte < prot.data;) {
                        rc = recv(client, buffer + n_byte, prot.data - n_byte, 0); //MSG_DONTWAIT

                        if(!rc){
                            if(++err_count > 5) break;
                        }

                        if (rc < 0) break;

                        if (rc > 0) n_byte += rc;
                    }

                    n_data += prot.data;
                } while (buffer[0] == 0 && rc > 0);

            }else if (prot.cmd == CMD_S2C){
                buffer[0] = 0;
                n_data      = 0;

                stop_send = clock() + INTERVAL;

                while(clock() < stop_send) {
                    for(n_byte = 0; n_byte < prot.data;) {
                        rc = send(client, buffer + n_byte, prot.data - n_byte, 0);

                        if (rc < 0) break;
                        if (rc > 0) n_byte += rc;
                    }

                    n_data += prot.data;
                }

                buffer[0] = 1;

                if(send_data(client, buffer, prot.data, 0)) break;

            } else { /* quit */
                break;
            }
        }

        closesocket(client);

        if (rc < 0) break;
    }

    closesocket(server);

    return;
}

////////////////////////////////////////////////////////////////////////////////
static void *altium_benchmark_daemon(void *arg)
{
    while(1){
        altium_benchmark_server();
        delay_ms(10000);   // Sleep before server restart attempt
    }
}

////////////////////////////////////////////////////////////////////////////////
int start_benchmark_daemon()
{
#if __POSIX_KERNEL__
    // We have threads. Spawn server and return.
    pthread_attr_t attr;
    struct sched_param  sched_param;

    printf("Starting Altium Network Performance server...");
    memset(&sched_param, 0, sizeof(sched_param));
    pthread_attr_init(&attr);
    pthread_create(NULL, &attr, altium_benchmark_daemon, NULL);
    printf("DONE!\n");
#else
    // No threads. Spawn blocking.
    altium_benchmark_daemon();
#endif // __POSIX_KERNEL__

    return 0;
}


