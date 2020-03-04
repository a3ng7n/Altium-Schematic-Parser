/*****************************************************************************\
|*
|*  IN PACKAGE:
|*
|*  COPYRIGHT:          Copyright (c) 2008, Altium
|*
|*  DESCRIPTION:
|*
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <timing.h>
#include <string.h>

#include <devices.h>
#include <drv_uart8.h>

static void print_incoming(bool compare);
static void init(void);
uart8_t *uart;

#define BUFSIZE  16

int buf[BUFSIZE];


/**********************************************************************
|*
|*  FUNCTION    :
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION :
 */

void main(void)
{
    int i;
    int c;
    int parity = 0;
    int wordmask;

    init();

    puts("Serial mode:");
    printf("- databits %i\n", uart8_get_databits(uart));
    switch (uart8_get_parity(uart))
    {
    case UART8_ODD_PARITY : puts("- odd parity"); break;
    case UART8_EVEN_PARITY : puts("- even parity"); break;
    case UART8_USER_PARITY : puts("- user controlled parity"); break;
    default: puts("- no parity"); break;
    }
    printf("- %i stopbits\n\n", uart8_get_stopbits(uart));

    // generate random data based on databits and optional userparity
    printf("Fill buffer: ");
    wordmask = (0xFF >> (8 - uart8_get_databits(uart)));
    if (uart8_get_parity(uart) == UART8_USER_PARITY) wordmask |= UART8_PARERR;
    for (i = 0; i < BUFSIZE; i++)
    {
        c = rand() & wordmask;
        printf("%02X%c ", c & 0xFF, (c & UART8_PARERR) ? '+' : '-');
        buf[i] = c;
    }
    printf("(%i bytes)\n", i);

    puts("Writing data... \n");
    for (i = 0; i < BUFSIZE; i++)
    {
        uart8_set_user_parity(uart, (buf[i] & UART8_PARERR) ? 1 : 0 );
        uart8_putchar(uart, buf[i]);
    }

    if (uart8_transmit_idle(uart))
    {
        puts("ERROR: No transmission");
    }
    else
    {
        puts("OK");
    }

    // Transmission should be finished within transmission time for 11 x bytes x bittime.
    // To make sure, we calculate with 12 bits per byte

    delay_ms(1000 * 12 * BUFSIZE / uart8_get_baudrate(uart));

    if (uart8_transmit_idle(uart))
    {
        printf("Done transmitting %d bytes...\n", i);
    }
    else
    {
        puts("WARNING: transmission takes longer than expected...");
    }

    // Are there any characters for me?
    puts(uart8_receive_buf_available(uart) ? "Remote answered" : "ERROR: No answer");

    print_incoming(true);

    // Send a break to have remote empty it's buffers
    puts("Clearing remote...");
    uart8_putbreak(uart, 20);
    delay_ms(100);

    // Signal to the remote not to send me anything back
    puts("Blocking remote...");
    uart8_rts(uart, true);

    // Clear our own buffers by writing baudrate
    uart8_set_baudrate(uart, uart8_get_baudrate(uart));

    // Write buffer over serial port:
    for (i = 0; i < BUFSIZE; i++)
    {
        uart8_set_user_parity(uart, (buf[i] & UART8_PARERR) ? 1 : 0);
        uart8_putchar(uart, buf[i]);
    }
    while (!uart8_transmit_idle(uart)) __nop();        // Wait until transmission is actually done
    printf("Transmitted %d bytes\n", i);

    // Are there any characters for me?
    if (uart8_receive_buf_available(uart))
    {
        puts("ERROR: Remote ignored RTS and answered");
        print_incoming(false);
    }
    else
    {
        puts("OK: No answer");
    }
    puts("Ready");

    // Signal to the remote to continue
    puts("Releasing remote...");
    uart8_rts(uart, false);

    // wait for remote to send pending data
    delay_ms(1000 * 12 * BUFSIZE / uart8_get_baudrate(uart));

    // Are there any characters for me?
    if (uart8_receive_buf_available(uart))
    {
        print_incoming(true);
    }
    else
    {
        puts("ERROR: No answer");
    }

    puts("Ready\n");

    puts("This example can run with any baudrate/databits/parity/stopbit setting,");
    puts("use the software platform document to change them.");
    puts("Please make sure both embedded projects use the same settings.");
}


/**********************************************************************
|*
|*  FUNCTION    : print_incoming
|*
|*  PARAMETERS  : compare = if true compare data with buf
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Print incoming bytes and optionally compare with buffer
 */

static void print_incoming(bool compare)
{
    int i;
    int c;

    printf("Receiving: ");
    i = 0;
    while (uart8_receive_buf_available(uart))
    {
        c = uart8_getchar(uart);
        printf("%02X%c ", c & 0xFF, (c & UART8_PARERR) ? '+' : '-');

        if (compare & (i < BUFSIZE) & ((c & (0xFF | UART8_PARERR)) != buf[i]))
        {
            printf("[buf=%02X%c] ", buf[i] & 0xFF, (buf[i] & UART8_PARERR) ? '+' : '-');
        }

        ++i;
    }
    printf("(%i bytes)\n", i);
}




/**********************************************************************
|*
|*  FUNCTION    : init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize hardware & device drivers
 */

static void init(void)
{
    puts("UART example main, " __FILE__ " compiled " __DATE__ ", " __TIME__);

    if (uart = uart8_open(DRV_UART8_MAIN), uart == NULL)
    {
        puts("Fatal: unable to initialize UART");
        abort();
    }
    puts("UART intialization done\n");

    srand(0x12345678);
}
