/*********************************************************
 * FILE:	@(#)serial.h	1.2 04/09/21
 * DESCRIPTOR:
 * 	uart interface.
 ********************************************************/

#ifndef _H_UART
#define _H_UART

#include <osek/osek.h>
#include "output.h"

/* uart protocols */
#define UART_SLIP	0
#define UART_SHELL	1
#define UART_RAW	2

/* defined in uart.c */
extern void tx_action(void);
extern void rx_action(char c);
/* defined in serial.c */
extern void serial_init(void);
extern void uart_tx_send(device,char);
extern char uart_tx_read(device);

#endif


