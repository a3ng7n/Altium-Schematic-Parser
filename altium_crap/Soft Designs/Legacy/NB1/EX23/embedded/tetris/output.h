/******************************************************************************
 * FILE:	@(#)output.h	1.7 04/09/21
 * DESCRIPTION:
 * 	interface for unified io.
 *  	Wrapper around io "channels" (uart,stdio, ..).
 *	- SERIAL0/1: Makes an IO request (transmit or receive).
 *		   A simple uart solution, handling io requests, that
 *	   	   has been tailored to the needs of these examples.
 *		   All uart io requests are non-blocking calls and
 *		   interrupt driven.
 *		   
 *	- STDIO:   just calls standard io routines.
 *		   Blocking io call (so only output!). 
 *		   Meant to be used only as a development aid when running
 *		   in simulators with no serial interrupt support.
 *		   When running in hw, it should be best avoided.
 *		   STDIOTX must be defined.
 *		   
 *	- LCD:	   Nexar output driver (only input).
 *	- KEYPAD:  Nexar input driver (only output).
 *		   NEXAR must be defined.
 *****************************************************************************/
#ifndef _H_IO
#define _H_IO

#include <stdint.h>

/******************************************************************************
 * external interface of io component external interface of io component 
 *****************************************************************************/

#define IO_WRITE	1
#define IO_FLUSH	2
#define IO_READ		4

#define	NONE		-1
#define SERIAL0		0
#define SERIAL1		1
#define STDIO		2
#define LCD		3
#define KEYPAD		4
#define SIMIN		5

#define NO_UART		2
/* final number of devices */
#define NO_DEVICES	6
typedef int_least8_t	device;

/* string output */
extern int outs(device out, char* txt);
/* string input */
extern int ins(device in, char* txt, int nbytes);
/* flush device : ONLY for stdio */
extern int outflush(device out);
/* init channels */
extern void ioinit(void);

/******************************************************************************
 * internal interface of io component internal interface of io component 
 *****************************************************************************/

/* max cifers for outputted integers */
#define NO_CIFERS		4

/* layout of an io request */
typedef struct ioreq_s ioreq;
struct ioreq_s
{
    /* points to next byte to write/read */
    char *        buf;
    /* length of request */
    int           len;
    /* current number of bytes already read/written */
    int           pos;
    /* result of the operation */
    int           err;
    /* who owns this request (i.e. whos waiting ) */
    TaskType      task;
    /* waiting on which event */
    EventMaskType event;
    /* device */
    device	  device;
    /* device protocol */
    uint_least8_t protocol;
    /* operational mode */
    uint_least8_t mode;
};

/* layout of device configuration */
typedef struct
{
	/* event to wait when device is busy receiving */
	EventMaskType   rxevent;
	/* message to make a rx-io-request possible in device */
	SymbolicName  	rxmsg;
	/* event to wait when device is busy transmitting */
	EventMaskType 	txevent;
	/* message to make a tx-io-request possible in device */
	SymbolicName  	txmsg;
} device_s;

/* devices configuration */
extern device_s devices[];

/* devices initializations */
extern void uart_init(void);
extern void standard_init(void);
extern void lcd_init(void);
extern void keypad_init(void);
extern void simin_init(void);

#endif


