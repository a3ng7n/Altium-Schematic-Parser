/******************************************************************************
**  FILE:   	@(#)serial.c	1.3 04/09/21
**  DESCRIPTION:
**  	UART component. 
******************************************************************************/
#if defined(DEMO)
#include "demo.h"
#endif
#if defined(TETRIS)
#include "tetris.h"
#endif

#if defined(UART)

#include "mytypes.h"
#include "serial.h"

/* SLIP constants */
#define END             '\300'      
#define ESC             '\333'      
#define ESC_END         '\334'
#define ESC_ESC         '\335'
#define CONTROL(c)      ((c) & 31)

DeclareMessage(mruartrx);
DeclareMessage(mruarttx);
DeclareTask(tuarttx);
DeclareTask(tuartrx);
DeclareEvent(euarttx);
DeclareEvent(euartrx);
DeclareMessage(msuartrx);
DeclareMessage(msuarttx);

static uint_least8_t uart0_protocol;
static uint_least8_t uart1_protocol;

static int uart_tx_getchar(ioreq*);

static int tx_slip_action(ioreq*);
static int tx_shell_action(ioreq*);
static int tx_raw_action(ioreq*);

static int rx_slip_action(ioreq*, char c);
static int rx_shell_action(ioreq*, char c);
static int rx_raw_action(ioreq*, char);
	
/******************************************************************
 * 'preqtx': information exchange from this task and uart tx interrupt. 
 * - only this task updates the value (with service
 *   ReceiveMessage). 
 *   Since:
 * 	- only one instance of this task can be active.
 * 	- ISR only reads it.
 *      - when writing it in ReceiveMessage interrupts are
 *      disabled
 *   no concurrency problems are expected.
 *****************************************************************/
static ioreq* preqtx;

TASK (tuarttx)
{
	int 	c;
    	if (E_OK != ReceiveMessage(mruarttx,&preqtx))
    	{
            	/* an io request has been lost */
        	ShutdownOS(E_ERR_IO_OFLW);
    	}
	else
	{
		/* preqtx points at the request in waiting callers stack */
		preqtx->protocol = (preqtx->device == SERIAL0) ? uart0_protocol:uart1_protocol;
		
		if ( (c = uart_tx_getchar(preqtx)) >= 0)
		{
			ClearEvent(preqtx->event);
			uart_tx_send(preqtx->device,(char)c);
			/* interrupt work */
			WaitEvent(preqtx->event);
		}
		else
		{
			preqtx->err = -1;
		}

		/* wake up write task */
		SetEvent(preqtx->task,preqtx->event);
		
		/* no tx request anymore */
		DisableAllInterrupts();
		preqtx = 0;
		EnableAllInterrupts();
	}
	
	TerminateTask();
}

/* what to do in the tx interrupt */
void tx_action(void)
{
	int c;
	if (preqtx)
	{
		if ( (c = uart_tx_getchar(preqtx))>=0 )
		{
			uart_tx_send(preqtx->device,(char)c);
		}
		else
		{
			SetEvent(tuarttx,preqtx->event);
		}
	}
}

static int uart_tx_getchar(ioreq* req)
{
	int c;
	switch (req->protocol) 
	{
		case UART_SLIP:
			c = tx_slip_action(req);
			break;
		case UART_SHELL:
			c = tx_shell_action(req);
			break;
		case UART_RAW:
			c =tx_raw_action(req);
			break;
		default:
			c = -1;
			break;
	}
	return c;
}

static int tx_slip_action(ioreq* req)
{
	/* not implemented */
        (void)req;
	return -1;
}

static int tx_shell_action(ioreq* req)
{
        return tx_raw_action(req);
}


static int tx_raw_action(ioreq* req) 
{
        if (req->pos == req->len)
        {
                return -1;
        }
	else
	{	
		return req->buf[req->pos++];
	}
}

/* same discussion as with 'preqtx' applies here */
static ioreq* preqrx;

static int uart_rx_putchar(ioreq*,char);

TASK (tuartrx)
{
    	if (E_OK != ReceiveMessage(mruartrx,&preqrx))
    	{
            	/* an io request has been lost */
        	ShutdownOS(E_ERR_IO_OFLW);
    	}
	else
	{
		/* preqrx points somewhere in the callers stack */
		preqrx->protocol = (preqrx->device == SERIAL0) ? uart0_protocol:uart1_protocol;
		
		/* preqrx points to an iorequest still on the 
		 * callers stack */
		ClearEvent(preqrx->event);
		WaitEvent(preqrx->event);
		/* interrupt work */

		/* wake up read task */
		SetEvent(preqrx->task,preqrx->event);			
		
		/* preqrx points somewhere in the callers stack */
		DisableAllInterrupts();
		preqrx = 0;
		EnableAllInterrupts();
	}
	TerminateTask();
	
}

/* what to do in the rx interrupt */
void rx_action(char c)
{
	if (preqrx)
	{
		if (uart_rx_putchar(preqrx,c))
		{
			SetEvent(tuartrx,preqrx->event);
		}
	}
}

	
static int uart_rx_putchar(ioreq* req, char c)
{
	int ret = 0;
	switch (req->protocol) 
	{
		case UART_SLIP:
			ret = rx_slip_action(req,c);
			break;
		case UART_SHELL:
			ret = rx_shell_action(req,c);
			break;
		case UART_RAW:
			ret = rx_raw_action(req,c);
			break;
		default: break;
	}
	return ret;
}

static int rx_slip_action(ioreq* req, char c)
{
	/* not implemented */
	(void)req;(void)c;
        return 1;
}

static int rx_shell_action(ioreq* req, char c)
{
        int  done = 0;

        switch (c)
        {
        case CONTROL('c'):
                req->err = -1;
                done = 1;
                break;
        case '\177':
        case '\b':
                if (req->pos > 0)
                {
                        --req->pos;
                }
                break;
        case '\r':
        case '\n':
                done = 1;
		break;
        default:
                req->buf[req->pos++] = c;
                if (req->pos == req->len)
                {
                        --req->pos;
                }
                break;
        }
	
	if(done)
	{
		/* strings comparisons in shell */
                req->buf[req->pos] = '\0';
	}
	
        return done;
}

static int rx_raw_action(ioreq* req, char c) 
{
        req->buf[req->pos++] = c;
        return req->pos == req->len;
}

#endif /* UART */

void uart_init(void)
{
	uint_least8_t i;
#if defined(UART)	
	
	/* configure uart devices */
	for (i=SERIAL0;i<NO_UART;i++)
	{
		devices[i].rxevent = euartrx;
		devices[i].rxmsg   = msuartrx;
		devices[i].txevent = euarttx;
		devices[i].txmsg   = msuarttx;
	}
	
	#if defined(DEMO)
	/* i assume SHELLIN, SHELLOUT = SERIAL0 */
	uart0_protocol = UART_SHELL;
	/* in case DEMOUT == SERIAL1 */
	uart1_protocol = UART_RAW;	
	#endif
	#if defined(TETRIS)
	/* in case TETRISIN, TETRISOUT = SERIAL0 */
	uart0_protocol = UART_RAW;
	#endif

	/* init serial */
	serial_init();
#else
	/* configure uart devices */
	for (i=SERIAL0;i<NO_UART;i++)
	{
		devices[i].rxevent = 0;
		devices[i].rxmsg   = -1;
		devices[i].txevent = 0;
		devices[i].txmsg   = -1;
	}
	
#endif

}

