/******************************************************************************
 *  FILE:   @(#)output.c	1.10 04/09/21
 *  DESCRIPTION:
 * 	io sw component
 ******************************************************************************/
#include <osek/osek.h>
#include <string.h>
#include "mytypes.h"

#if defined(DEMO)
#include "demo.h"
#endif
#if defined(TETRIS)
#include "tetris.h"
#endif

#if defined(STDIOTX)
#include <stdio.h>
#endif

/* devices configuration */
device_s devices[NO_DEVICES==0?1:NO_DEVICES];

int	outs(device out, char* txt)
{
	int 		ret 	= -1;
	/* io request on the stack */
	ioreq 		req;
	/* request address */
	ioreq*  	msg     = &req;
	
	if ( out >= 0 && out < NO_DEVICES && devices[out].txmsg != -1 )
	{
		/* build tx io request */
		req.len 	= strlen( txt );
		req.buf 	= txt;
		req.pos 	= 0;
		req.err 	= 0;
		req.mode        = IO_WRITE;
		GetTaskID(&req.task);
		req.event	= devices[out].txevent;
		req.device 	= out;
		ClearEvent(req.event);
		/* send tx io request */
		SendMessage(devices[out].txmsg,(void*)&msg);
		/* wait for completion */
		WaitEvent(req.event);
		/* return error or number of bytes written */
		ret = req.err ? -1 : req.pos; 
	}
	else
	{
		ret = -1;
	}
	return ret;
}

int	outflush(device out)
{
	int 		ret 	= -1;
	/* io request on the stack */
	ioreq 		req;
	/* request address */
	ioreq*  	msg     = &req;
	
	if ( out == STDIO && devices[STDIO].txmsg != -1 )
	{
		/* build tx io request */
		req.mode        = IO_FLUSH;
		GetTaskID(&req.task);
		req.event	= devices[out].txevent;
		req.device 	= out;
		ClearEvent(req.event);
		/* send tx io request */
		SendMessage(devices[out].txmsg,(void*)&msg);
		/* wait for completion */
		WaitEvent(req.event);
		/* return error or number of bytes written */
		ret = req.err ? -1 : req.pos; 
	}
	else
	{
		ret = -1;
	}
	return ret;
}

int 	ins(device in, char* txt, int nbytes)
{
	int 	ret 	= -1;
	/* io request on the stack */
	ioreq 	req;
	/* request address */
	ioreq*  msg     = &req;
	
	if ( in >= 0 && in < NO_DEVICES && devices[in].rxmsg != -1 )
	{
		/* build rx io request */
		req.len 	= nbytes;
		req.buf 	= txt;
		req.pos 	= 0;
		req.err 	= 0;
		req.mode        = IO_READ;
		GetTaskID(&req.task);
		req.event	= devices[in].rxevent ;
		req.device 	= in;
		ClearEvent(req.event);
		/* send rx io request */
		SendMessage(devices[in].rxmsg ,(void*)&msg);
		/* wait for completion */
		WaitEvent(req.event);
		/* return error or number of bytes written */
		ret = req.err ? -1 : req.pos;
	}
	else
	{
		ret = -1;
	}
	
	return ret;
}

void ioinit(void)
{
	uart_init();
	standard_init();
	keypad_init();
	lcd_init();
	simin_init();
}



