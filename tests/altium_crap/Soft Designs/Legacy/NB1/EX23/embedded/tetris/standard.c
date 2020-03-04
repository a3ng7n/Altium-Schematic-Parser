/******************************************************************************
**  FILE:   	@(#)standard.c	1.2 04/09/21
**  DESCRIPTION:
**  		integration of stadio in our io component.
******************************************************************************/

#if defined(DEMO)
#include "demo.h"
#endif
#if defined(TETRIS)
#include "tetris.h"
#endif

#if defined(STDIOTX)
#include <osek/osek.h>
#include <stdio.h>
#include <string.h>

DeclareMessage(msstdiotx);
DeclareMessage(mrstdiotx);
DeclareEvent(estdiotx);

TASK (tstdiotx)
{

	ioreq* preqtx;
    	
	if (E_OK != ReceiveMessage(mrstdiotx,&preqtx))
    	{
            	/* an io request has been lost */
        	ShutdownOS(E_ERR_IO_OFLW);
    	}
	else
	{
		if (preqtx->mode == IO_WRITE)
		{
			printf("%s",preqtx->buf);
			preqtx->pos = strlen(preqtx->buf);
		}
		else if (preqtx->mode == IO_FLUSH)
		{
			fflush(stdout);
		}
		else
		{
			preqtx->err = -1;
		}
	}
	
	/* wake up read task */
	SetEvent(preqtx->task,preqtx->event);		
	
	TerminateTask();
}
#endif


void standard_init(void)
{

	/* configure standard device */
	devices[STDIO].rxevent = 0;
	devices[STDIO].rxmsg   = -1;
#if defined(STDIOTX)	
	devices[STDIO].txevent = estdiotx;
	devices[STDIO].txmsg   = msstdiotx;
#else
	devices[STDIO].txevent = 0;
	devices[STDIO].txmsg   = -1;
#endif
}


