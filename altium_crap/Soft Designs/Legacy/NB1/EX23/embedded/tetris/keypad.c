/******************************************************************************
 * FILE:       @(#)keypad.c	1.4 04/08/23
 * DESCRIPTION:                                      
 *        Keypad OSEK device driver source           
 *****************************************************************************/
#if defined(DEMO)
#include "demo.h"
#else
#include "tetris.h"
#endif

#if defined(NEXAR)

#include <osek/osek.h>

/* polling time */
#define KEYPAD_SENSORTIME   60

#define KEY_IN      P1
#define KEY_BIT0    0
#define KEY_VALID   P1_7
#define KEY_RESET   P1_0

typedef unsigned char KEY;

#define KEYPAD_POLLTIME   KEYPAD_SENSORTIME/(OSTICKDURATIONINMSCS)

DeclareAlarm(akeypad);
DeclareEvent(ekeypadrx);
DeclareMessage(mskeypadrx);
DeclareMessage(mrkeypadrx);

static void DelayTask(TickType time);

TASK (tkeypadrx)
{
    ioreq* preqrx;

    unsigned char knew;
    unsigned char kold=0;

    if (E_OK != ReceiveMessage(mrkeypadrx,&preqrx))
    {
             /* an io request has been lost */
          ShutdownOS(E_ERR_IO_OFLW);
       }
    else if ( preqrx->len != 1 )
    {
       preqrx->err = -1;
    }
    else
    {
       while(1)
       {
             if ( KEY_VALID )
             {
                	knew = (( KEY_IN & ( 0x0F << KEY_BIT0 )) >> KEY_BIT0 ) + 1;
                	KEY_RESET = 1;
                	if ( kold != knew)
                	{
                	        preqrx->buf[preqrx->pos] = knew;
                		SetEvent(preqrx->task,preqrx->event);
          			KEY_RESET = 0;
          			break;
                	}
             }
             else
             {
             		kold = 0;
       			KEY_RESET = 0;
             }
         
	     DelayTask(KEYPAD_POLLTIME);
       
       }  // while
    }
       
    TerminateTask();
}

static void DelayTask(TickType time)
{
       SetRelAlarm(akeypad,time,0);
       WaitEvent(ekeypadrx);
       ClearEvent(ekeypadrx);
       return;
}

#endif


void keypad_init ( void )
{
    
    /* init the lcd device */
    devices[KEYPAD].txevent = 0;
    devices[KEYPAD].txmsg   = -1;
#if defined(NEXAR)
    devices[KEYPAD].rxevent = ekeypadrx;
    devices[KEYPAD].rxmsg   = mskeypadrx;
#else
    devices[KEYPAD].rxevent = 0;
    devices[KEYPAD].rxmsg   = -1;
#endif    
}



