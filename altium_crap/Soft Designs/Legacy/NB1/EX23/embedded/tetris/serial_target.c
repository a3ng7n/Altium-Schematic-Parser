/**************************************************************
**  FILE:	@(#)serial_target.c	1.1 04/08/19	      
**  DESCRIPTION:
**	Shell driver implementation: target file
**************************************************************/
#include <osek/osek.h>

#if defined(TETRIS)
# include "tetris.h"
#elif defined(DEMO)
# include "demo.h"
#endif

#if defined(UART)

#include "serial.h"

void serial_init(void)
{
	SCON 	= 0x50;
	TMOD    |= 0x20;          
	TR1     = 1;        
	TH1     = 0xF3 ;    
	ES 	= 1;
	PS 	= 1;
    return; 
}

char uart_rx_read(device uart) 
{
        if (uart == SERIAL0)
                return SBUF;
}

void    uart_tx_send(device uart,char c)
{
        if (uart == SERIAL0)
                SBUF = c;
}

ISR (i_uart0)
{  
	if (RI) 
        {
             /* read receive buffer */
             rx_action(uart_rx_read(SERIAL0)); 
	     RI = 0;
	}
	if (TI)
	{
	     TI = 0;
    	     tx_action();
	}
}

#endif






