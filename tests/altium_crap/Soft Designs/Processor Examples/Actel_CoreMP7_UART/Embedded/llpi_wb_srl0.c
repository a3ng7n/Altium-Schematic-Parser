/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   SRL0 device driver
|*
\*****************************************************************************/

#include "llpi_util_timing.h"
#include "llpi_wb_srl0.h"
#include "llpi_common.h"

/* UART hardware definitions */

#define UART_BASE(x) ((volatile unsigned char *) x)

#define UART_PCON(x)   UART_BASE(x)[0]
#define UART_SCON(x)   UART_BASE(x)[1]
#define UART_SBUF(x)   UART_BASE(x)[2]
#define UART_SRELL(x)  UART_BASE(x)[3]
#define UART_SRELH(x)  UART_BASE(x)[4]
#define UART_TCON(x)   UART_BASE(x)[5]
#define UART_TL(x)     UART_BASE(x)[6]
#define UART_TH(x)     UART_BASE(x)[7]
#define UART_ADCON(x)  UART_BASE(x)[8]

#define GET_UART_TI(x)     (UART_SCON(x)&2)
#define SET_UART_TI(x, b)  (UART_SCON(x)=(UART_SCON(x) & 0xFD) | (b << 1))

#define GET_UART_RI(x)     (UART_SCON(x)&1)
#define SET_UART_RI(x, b)  (UART_SCON(x)=(UART_SCON(x) & 0xFE) | b )

void srl0_init( unsigned int base, unsigned int baudrate )
{
    unsigned short baud_calc;
    UART_TCON(base)  = 0x00; // 0b00000000
                             //    |
                             //    `-------- Timer Run Control = 0: stop
    UART_SCON(base)  = 0x50; // 0b01010000
                             //   || |  ||
                             //   || |  |`-- RI = 0: clear receiver interrupt
                             //   || |  `--- TI = 0: clear transmitter interrupt
                             //   || `------ REN = 1: enable receiver
                             //   ``-------- SMOD = 01: mode 1 (8 bit UART with variable baudrate)
    UART_ADCON(base) = 0x80; // 0b10000000
                             //   |
                             //   `--------- BD = 1: use internal baudrate generator
    UART_PCON(base)  = 0x00; // 0b00000000
                             //   |
                             //   `--------- SMOD = 0: Do not double baudrate
    baud_calc = (timing_get_clock_freq_hz() / (baudrate * 64 + baudrate / 2));
    UART_SRELL(base) = (1024 - baud_calc) & 0xFF;
    UART_SRELH(base) = ((1024 - baud_calc) >> 8) & 0xFF;
    UART_TCON(base)  = 0x40; // Start timer run control
}

void srl0_put_byte( unsigned int base, unsigned char val )
{
    SET_UART_TI(base, 0);
    UART_SBUF(base) = val;
}

void srl0_put_byte_blocking( unsigned int base, unsigned char val )
{
    SET_UART_TI(base, 0);
    UART_SBUF(base) = val;
    while( !GET_UART_TI(base) ) ;
}

unsigned char srl0_get_byte( unsigned int base )
{
    unsigned char retval = -1;
    if ( GET_UART_RI(base) )
    {
        retval = UART_SBUF(base);
        SET_UART_RI(base, 0);
    }
    return retval;
}

unsigned char srl0_get_byte_blocking( unsigned int base )
{
    unsigned char retval;
    while (! GET_UART_RI(base))
    {
    }
    retval = UART_SBUF(base);
    SET_UART_RI(base, 0);
    return retval;
}

int srl0_receive_byte(srl0_receive_buffer_t *buffer, char *buf)
{
	int retval;

	if ( buffer->head != buffer->tail )
	{
		*buf = buffer->buffer[buffer->tail++];
		if ( buffer->tail == buffer->buffer_size )
		{
			buffer->tail = 0;
		}
		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}


void srl0_send_byte( unsigned int base, unsigned char val, srl0_transmit_buffer_t *buffer )
{
    unsigned short next;
    next = buffer->head + 1;
    if ( next == buffer->buffer_size ) next = 0;
    while ( next == buffer->tail ) __asm( "nop" );
    buffer->buffer[buffer->head] = val;
    if ( buffer->busy )
    {
        buffer->head = next;
    }
    else
    {
        /* Start the chain */
        buffer->busy = 1;
        UART_SBUF(base) = val;
    }
}

void srl0_handle_interrupt ( unsigned int base, srl0_transmit_buffer_t *transmit_buffer, srl0_receive_buffer_t *receive_buffer)
{
    if ( GET_UART_RI(base) )
    {
        SET_UART_RI(base, 0);
        register unsigned short next;
        next = receive_buffer->head + 1;
        if ( next == receive_buffer->buffer_size ) next = 0;

        if ( next != receive_buffer->tail )
        {
            receive_buffer->buffer[receive_buffer->head] = UART_SBUF(base);
            receive_buffer->head = next;
        }
    }
    if ( GET_UART_TI(base) )
    {
        SET_UART_TI(base, 0);
        if ( transmit_buffer->tail == transmit_buffer->head )
        {
            transmit_buffer->busy = 0;
        }
        else
        {
            UART_SBUF(base) = transmit_buffer->buffer[transmit_buffer->tail++];
            if ( transmit_buffer->tail == transmit_buffer->buffer_size ) transmit_buffer->tail = 0;
            transmit_buffer->busy = 1;
        }
    }
}

