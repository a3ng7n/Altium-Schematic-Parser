/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   SRL0 device driver
|*
\*****************************************************************************/

#ifndef _WB_SRL0_H
#define _WB_SRL0_H

//..............................................................................
// Initialization routine. Must be called to setup the SRL0 controller
// This version of the device driver always uses 8 data bits, no parity,
// one stop bit, although other modes can be set by directly accessing the
// SRL0 core.
void          srl0_init          ( unsigned int base, unsigned int baudrate );

//..............................................................................
// These functions go straight to the controller. They do not queue
// or use interrupts.
unsigned char srl0_get_byte          ( unsigned int base );
unsigned char srl0_get_byte_blocking ( unsigned int base );
void          srl0_put_byte          ( unsigned int base, unsigned char val );
void          srl0_put_byte_blocking ( unsigned int base, unsigned char val );

//..............................................................................
// Receive / transmit buffers.
// These are used when interrupt driven serial communications are used.
// Application code should declare the buffers, set the buffer_size
// field and initialize the head, tail and busy fields to 0
typedef struct
{
    char * buffer;
    int    buffer_size;
    int    head;
    int    tail;
} srl0_receive_buffer_t;

typedef struct
{
    char * buffer;
    int    buffer_size;
    int    head;
    int    tail;
    int    busy;
} srl0_transmit_buffer_t;

//..............................................................................
// Send byte - send immediately if possible, otherwise queue in buffer and
// send in interrupt handler
void          srl0_send_byte        ( unsigned int base, unsigned char val, srl0_transmit_buffer_t *buffer );

//..............................................................................
// Receive byte - will return -1 if no value in buffer. Buffer is populated
// by interrupt handler
unsigned char srl0_receive_byte     ( srl0_receive_buffer_t *buffer );

//..............................................................................
// This is the main interrupt handler. The application code should setup the
// interrupt and call this function.
// The interrupt should be setup as edge triggered.
void          srl0_handle_interrupt ( unsigned int           base,
                                      srl0_transmit_buffer_t *transmit_buffer,
                                      srl0_receive_buffer_t  *receive_buffer);

#endif
