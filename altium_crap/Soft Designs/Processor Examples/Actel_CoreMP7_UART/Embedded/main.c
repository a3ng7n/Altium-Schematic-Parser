/*****************************************************************************\
|*
|*  VERSION CONTROL:  $Version$   $Date$
|*
|*  IN PACKAGE:
|*
|*  COPYRIGHT:      Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:
|*
|*
|*
|*
\*****************************************************************************/

#include <stdio.h>
#include <string.h>

#include "hardware.h"
#include "llpi_wb_srl0.h"
#include "llpi_util_timing.h"

#define GPIO_BASE(base)                ((volatile unsigned char *) base)
#define GPIO_LED(base)                 GPIO_BASE(base)[0x0]
#define GPIO_SW(base)                  GPIO_BASE(base)[0x1]

#define LF                             0x0A
#define ESC                            0x1B
#define BS                             0x08
#define CR                             0x0D

#define STRING_SIZE                    80
#define UART_RXBUFSIZE                 256
#define UART_TXBUFSIZE                 256

#define CPSR_F_BIT                     0x40
#define CPSR_I_BIT                     0x80

static char rxbuf[UART_RXBUFSIZE];
srl0_receive_buffer_t receive_buffer;

static char txbuf[UART_TXBUFSIZE];
srl0_transmit_buffer_t transmit_buffer;

void arm7_enable_interrupts(void)
{
    int cpsr;
    __asm("mrs  %0,cpsr":"=r"(cpsr));
    cpsr &= ~(CPSR_I_BIT | CPSR_F_BIT );
    __asm("msr  cpsr_fsxc,%0"::"r"(cpsr));
    return;
}

void __interrupt_irq irq_handler( void )
{
    srl0_handle_interrupt(Base_UART, &transmit_buffer, &receive_buffer);
}

void initialize (void)
{
    GPIO_LED( Base_GPIO ) = 0x00;
    timing_set_clock_freq_hz(12 * 1000 * 1000);
    srl0_init( Base_UART, 9600 );

    receive_buffer.buffer       = rxbuf;
    receive_buffer.buffer_size  = UART_RXBUFSIZE;
    receive_buffer.head         = 0;
    receive_buffer.tail         = 0;

    transmit_buffer.buffer      = txbuf;
    transmit_buffer.buffer_size = UART_TXBUFSIZE;
    transmit_buffer.head        = 0;
    transmit_buffer.tail        = 0;
    transmit_buffer.busy        = 0;

    setbuf( stdout, NULL );
    setbuf( stdin,  NULL );         
    arm7_enable_interrupts();
}

void output_welcome( void )
{
    puts( "Actel CoreMP7 example, built " __DATE__ ", " __TIME__ "\r\n" );
}

void output_instructions( void )
{
    puts( "\r\ntype -write to write the character value on the LEDs (press ESC to exit)\r" );
    puts( "type -read to read back a line of text (80 characters maximum)\r" );
    puts( "press the ESC key to reset the example\r\n" );
}

void main( void )
{
    unsigned char c = 0;
    unsigned int  k = 0;
    char str_input[STRING_SIZE];
    char str_read_cmd[]  = "-read";
    char str_write_cmd[] = "-write";

    initialize();

    output_welcome();

    while (1)
    {
        output_instructions();

        gets( str_input );

        if ( strncmp(str_input, str_write_cmd, 6) == 0 )
        {
            c = getchar();
            while ( c != ESC )
            {
                GPIO_LED( Base_GPIO ) = c;
                c = getchar();
            }
        }
        else if (  strncmp(str_input, str_read_cmd, 5) == 0  )
        {
            gets( str_input );
            puts( str_input );
        }
    }
}

int _write( int fd, const char * buf, int size )
{
    for ( fd = 0; fd < size; fd++ )
    {
        srl0_send_byte( Base_UART, *buf++, &transmit_buffer );
    }
    return size;
}

int _read( int fd, char * buf, int size )
{
    char val;
    int i;
    for ( i = 0; i < size; i++ )
    {
        while ( srl0_receive_byte(&receive_buffer, &val) == 0 );
        buf[i] = val;
    }
    return i;
}
