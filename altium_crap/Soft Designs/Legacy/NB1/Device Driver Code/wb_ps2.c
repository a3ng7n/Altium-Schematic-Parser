/*****************************************************************************\
|*
|*  COPYRIGHT:      Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:    Device driver for PS/2 keyboard
|*
\*****************************************************************************/

#include <stdio.h>
#include "util_timing.h"
#include "wb_ps2.h"

/*
 * Local definitions, declarations and prototypes
 */

typedef struct
{
    unsigned char b7 : 1;
    unsigned char b6 : 1;
    unsigned char b5 : 1;
    unsigned char b4 : 1;
    unsigned char b3 : 1;
    unsigned char b2 : 1;
    unsigned char b1 : 1;
    unsigned char b0 : 1;
} bitstruct_t;

static int  ps2_waitfor( unsigned int base, ps2_buffer *buffer, unsigned char val );
static void ps2_send   ( unsigned int base, unsigned char val );
static int  ps2_getbyte( ps2_buffer *buffer );

/*
 * Conversion array scancode => basic key definition
 */

static const short keyval[] =
{
    -1, KEY_F9, -1, KEY_F5, KEY_F3, KEY_F1, KEY_F2, KEY_F12, -1, KEY_F10, KEY_F8, KEY_F6, KEY_F4, '\t', '`', -1,
    -1, KEY_ALT, KEY_LSHIFT, -1, KEY_CTRL, 'q', '1', -1, -1, -1, 'z', 's', 'a', 'w', '2', KEY_LWIN,
    -1, 'c', 'x', 'd', 'e', '4', '3', KEY_RWIN, -1, ' ', 'v', 'f', 't', 'r', '5', KEY_MENU,
    -1, 'n', 'b', 'h', 'g', 'y', '6', -1, -1, -1, 'm', 'j', 'u', '7', '8', -1,
    -1, ',', 'k', 'i', 'o', '0', '9', -1, -1, '.', '/', 'l', ';', 'p', '-', -1,
    -1, -1, '\'', -1, '[', '=', -1, -1, KEY_CAPSLOCK, KEY_RSHIFT, '\n', ']', -1, '\\', -1, -1,
    -1, -1, -1, -1, -1, -1, '\b',-1, -1, KEY_END, -1, KEY_LEFT, KEY_HOME, -1, -1, -1,
    KEY_INS, KEY_DEL, KEY_DOWN, '5', KEY_RIGHT, KEY_UP, '\x1B', KEY_NUMLOCK, KEY_F11, '+', KEY_PGDN, '-', '*', KEY_PGUP, KEY_SCROLLOCK, -1,
    -1, -1, -1, KEY_F7
} ;

/*
 * Driver definitions
 */

#define KBD_BASE(x) ((volatile unsigned char *) x )
#define KBD_BIT(x)  ((volatile bitstruct_t *) x )

#define KBD_CTRL(x)    KBD_BASE(x)[0]
#define KBD_DATA(x)    KBD_BASE(x)[1]
#define KBD_BUSY(x)    KBD_BIT(x)[0].b0
#define KBD_STROBE(x)  KBD_BIT(x)[0].b1

#define CMD_SETLED          0xED        // Set LEDs, must be followed by LED pattern
#define CMD_ECHO            0xEE        // Echo command, keyboard echoes "0xEE"
#define CMD_SELECT_SCANSET  0xF0        // Scan code set select 1, 2 or 3. 0 = Query
#define CMD_GETID           0xF2        // Keyboard response: 0xFA (ACK), than ID: 0x83 0xAB
#define CMD_SETREPEAT       0xF3        // Set repeat rate, keyboard respons 0xFA, host sends repeat info, keyboard response = ACK
#define CMD_ENABLE          0xF4        // Enable keyboard, response = ACK
#define CMD_DISABLE         0xF5        // Disable keyboard, response = ACK
#define CMD_RESEND          0xFE        // Resend request for last byte, response = last byte
#define CMD_RESET           0xFF        // Reset keyboard, response = 0xAA ("OK") or 0xFC ("FaileD")

#define RESP_BAT_PASSED     0xAA
#define RESP_BAT_FAILED     0xFC
#define RESP_ACK            0xFA

#define KBD_RESPONSE_TIMEOUT         100
#define KBD_BUSY_TIMEOUT             2000

static int ps2_waitfor( unsigned int base, ps2_buffer *buffer, unsigned char val )
{
    int retval = 0;
    unsigned long long start_tick;
    start_tick = timing_get_tick_count();
    do
    {
        if ( ps2_getbyte(buffer) == val )
        {
            retval = val;
            break;
        }
    }
    while( timing_elapsed_time_ms (start_tick) < KBD_RESPONSE_TIMEOUT );
    return retval;
}

static int ps2_wait_until_not_busy( unsigned int base )
{
    int retval = 0;
    unsigned long long start_tick;
    start_tick = timing_get_tick_count();
    do
    {
        if ( !KBD_BUSY(base) )
        {
            retval = 1;
            break;
        }
        retval++;
    }
    while( timing_elapsed_time_ms (start_tick) < KBD_BUSY_TIMEOUT );
    return retval;
}

static void ps2_send( unsigned int base, unsigned char val )
{
    if (ps2_wait_until_not_busy(base))
    {
        KBD_CTRL(base) = 0x00;
        KBD_DATA(base) = val;
        KBD_STROBE(base) = 1;
    }
}

static int ps2_getbyte( ps2_buffer *buffer )
{
    int retval = -1;
    if ( buffer->head != buffer->tail )
    {
        retval = buffer->buffer[buffer->tail++];
        if ( buffer->tail == buffer->buffer_size ) buffer->tail = 0;
    }
    return retval;
}

int ps2_init( unsigned int base, ps2_buffer *buffer )
{
    KBD_CTRL(base) = 0x00;
    KBD_DATA(base) = CMD_RESET;
    KBD_STROBE(base) = 1;

    return ps2_waitfor( base, buffer, RESP_BAT_PASSED );
}

int ps2_get_char( ps2_buffer *buffer )
{
    static int state = 0;
    int key;
    while( key = ps2_getbyte(buffer), key != -1 )
    {
        switch( state )
        {
        case 0 :
            switch( key )
            {
            case 0xF0 :     // Break code, check for shifts, otherwise ignore
                state = 1;
                break;
            case 0xE1 : // Pause / break, wait for complete string
                state = 2;
                break;
            case -1 :
                break;
            case 0xE0 :   // Extended code, return from standard table
                break;
            default :
                return keyval[key];
            }
            break;
        case 1 :    // Break code, ignore
            if ( key == 0xE0 )
            {
                break;   // Do not change, this is an extended release
            }
            state = 0; // Last character from release, continue with normal handling
            switch ( key )
            {
            case 0x14 : return KEY_RELCTRL;
            case 0x12 : return KEY_RELLSHIFT;
            case 0x59 : return KEY_RELRSHIFT;
            case 0x11 : return KEY_RELALT;
            }
            break;
        case 2 :
        case 3 :
        case 4 :
        case 5 :
        case 6 :
        case 7 :
            state++;
            break;
        case 8 :
            state = 0;
        }
    }
    return key;
}

void ps2_handle_interrupt( unsigned int base, ps2_buffer *buffer )
{
    unsigned short next = buffer->head + 1;
    if ( next == buffer->buffer_size ) next = 0;
    if ( next != buffer->tail )
    {
        buffer->buffer[buffer->head] = KBD_DATA(base);
        buffer->head = next;
    }
}

void ps2_set_leds( unsigned int base, ps2_buffer *buffer, unsigned char ledval )
{
    ps2_send( base, CMD_SETLED );
    if ( ps2_waitfor( base, buffer, RESP_ACK ) )
    {
        ps2_send( base, ledval );
        ps2_waitfor( base, buffer, RESP_ACK );
    }
}
