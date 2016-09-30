/*****************************************************************************\
|*
|*  COPYRIGHT:      Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:    Device driver for PS/2 keyboard
|*
\*****************************************************************************/

#ifndef _WB_PS2_H
#define _WB_PS2_H

enum {
    KEY_F1 = 0x80, KEY_F2, KEY_F3, KEY_F4, KEY_F5, KEY_F6, KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_F11, KEY_F12,
    KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT, KEY_PGUP, KEY_PGDN, KEY_HOME, KEY_END, KEY_INS, KEY_DEL,
    KEY_PRSCR, KEY_SCROLLOCK, KEY_PAUSE, KEY_NUMLOCK, KEY_CAPSLOCK,
    KEY_LSHIFT, KEY_RSHIFT, KEY_CTRL, KEY_ALT, KEY_LWIN, KEY_RWIN, KEY_MENU,
    KEY_RELLSHIFT, KEY_RELRSHIFT, KEY_RELCTRL, KEY_RELALT

} ;

//..............................................................................
// Receive buffer for PS/2 device driver.
// This should be declared by application // code, the buffer_size field set
// and the head and tail fields initialized // to 0.
typedef struct
{
    short *buffer;
    int   buffer_size;
    int   head;
    int   tail;
} ps2_buffer;

//..............................................................................
// ps2_init returns 0 for failure and non-zero for success
int  ps2_init             ( unsigned int base, ps2_buffer *buffer );
void ps2_set_leds         ( unsigned int base, ps2_buffer *buffer, unsigned char ledval );
int  ps2_get_char         ( ps2_buffer *buffer);

//..............................................................................
// PS/2 interrupt handler. Application code should setup the interrupts and
// call function in the interrupt handler. Interrupts should be edge triggered.
void ps2_handle_interrupt ( unsigned int base, ps2_buffer *buffer);

#endif

