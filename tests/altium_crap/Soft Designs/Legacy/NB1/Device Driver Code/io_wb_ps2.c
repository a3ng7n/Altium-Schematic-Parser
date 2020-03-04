/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   PS/2 IO redirection
|*
\*****************************************************************************/

#include "io_wb_ps2.h"
#include "wb_ps2.h"

static unsigned char shift_state = 0;

int ps2_read( unsigned int base, ps2_buffer *buffer, char * buf, int size )
{
    int i;
    for ( i = 0; i < size;  )
    {
        int val = ps2_get_char(buffer);

        switch( val )
        {
        case KEY_LSHIFT :
            shift_state |= SHIFT_LEFT;
            break;
        case KEY_RELLSHIFT :
            shift_state &= ~SHIFT_LEFT;
            break;
        case KEY_RSHIFT :
            shift_state |= SHIFT_RIGHT;
            break;
        case KEY_RELRSHIFT :
            shift_state &= ~SHIFT_RIGHT;
            break;
        case KEY_CTRL :
            shift_state |= SHIFT_CTRL;
            break;
        case KEY_RELCTRL :
            shift_state &= ~SHIFT_CTRL;
            break;
        case KEY_ALT :
            shift_state |= SHIFT_ALT;
            break;
        case KEY_RELALT :
            shift_state &= ~SHIFT_ALT;
            break;
        case KEY_CAPSLOCK :
            shift_state ^= SHIFT_CAPS;
            ps2_set_leds( base, buffer, shift_state & 0x07);
            break;
        case KEY_NUMLOCK :
            shift_state ^= SHIFT_NUM;
            ps2_set_leds( base, buffer, shift_state & 0x07);
            break;
        case KEY_SCROLLOCK :
            shift_state ^= SHIFT_SCRL;
            ps2_set_leds( base, buffer, shift_state & 0x07);
            break;
        }

        /* Handle shift state */

        if ( shift_state & (SHIFT_LEFT | SHIFT_RIGHT | SHIFT_CAPS ))
        {
            if ( val >= 'a' && val <= 'z' ) val -= ('a' - 'A');
        }
        if ( shift_state & (SHIFT_LEFT | SHIFT_RIGHT) )
        {
            unsigned char key[] =      "`1234567890-=[]\\;',./";
            unsigned char shiftkey[] = "~!@#$%^&*()_+{}|:\"<>?";
            for ( int i = 0; key[i]; i++ )
            {
                if ( val == key[i] )
                {
                    val = shiftkey[i];
                    break;
                }
            }
        }

        /* Handle numlock */

        if ( shift_state & SHIFT_NUM )
        {
            unsigned char key[] = { KEY_HOME, KEY_UP, KEY_PGUP, KEY_LEFT, KEY_RIGHT, KEY_END, KEY_DOWN, KEY_PGDN, KEY_INS, KEY_DEL } ;
            unsigned char numkey[] = "789461230.";
            for( int i = 0; key[i]; i++ )
            {
                if ( val == key[i] )
                {
                    val = numkey[i];
                    break;
                }
            }
        }

        if ( (val < 0) || (val > 0x7F) ) break;
        buf[i++] = val & 0xFF;
    }
    return i;
}

