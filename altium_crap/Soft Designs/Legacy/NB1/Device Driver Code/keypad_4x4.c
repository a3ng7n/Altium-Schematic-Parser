/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   4 x 4 keypad device driver
|*
\*****************************************************************************/

#include "util_timing.h"
#include "keypad_4x4.h"

#define KEYPAD_BASE(base)   ((volatile unsigned char*) base)
#define KEYPAD_DATA(base)   KEYPAD_BASE(base)[0]
#define KEYPAD_STATUS(base) KEYPAD_BASE(base)[1]

static const unsigned char default_lookup[16] =
    {'1', '2', '3', 'C',
     '4', '5', '6', 'D',
     '7', '8', '9', 'E',
     'A', '0', 'B', 'F'};

unsigned char * keypad_4x4_asciilookup = default_lookup;

static unsigned int keypad_4x4_enable_repeat          = 1;
static unsigned int keypad_4x4_repeat_delay_primary   = 750;
static unsigned int keypad_4x4_repeat_delay_secondary = 250;

#define KEYPAD_DEBOUNCE 50

unsigned char * keypad_4x4_get_ascii_lookup_table ( void )
{
    return keypad_4x4_asciilookup;
}

void keypad_4x4_set_ascii_lookup_table ( unsigned char * table )
{
    keypad_4x4_asciilookup = table;
}

unsigned int keypad_4x4_get_enable_repeat ( void )
{
    return keypad_4x4_enable_repeat;
}

void keypad_4x4_set_enable_repeat ( unsigned int enabled )
{
    keypad_4x4_enable_repeat = enabled;
}

unsigned int keypad_4x4_get_repeat_delay_primary_ms ( void )
{
    return keypad_4x4_repeat_delay_primary;
}

void keypad_4x4_set_repeat_delay_primary_ms ( unsigned int ms )
{
    keypad_4x4_repeat_delay_primary = ms;
}

unsigned int keypad_4x4_get_repeat_delay_secondary_ms ( void )
{
    return keypad_4x4_repeat_delay_secondary;
}

void keypad_4x4_set_repeat_delay_secondary_ms ( unsigned int ms )
{
    keypad_4x4_repeat_delay_secondary = ms;
}

void keypad_4x4_reset ( unsigned int base )
{
    KEYPAD_DATA(base) = 0;
}

unsigned char keypad_4x4_get_current_key ( unsigned int base )
{
    unsigned char lastkey = 0;
    if (keypad_4x4_is_key_down(base))
    {
        lastkey = KEYPAD_DATA(base);
        keypad_4x4_reset(base);
    }

    return(lastkey);
}

int keypad_4x4_is_key_down ( unsigned int base )
{
    return(KEYPAD_STATUS(base));
}

unsigned long long last_key_down_tick = 0;
unsigned char      last_key           = 0xFF;
unsigned int       no_repeats_yet     = 0;

unsigned char keypad_4x4_get_next_key ( unsigned int base )
{
    unsigned char      next_key  = 0;
    unsigned long long this_tick = 0;
    unsigned long long tick_diff;
    while (1)
    {
        timing_delay_ms(KEYPAD_DEBOUNCE);
        if (keypad_4x4_is_key_down (base))
        {
            next_key  = keypad_4x4_get_current_key (base);
            this_tick = timing_get_tick_count();
            tick_diff = this_tick - last_key_down_tick;
            if (last_key == 0xFF)
            {
                no_repeats_yet = 1;
                break;
            }
            else if ( !keypad_4x4_enable_repeat )
            {
            }
            else if ( no_repeats_yet && (tick_diff > timing_get_ticks_ms (keypad_4x4_repeat_delay_primary)))
            {
                no_repeats_yet = 0;
                break;
            }
            else if (!no_repeats_yet && (tick_diff > timing_get_ticks_ms (keypad_4x4_repeat_delay_secondary)))
            {
                break;
            }
        }
        else
        {
            last_key = 0xFF;
        }
    }
    last_key           = next_key;
    last_key_down_tick = this_tick;
    return last_key;
}

int keypad_4x4_read( unsigned int base, char * buf, int size )
{
    *buf++ = keypad_4x4_asciilookup[keypad_4x4_get_next_key(base) & 0x0F];
    return 1;
}

