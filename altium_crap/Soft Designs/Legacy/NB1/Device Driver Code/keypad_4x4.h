/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   4 x 4 keypad device driver
|*
\*****************************************************************************/

#ifndef __KEYPAD_4X4_H__
#define __KEYPAD_4X4_H__

unsigned char * keypad_4x4_get_ascii_lookup_table        ( void );
void            keypad_4x4_set_ascii_lookup_table        ( unsigned char * table );

unsigned int    keypad_4x4_get_enable_repeat             ( void );
void            keypad_4x4_set_enable_repeat             ( unsigned int enabled );

unsigned int    keypad_4x4_get_repeat_delay_primary_ms   ( void );
void            keypad_4x4_set_repeat_delay_primary_ms   ( unsigned int ms );
unsigned int    keypad_4x4_get_repeat_delay_secondary_ms ( void );
void            keypad_4x4_set_repeat_delay_secondary_ms ( unsigned int ms );

void            keypad_4x4_reset                         ( unsigned int base );
unsigned char   keypad_4x4_get_current_key               ( unsigned int base );
int             keypad_4x4_is_key_down                   ( unsigned int base );

unsigned char   keypad_4x4_get_next_key                  ( unsigned int base );
int             keypad_4x4_read                          ( unsigned int base, char * buf, int size );

#endif

