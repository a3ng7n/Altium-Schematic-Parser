/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   PS/2 IO redirection
|*
\*****************************************************************************/

#ifndef __IO_WB_PS2__
#define __IO_WB_PS2__

#include "wb_ps2.h"

#define SHIFT_SCRL  0x01
#define SHIFT_NUM   0x02
#define SHIFT_CAPS  0x04
#define SHIFT_LEFT  0x10
#define SHIFT_RIGHT 0x20
#define SHIFT_CTRL  0x40
#define SHIFT_ALT   0x80

extern unsigned char shift_state;

int ps2_read( unsigned int base, ps2_buffer *buffer, char * buf, int size );

#endif
