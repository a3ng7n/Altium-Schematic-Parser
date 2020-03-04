
#ifndef __COMMON_H
#define __COMMON_H

/* system includes */
#include <time.h>
#include <stdint.h>


/* I/O ports */
#define IOPORT           ((volatile uint8_t *) SW_IO)
#define LEDS             IOPORT[0]
#define PUSH_BUTTONS     IOPORT[1]

#define PUSH_BUTTON_1    0x01
#define PUSH_BUTTON_2    0x02
#define PUSH_BUTTON_3    0x04
#define PUSH_BUTTON_4    0x08


//..............................................................................
#define PUSH_BUTTON_1_DOWN    ((~PUSH_BUTTONS) & PUSH_BUTTON_1)
#define PUSH_BUTTON_2_DOWN    ((~PUSH_BUTTONS) & PUSH_BUTTON_2)
#define PUSH_BUTTON_3_DOWN    ((~PUSH_BUTTONS) & PUSH_BUTTON_3)
#define PUSH_BUTTON_4_DOWN    ((~PUSH_BUTTONS) & PUSH_BUTTON_4)
//..............................................................................


#endif /* !defined(__COMMON_H) */




