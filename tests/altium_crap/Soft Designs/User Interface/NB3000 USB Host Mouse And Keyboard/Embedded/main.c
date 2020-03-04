/*****************************************************************************\
|*
|*  IN PACKAGE:         Software Platform Builder
|*
|*  COPYRIGHT:          Copyright (c) 2009, Altium
|*
|*  DESCRIPTION:        Shows how to use the USB Host HID (mouse & keyboard) drivers
|*
 */

// Undefine to use the keyboarddriver directly without DEVIO
#define USE_KEYBOARD_DEVIO  1

//************************************************************

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <timing.h>
#include <interrupts.h>
#include <usbhost.h>

#if USE_KEYBOARD_DEVIO
#include <keyboard.h>
#else
#include <usbhost_keyboard.h>
#endif

#include "devices.h"

#include <pointer.h>

//************************************************************

#if USE_KEYBOARD_DEVIO

const int keycodes[] =
{
   '\a', '\b', '\t', '\n', '\v', '\f', '\r', '\033', ' ',
   0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B,
   0x8C, 0x8D, 0x8E, 0x8F,
   0x90, 0x91, 0x92, 0x93, 0x94, 0x95,
   0x96, 0x97, 0x98, 0x99, 0x9A,
   0x9B, 0x9C, 0x9D, 0x9E, 0x9F, 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5
};

const char *keynames[] =
{
    "BELL", "BACKSPACE", "TAB", "LF", "VERTTAB", "FORMFEED", "CR", "ESC", "SPACE",
    "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
    "UP", "DOWN", "LEFT", "RIGHT",
    "PGUP", "PGDN", "HOME", "END", "INS", "DEL",
    "PRSCR", "SCROLLOCK", "PAUSE", "NUMLOCK", "CAPSLOCK",
    "LSHIFT", "RSHIFT", "CTRL", "ALT", "LWIN", "RWIN", "MENU", "RELLSHIFT", "RELRSHIFT", "RELCTRL", "RELALT"
};

#else

usbhost_keyboard_t *usbhost_keyboard;
uint8_t leds = 0x00;

#define SCANCODE_CAPSLOCK    57
#define SCANCODE_SCROLLOCK   71
#define SCANCODE_NUMLOCK     83

#endif

pointer_t *pointer;

usbhost_t *usbhost;

void init(void);

//************************************************************

void main(void)
{
    init();

    for (;;)
    {
        usbhost_process(usbhost);

        pointer_state_t pointer_state;
        if (pointer_update(pointer, &pointer_state))
        {
            printf("b=%02X x=%04i y=%04i\n", pointer_state.buttons, pointer_state.x, pointer_state.y);
        }

#if USE_KEYBOARD_DEVIO
        int ch = getchar();
        if (ch != -1)
        {
            int keycode;
            for (keycode = 0; keycodes[keycode]; ++keycode)
            {
                if (keycodes[keycode] == ch)
                {
                    printf("%s %i 0x%02X\n", keynames[keycode], ch, ch);
                    break;
                }
            }
            if (!keycodes[keycode]) printf("%c %i 0x%02X\n", ch, ch, ch);
        }
#else
        uint16_t keyboard_scancode;
        keyboard_scancode = usbhost_keyboard_get_scancode(usbhost_keyboard);
        if (keyboard_scancode != 0xFFFF)
        {
            printf("k=%04X\n", keyboard_scancode);
            switch (keyboard_scancode)
            {
            case SCANCODE_CAPSLOCK:
                leds ^= USBHOST_KEYBOARD_LED_CAPSLOCK;
                usbhost_keyboard_setleds(usbhost_keyboard, leds);
                break;

            case SCANCODE_NUMLOCK:
                leds ^= USBHOST_KEYBOARD_LED_NUMLOCK;
                usbhost_keyboard_setleds(usbhost_keyboard, leds);
                break;

            case SCANCODE_SCROLLOCK:
                leds ^= USBHOST_KEYBOARD_LED_SCROLLOCK;
                usbhost_keyboard_setleds(usbhost_keyboard, leds);
                break;

            }
        }
#endif
    }
}


void init(void)
{
    printf("USB host HID (mouse & keyboard) demo\n\n");

    pointer = pointer_open(POINTER_1);

#if USE_KEYBOARD_DEVIO
    int fd = fileno(stdin);
    posix_devctl_keyboard_blocking_t keyboard_blocking;
    keyboard_blocking.blocking = false;
    posix_devctl(fd, DEVCTL_KEYBOARD_BLOCKING, (void*) &keyboard_blocking, sizeof(keyboard_blocking), NULL);
#else
    usbhost_keyboard = usbhost_keyboard_open(USBHOST_KEYBOARD_1);
#endif

    usbhost = usbhost_open(USBHOST_1);
}


