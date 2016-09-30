/*****************************************************************************
|*
|* PS/2 Example
|*
|* Keyboard functions:
|* - low level driver: scancodes
|* - device I/O (i.e. c library) interface: keyboard as stdin
|*
|* Mouse functions:
|* - polling mode
|* - streaming mode, with event callback
|*
|* NOTE:
|*      Only one keyboard interface can be used. To test lower interface,
|*      unlink the context from the keyboard stack in the SwPlatform file and
|*      adjust the defines below.
\*****************************************************************************/

// choose one of the keyboard interfaces below and adjust the SwPlatform file accordingly
#define USE_KEYBOARD_DEVIO      1
#define USE_PS2KB_DRIVER        0

#if ( USE_KEYBOARD_DEVIO == 1 )
  // stdio.h is our interface
#else
# include <drv_ps2kb.h>
#endif

#include <stdio.h>

#include <timing.h>
#include <drv_ps2mouse.h>

#include "devices.h"

// Save mouse packets
static ps2mouse_state_t mouse_state[100];

// Event synchronization
volatile uint8_t event_sync = 0;

// Event callback
static void mouse_event(ps2mouse_t *ps2mouse, ps2mouse_state_t *state, void *data)
{
    // Normally one would update program state here
    event_sync ^= 0x1;
}

extern int main(void)
{
#if ( USE_KEYBOARD_DEVIO == 1 )
    // we use stdin
#else
    ps2kb_t *kbd;
#endif
    ps2mouse_t *mouse;
    ps2mouse_state_t state;

#if ( USE_KEYBOARD_DEVIO == 1 )
    // In the SwPlatform file set the Posix name in the keyboard devio configuration to KEYBOARD_1
#else
    // low level keyboard driver
    kbd = ps2kb_open(DRV_PS2KB_1);
    if (!kbd)
    {
        return -1;
    }
#endif

    // mouse driver
    mouse = ps2mouse_open(PS2MOUSE);
    if (!mouse)
    {
        printf("Error opening mouse\n");
        return -2;
    }

#if ( USE_KEYBOARD_DEVIO == 1 )
    // Keyboard as stdin
    printf("POSIX device I/O Example\n");
    printf("- typing will print characters\n");
    printf("- type 'q' to quit\n");
    printf("Echo> ");

    while (1)
    {
        int ch = getchar();

        if (ch != -1)
        {
            putchar(ch);
            if (ch == 'q')
            {
                printf("\nFinished Keyboard Example\n\n");
                break;
            }
        }
    }
#else
    // Low level keyboard example
    printf("PS/2 Keyboard Driver Example\n");
    printf("- typing will print scancodes\n");
    printf("- type 'q' to quit\n");
    printf("Scancodes:\n");

    ps2kb_setleds(kbd, 0x0); // LED's off
    while (1)
    {
        const uint8_t *scancode = ps2kb_get_scancode(kbd);

        if (scancode[0] != 0)
        {
            printf("0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x\n",
                   scancode[0], scancode[1], scancode[2], scancode[3], scancode[4], scancode[5], scancode[6], scancode[7]);
            if (scancode[0] == 0x15) // 'q'
            {
                printf("\nFinished Keyboard Example\n\n");
                break;
            }
        }
    }
    ps2kb_setleds(kbd, 0x7); // LED's on
#endif

    // PS/2 mouse in polling mode
    printf("PS/2 Mouse Test 1: polling mode\n");
    printf("- move the mouse or press a button\n");

    ps2mouse_set_streaming(mouse, false); // set remote mode
    // collect packets
    for (int i = 0; i < 100; i++)
    {
        while (ps2mouse_get_state(mouse, &state) < 0) /**/;

        mouse_state[i] = state;
        delay_ms(50);
    }
    // print packets
    for (int i = 0; i < 100; i++)
    {
        ps2mouse_state_t *state = &mouse_state[i];
        printf("(x, y) buttons = (%d, %d) 0x%x\n",
               state->xmov, state->ymov, state->buttons);
    }
    printf("\nFinished Mouse Test 1!\n\n");

    // PS/2 mouse in streaming mode with event callback
    printf("PS/2 Mouse Test 2: event callback\n");
    printf("- move the mouse or press a button\n");

    ps2mouse_install_callback(mouse, mouse_event, &state, NULL);
    ps2mouse_set_streaming(mouse, true); // set streaming mode
    // collect packets
    for (int i = 0; i < 100; i++)
    {
        uint8_t x = event_sync;

        while (x == event_sync) /* wait for event */;

        mouse_state[i] = state;
    }
    ps2mouse_set_streaming(mouse, false); // stop interrupts
    // print packets
    for (int i = 0; i < 100; i++)
    {
        ps2mouse_state_t *state = &mouse_state[i];
        printf("(x, y) buttons = (%d, %d) 0x%x\n",
               state->xmov, state->ymov, state->buttons);
    }
    printf("\n\nFinished Mouse Test 2!\n\n");

    printf("Finished all tests.\n");
    return 0;
}
