#ifdef BUILD_TERMINAL

#ifndef __TERMINAL_H__
#define __TERMINAL_H__

#define SCREEN_X_OFF  0
#define SCREEN_X_W    SCREEN_X

#define TIMER_INIT_H  0xD0

#define GAME_TICK_HANDLER   terminal_tick_handler
#define GAME_INIT           terminal_init
#define GAME_MAIN           terminal_main

void terminal_tick_handler (void);
void terminal_init (void);
int terminal_main (void);

#define BUILD_KEYBOARD
#define BUILD_MOUSE
#define BUILD_SERIAL

#endif

#endif

