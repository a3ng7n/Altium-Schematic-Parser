#ifdef BUILD_BOCMAN

#ifndef __PACMAN_H__
#define __PACMAN_H__

#define SCREEN_X_OFF  0
#ifdef MULTI_MCU
#define SCREEN_X_W    32
#else
#define SCREEN_X_W    SCREEN_X
#endif

#define TIMER_INIT_H  0xD0

#define GAME_TICK_HANDLER   pacman_tick_handler
#define GAME_INIT           pacman_init
#define GAME_MAIN           pacman_main

void pacman_tick_handler (void);
void pacman_init (void);
int pacman_main (void);

#define BUILD_KEYBOARD
#ifndef MULTI_MCU
#define BUILD_LCD
#endif

#endif

#endif

