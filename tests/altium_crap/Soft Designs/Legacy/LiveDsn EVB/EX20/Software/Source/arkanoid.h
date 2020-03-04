#ifdef BUILD_BOCANOID

#ifndef __ARKANOID_H__
#define __ARKANOID_H__

#ifdef MULTI_MCU
#define SCREEN_X_OFF  32
#define SCREEN_X_W    32
#else
#define SCREEN_X_OFF  0
#define SCREEN_X_W    SCREEN_X
#endif

#define TIMER_INIT_H  0xC0

#define GAME_TICK_HANDLER   arkanoid_tick_handler
#define GAME_INIT           arkanoid_init
#define GAME_MAIN           arkanoid_main

void arkanoid_tick_handler (void);
void arkanoid_init (void);
int arkanoid_main (void);

#ifndef MULTI_MCU
#define BUILD_KEYBOARD
#endif
#ifndef __GNUC__
#define BUILD_MOUSE
#endif

#endif

#endif

