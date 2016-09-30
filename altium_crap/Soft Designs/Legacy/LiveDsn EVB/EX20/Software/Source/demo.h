#ifdef BUILD_DEMO

#ifndef __DEMO_H__
#define __DEMO_H__

#define SCREEN_X_OFF  0
#define SCREEN_X_W    64

#define BLANK_TILE    0x7F
#define TIMER_INIT_H  0x40

#define GAME_TICK_HANDLER   demo_tick_handler
#define GAME_INIT           demo_init
#define GAME_MAIN           demo_main

void demo_tick_handler (void);
void demo_init (void);
int demo_main (void);

#define BUILD_KEYBOARD
#ifndef __GNUC__
#define BUILD_MOUSE
#endif

#endif

#endif

