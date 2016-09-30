#include <regtsk51a.sfr>
#ifdef BUILD_BOCMAN
#include "pacman.h"
#endif
#ifdef BUILD_BOCANOID
#include "arkanoid.h"
#endif
#ifdef BUILD_DEMO
#include "demo.h"
#endif
#ifdef BUILD_TERMINAL
#include "terminal.h"
#endif
#include "osdepend.h"

XDATA char *reg = (XDATA char *)0x0000;
XDATA char *sprite = (XDATA char *)0x1000;
XDATA char *tileMap = (XDATA char *)0x2000;
XDATA char *tile = (XDATA char *)0x3000;
XDATA char *collide = (XDATA char *)0x4000;
#ifndef MULTI_MCU
XDATA char *sndReg = (XDATA char *)0x5000;
#endif

#ifdef BUILD_PS2_A

volatile signed char lastMouseData[] = { 0, 0, 0 };
volatile BIT mouseAck = 0;

#define MOUSE_STATE_RESET (0x100-(3+MOUSE_ACKS_AFTER_RESET))

// external interrupt 0 (mouse)
__interrupt(__INTNO(0)) void Interrupt2IntHandler( void )
{
    // expecting 3 acknowledgements before streaming data
    //static unsigned char state = MOUSE_STATE_RESET;
    static unsigned char state = 0xFF;

    unsigned char mouse = P2;
    
    //tileMap[count++] = (mouse>>4)>0x0a ? (mouse>>4) + 0x10 : (mouse>>4) + 'A' - 42;
    //tileMap[count++] = (mouse&0x0f)<0x0a ? (mouse&0x0f) + 0x10 : (mouse&0x0f) + 'A' - 42;
    //tileMap[count++] = (mouse>>4)>9 ? (mouse>>4) + 23 : (mouse>>4);
    //tileMap[count++] = (mouse&0x0f)>9 ? (mouse&0x0f) + 23 : (mouse&0x0f);
        
    if (state & 0x80)
    {
       // expect 3 bytes (FA AA 00) after the reset command
       if (state > (MOUSE_STATE_RESET+2))
       {       
          // ignore anything that is not an acknowledgement
          if (mouse != 0xFA)
             return;
       }
       if (state > MOUSE_STATE_RESET+1)
          mouseAck = 1;
       state++;
       
       return;
    }

    // bit3 of byte 0 should always be '1'
    if (state == 0)
    {
       if ((mouse & (1<<3)) == 0)
          return;
    }
    lastMouseData[state++] = mouse;
    if (state == 3)
    {
       state = 0;
       mouseAck = 1;
    }

    // flag that we have new mouse data
    P1 = lastMouseData[0];
}

#endif

#ifdef BUILD_PS2_B

volatile XDATA unsigned char key[128];
volatile BIT lastKeyEx = 0;
volatile XDATA unsigned char lastKeyPressed = 0;

// external interrupt 1 (keyboard)
__interrupt(__INTNO(2)) void Interrupt0IntHandler( void )
{
    static BIT breakCode = 0;
    static BIT exCode = 0;
    
    unsigned char scanCode = P3;

    // handle extended key
    if ((exCode == 0) && scanCode == EXKEY)
    {
       exCode = 1;
       return;
    }
    
    if ((breakCode == 0) && scanCode == BREAKCODE)
    {
       breakCode = 1;
       return;
    }

    // this is the make or break keyCode
    // - won't worry about extended keys atm

    // edge-detect a key-down
    if (!breakCode && key[scanCode] == 0)
    {
       lastKeyPressed = scanCode;
       lastKeyEx = exCode;
    }

    if (scanCode < 128)
       key[scanCode] = !breakCode;

    exCode = 0;
    breakCode = 0;
    
    P1 = lastKeyPressed;          // show the current key on the leds
}

#endif

// timer 0 overflow interrupt
__interrupt(__INTNO(1)) void Interrupt1IntHandler( void )
{
    // re-load timer
    TH0 = TIMER_INIT_H;
    TL0 = 0x00;

    GAME_TICK_HANDLER ();
}

void main(void)
{
    int   i, x, y;

    // IE SFR
    EA = 0;        // disable all interrupts
    IE = 0;
    ENABLE_SERIAL_INT;
    //ET1 = 1;       // enable timer 1 overflow interrupt
    ET0 = 1;       // enable timer 0 overflow interrupt
    ENABLE_MOUSE_INT;
    ENABLE_KEYBOARD_INT;

    // TMOD, TIMER0 SFRs
    // - setup game timer (10ms)
    TMOD = 0x21;   // timer 1 = 8-bit auto-reload, timer0 = 16-bit timer
    TH0 = TIMER_INIT_H;
    TL0 = 0x00;
    TR0 = 1;       // start timer running
        
    // TCON SFR    
    IT0 = 1;       // extern INT0 type falling edge
    IT1 = 1;       // extern INT1 type falling edge

    INIT_SERIAL_PORT;
    INIT_MOUSE_PORT;
    INIT_KEYBOARD_PORT;
    INIT_LCD;

    //clear the screen
    for (y=0; y<SCREEN_Y; y++)
    {
       i = y * SCREEN_X + SCREEN_X_OFF;
       for (x=0; x<SCREEN_X_W; x++)
          tileMap[i++] = BLANK_TILE;
    }
#if 0
    for (i=0; i<16; i++)
    {
       for (x=0; x<4*8; x++)
          tile[i*32+x] = (i<<4) | i;

       PUTTILE (i, i, 0);
    }
#else
    GAME_INIT ();
#endif

    // clear any pending external interrupts
    IE1 = 0;
    IE0 = 0;

    EA = 1;           // enable interrupts

    //INIT_MOUSE_COMMS;
#ifdef BUILD_MOUSE
    SEND_MOUSE_CMD (0xf4);
#endif

    GAME_MAIN ();

#ifdef BUILD_KEYBOARD
    while (lastKeyPressed != KEY_ESC)
          ;
#endif

    // reset the software
    __asm ("ljmp 0");
}


