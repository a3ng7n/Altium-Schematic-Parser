#ifdef BUILD_TERMINAL

#include <string.h>
#include <stdlib.h>

#include "terminal.h"
#include "osdepend.h"

#define TERMINAL_TICK    10
#define TERMINAL_COLOUR  BRIGHT(COLOUR_GREEN)
#define TCSTR            "\x0A"
#define CURSOR_COLOUR    COLOUR_WHITE

/* global variables */

static volatile XDATA unsigned char tick = TERMINAL_TICK;

static char cursor_x = 0;
static char cursor_y = 0;
static char under_cursor = ' ';
static BIT caps = 0;
static BIT echo = 1;
static BIT crlf = 0;

static int mouse_x = 0;
static int mouse_y = 0;

/* routines */

static ROM char scan[] =
{
    // $00-0F
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x09, '`',  0x00,
    // $10-1F
    0x00, 0x00, 0x00, 0x00, 0x00, 'Q',  '1',  0x00,
    0x00, 0x00, 'Z',  'S',  'A',  'W',  '2',  0x00,
    // $20-2F
    0x00, 'C',  'X',  'D',  'E',  '4',  '3',  0x00,
    0x00, ' ',  'V',  'F',  'T',  'R',  '5',  0x00,
    // $30-3F
    0x00, 'N',  'B',  'H',  'G',  'Y',  '6',  0x00,
    0x00, 0x00, 'M',  'J',  'U',  '7',  '8',  0x00,
    // $40-4F
    0x00, ',',  'K',  'I',  'O',  '0',  '9',  0x00,
    0x00, '.',  0x00, 'L',  ';',  'P',  '-',  0x00,
    // $50-5F
    0x00, 0x00, '\'', 0x00, '[',  '=',  0x00, 0x00,
    0x00, 0x00, 0x0D, ']',  '\\', 0x00, 0x00, 0x00,
    // $60-6F
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00,
    0x00, '1',  0x00, '4',  '7',  0x00, 0x00, 0x00,
    // $70-7F
    '0',  '.' , '2',  '5',  '6',  '8',  0x1B, 0x00,
    0x00, '+',  '3',  '-',  '*',  '9',  0x00, 0x00,
};

int terminal_printat(int x, int y, ROM char *str)
{
    for (; *str; str++)
        PUTTILE (*str, x++, y);

    return (x);
}

static BIT t = 0;
static BIT mouse_lbtn_click = 0;

void terminal_putchar (char c)
{
    ET0 = 0;
    if (c == 0x08)
    {
       if (cursor_x > 0 || cursor_y > 1)
       {
          PUTTILE (under_cursor, cursor_x, cursor_y);
          if (--cursor_x < 0)
          {
             cursor_x = SCREEN_X_W - 1;
             cursor_y--;
          }
       }
    }
    else if (c == 0x0A)
    {
       PUTTILE (under_cursor, cursor_x, cursor_y);
       cursor_y++;
    }
    else if (c == 0x0D)
    {
       PUTTILE (under_cursor, cursor_x, cursor_y);
       cursor_x = 0;
    }
    else if (c < 0x20 || c > 0x7E)
    {
         // non-printable
         ET0 = 1;
         return;
    }
    else
    {
        PUTTILE (c, cursor_x++, cursor_y);
        if (cursor_x == 0x40)
        {
           cursor_x = 0;
           cursor_y++;
        }
    }

    if (cursor_y == SCREEN_Y-1)
    {
       int i;

       // need to scroll
       cursor_y--;
       for (i=SCREEN_X; i<SCREEN_X*(SCREEN_Y-2); i++)
           tileMap[i] = tileMap[i+SCREEN_X];
       for (; i<SCREEN_X*(SCREEN_Y-1); i++)
           tileMap[i] = ' ';
    }

    t = 0;
    ET0 = 1;
}

void terminal_tick_handler (void)
{
    if (--tick != 0)
        return;

    tick = TERMINAL_TICK;

    /* draw cursor */
    t = !t;
    if (t)
    {
       if (mouse_lbtn_click)
       {
          cursor_x = mouse_x >> 3;
          cursor_y = mouse_y >> 3;
          mouse_lbtn_click = 0;
       }
       under_cursor = tileMap [cursor_y * SCREEN_X + cursor_x];
    }
    PUTTILE ((t ? CURSOR_COLOUR : under_cursor), cursor_x, cursor_y);

    /* update mouse */
    PUTSPRITE (0, mouse_x, mouse_y);
}

static void update_status_bar (void)
{
    terminal_printat (2, SCREEN_Y-1, "9600-8-N-1");
    terminal_printat (48, SCREEN_Y-1, (echo ? "ECHO" : TCSTR TCSTR TCSTR TCSTR));
    terminal_printat (53, SCREEN_Y-1, (crlf ? "CRLF" : TCSTR TCSTR TCSTR TCSTR));
    terminal_printat (58, SCREEN_Y-1, (caps ? "CAPS" : "caps"));
}

void terminal_init (void)
{
    int i, j;

    /* hide the sprites */
    for (i=0; i<8; i++)
    {
       // this should be off the screen, but it doesn't work!
       PUTSPRITE (i, 255, (SCREEN_Y<<3));
    }

    // setup mouse pointer sprite
    SPRITE_N (0, 27);
    SPRITE_COLOUR (0, COLOUR_BLACK, BRIGHT(COLOUR_YELLOW), COLOUR_WHITE);

    /* create some pretty tiles */
    for (i=0; i<16; i++)
        for (j=0; j<32; j++)
            tile[i*32+j] = (i << 4) | i;

    // and let's make the font green
    for (; i<128; i++)
        for (j=0; j<32; j++)
            tile[i*32+j] &= (TERMINAL_COLOUR << 4) | TERMINAL_COLOUR;

    // draw bars top/bottom of screen
    for (i=0; i<SCREEN_X; i++)
    {
       PUTTILE (TERMINAL_COLOUR, i, 0);
       PUTTILE (TERMINAL_COLOUR, i, SCREEN_Y-1);
    }

    // clear the scrolling region
    for (i=1; i<SCREEN_Y-1; i++)
        for (j=0; j<SCREEN_X; j++)
            tileMap[i*SCREEN_X+j] = ' ';

    terminal_printat (2, 0, "[RESET]");
    terminal_printat (10, 0, "[ECHO]");
    terminal_printat (17, 0, "[CRLF]");

    terminal_printat (0, 3, "Altium Limited");
    terminal_printat (0, 4, "Altera Cyclone W/8051 core");
    terminal_printat (0, 5, "Powered by Nexar 2004");
    terminal_printat (0, 7, "EvalBoard Serial Terminal Emulator");

    cursor_y = 9;

    update_status_bar ();

    // reset the sprite collision latch
    *collide = 0;

    // init sound
    SOUND_DIR_UP;
}

#define RBUF_SIZE       128

static unsigned char txChar;
static XDATA unsigned char rbuf[RBUF_SIZE];
static unsigned int rs = 0;
static unsigned int re = 0;

/* Serial ISR */
__interrupt(__INTNO(4)) void Interrupt4IntHandler( void )
{
    if (RI)
    {
       if (((re+1) & (RBUF_SIZE-1)) != rs)
       {
          rbuf[re] = SBUF;
          re = (re + 1) & 31;
       }
       RI = 0;
    }

    if (TI)
    {
       TI = 0;
       if (txChar == 0x0D && crlf)
          SBUF = 0x0A;
       txChar = 0;
    }
}

static void handle_menu_bar (unsigned char x)
{
    if (x>=2 && x<=8)
       terminal_init ();
    else if (x>=10 && x<=15)
       echo = !echo;
    else if (x>=17 && x<=22)
       crlf = !crlf;
    update_status_bar ();
}

int terminal_main (void)
{
    unsigned char mouse_hysteresis = 0;
    char c;

    while (1)
    {
       if (lastKeyPressed == KEY_ESC)
          break;

       while (rs != re)
       {
             terminal_putchar (rbuf[rs]);
             if (rbuf[rs] == 0x0D && crlf)
             {
                under_cursor = tileMap [cursor_y * SCREEN_X + cursor_x];
                terminal_putchar (0x0A);
             }

             rs = (rs+1) & (RBUF_SIZE - 1);
       }

       if (lastKeyPressed)
       {
          c = scan[lastKeyPressed & 0x7F];

          if (lastKeyPressed == KEY_CAPS)
          {
             caps = !caps;
             update_status_bar ();
          }

          // convert to lowercase if req'd
          if (c >= 'A' && c <= 'Z')
             if ((caps ^ (key[KEY_LSHIFT] || key[KEY_RSHIFT])) == 0)
                c |= 0x20;

          if (c)
          {
             if (echo)
             {
                terminal_putchar (c);
                if (c == 0x0D && crlf)
                {
                   under_cursor = tileMap [cursor_y * SCREEN_X + cursor_x];
                   terminal_putchar (0x0A);
                }
             }
             txChar = c;
             SBUF = c;
          }

          lastKeyPressed = 0;
       }

       // add some hysteresis to the (dodgy) mouse input
       if (mouseAck)
          mouse_hysteresis = 2;
       else
       {
           if (mouse_hysteresis == 0)
              continue;
           else
               mouse_hysteresis--;
       }
       mouseAck = 0;

       // handle new mouse data
       if ((lastMouseData[MOUSE_DATA_BTN] & MOUSE_X_OV) == 0)
       {
          mouse_x += lastMouseData[MOUSE_DATA_X];
          if (mouse_x < 0)
             mouse_x = 0;
          else if (mouse_x >= (SCREEN_X<<3))
             mouse_x = (SCREEN_X<<3) - 2;
       }
       if ((lastMouseData[MOUSE_DATA_BTN] & MOUSE_Y_OV) == 0)
       {
          mouse_y -= lastMouseData[MOUSE_DATA_Y];
          if (mouse_y < 0)
             mouse_y = 0;
          else if (mouse_y >= (SCREEN_Y<<3))
             mouse_y = (SCREEN_Y<<3) - 1;
       }

       if (lastMouseData[MOUSE_DATA_BTN] & MOUSE_BTN_LT)
       {
          if (mouse_y < (1<<3))
             handle_menu_bar(mouse_x>>3);
          else if (mouse_y < ((SCREEN_Y-1)<<3))
             mouse_lbtn_click = 1;
       }
    }

    return (0);
}
#endif

