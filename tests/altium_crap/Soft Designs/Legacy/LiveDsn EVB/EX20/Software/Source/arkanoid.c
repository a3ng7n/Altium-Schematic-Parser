#ifdef BUILD_BOCANOID

#include <string.h>
#include <stdlib.h>

#include "arkanoid.h"
#include "osdepend.h"

#define ENABLE_BAT

#ifdef MULTI_MCU
#define XOFF 34
#else
#define XOFF 2
#endif
#define YOFF 14

#define BOARD_Y     31
#define BOARD_X     15

#define ARKANOID_TICK 2

/* global variables */

static XDATA unsigned char board[BOARD_Y][BOARD_X];
static XDATA unsigned char score[4] = { 0, 0, 0, 0 };
static volatile char inputs = 0;
static volatile BIT freeze = 1;

/* routines */

static void arkanoid_player_init (void);

static int arkanoid_printat(int x, int y, ROM char *str)
{
    unsigned char c;
    for (; *str; str++)
    {
        c = *str;
        if (*str >= '0' && *str <= ':')
            c = *str - '0' + 0x40;
        else
        if (*str >= 'A' && *str <= '[')
            c = *str - 'A' + 0x21;
        
        PUTTILE (c, x++, y);
    }
    
    return (x);
}

void arkanoid_printbyte(int x, int y, ROM char *str, unsigned char value)
{
    unsigned char c;    
    int x2 = arkanoid_printat(x, y, str);

    c = value >> 4;
    PUTTILE ((c>9 ? c+23 : c+0x40), x2++, y);
    c = value & 0x0f;
    PUTTILE ((c>9 ? c+23 : c+0x40), x2++, y);
}

void put_bg (int x, int y)
{
    //PUTTILE (0x70+(((x<<2)%3)<<2)+(y%0x03), XOFF+(x<<1)-1, YOFF+y+2);
    //PUTTILE (0x70+((((x<<2)+1)%3)<<2)+(y%0x03), XOFF+(x<<1), YOFF+y+2);
    PUTTILE (0x3C, XOFF+(x<<1)-1, YOFF+y+3);
    PUTTILE (0x3D, XOFF+(x<<1), YOFF+y+3);
}

void put_brick (int x, int y)
{
    int t = 0x10 + (board[y][x] << 1);

    PUTTILE (t,   XOFF+(x<<1)-1, YOFF+y+3);
    PUTTILE (t+1, XOFF+(x<<1),   YOFF+y+3);
}

static int ball_x;
static int ball_y;
static char ball_ox;
static char ball_oy;
static char ball_dx;
static char ball_dy;

static int bat_x;
static int bat_y;

#ifndef MULTI_MCU
void show_debug (void)
{
#ifdef BUILD_MOUSE
    arkanoid_printbyte (37, YOFF+ 3,      "B ", lastMouseData[MOUSE_DATA_BTN]);
    arkanoid_printbyte (37, YOFF+ 4,      "X ", lastMouseData[MOUSE_DATA_X]);
    arkanoid_printbyte (37, YOFF+ 5,      "Y ", lastMouseData[MOUSE_DATA_Y]);
#endif
    arkanoid_printbyte (32, YOFF+ 7, "BALL X ", ball_x);
    arkanoid_printbyte (37, YOFF+ 8,      "Y ", ball_y);
    arkanoid_printbyte (36, YOFF+ 9,     "DX ", ball_dx);
    arkanoid_printbyte (36, YOFF+10,     "DY ", ball_dy);
    arkanoid_printbyte (33, YOFF+12,  "BAT X ", bat_x);
    arkanoid_printbyte (37, YOFF+13,      "Y ", bat_y);
}
#endif

void add_score (unsigned char hundreds, unsigned char tens)
{
    BIT leading = 0;
    
    score[3] += tens;
    if (score[3] > 9)
    {
       score[3] -= 10;
       hundreds++;
    }

    score[2] += hundreds;
    if (score[2] > 9)
    {
       score[2] -= 10;
       if (++score[1] > 9)
       {
          score[1] -= 10;
          ++score[0];
       }
    }
    
    if ((leading = (score[0] > 0)))
       PUTTILE (score[0]+0x40, XOFF+2, YOFF+1);
    if (leading || (leading = score[1] > 0))
       PUTTILE (score[1]+0x40, XOFF+3, YOFF+1);
    if (leading || (leading = score[2] > 0))
       PUTTILE (score[2]+0x40, XOFF+4, YOFF+1);
    PUTTILE (score[3]+0x40, XOFF+5, YOFF+1);
}

#define BAT_EDGE   6

void arkanoid_tick_handler (void)
{
    static char tick = ARKANOID_TICK;

    XDATA unsigned char *b;
    int chk;

    if (freeze)
        return;
    
    if (--tick != 0)
        return;
        
    tick = ARKANOID_TICK;

    WIPESPRITE (2);
    WIPESPRITE (1);
    WIPESPRITE (0);
    
    chk = ball_y;
#ifdef ENABLE_BAT
    if (ball_dy > 0 && ball_oy + ball_dy > 2 && ball_y == bat_y - 3)
    {
       int bx = ((ball_x-1)<<4) + ball_ox;

       // the ball is at the correct height to hit the bat
        if (bx >= bat_x && bx < bat_x+28)
        {
          SOUND_DIR(0);
          PLAY_SOUND (0x0600, 0x0500);

          ball_dy = -ball_dy;

          // we've hit the bat here - work out an angle...
          if (ball_dx < 0)
          {
             // heading left and glancing lhs increases angle
             if (bx < bat_x+BAT_EDGE)
                ball_dx = -2;
             // heading left and glancing rhs decreases angle
             else if (bx > bat_x+(28-BAT_EDGE))
                ball_dx = -1;
          }
          else
          {
             // heading right and glancing lhs decreases angle
             if (bx < bat_x + BAT_EDGE)
                ball_dx = 1;
             // heading right and glancing rhs increases angle
             else if (bx > bat_x+(28-BAT_EDGE))
                ball_dx = 2;
          }
        }
    }
    else
#endif
    if (((ball_oy + ball_dy) > 2) && (board[ball_y+1][ball_x] != 0))
    {
#ifdef ENABLE_BAT
        if (ball_y == BOARD_Y-2)
        {
            freeze = 1;
            return;
        }
        else
#endif
            chk++;
    }
    else
    if (((ball_oy + ball_dy) < 0) && (board[ball_y-1][ball_x] != 0))
       chk--;

    b = &(board[chk][ball_x]);
    if (*b != 0)
    {
        ball_dy = -ball_dy;
        if (*b != 0xff)
        {
            SOUND_DIR_UP;
            PLAY_SOUND (0x600+(((int)*b)<<8), 0x700+(((int)*b)<<8));

            // shaded bricks require 2 hits
            if (*b == 7)
                (*b)--;
            else
            {
                // remove the block
                add_score (0, *b);
                *b = 0;
                put_bg (ball_x, chk);
            }
        }
        else
        {
           SOUND_DIR_UP;
           PLAY_SOUND (0x500, 0x600);
        }
    }

    chk = ball_x;
    if (ball_ox + ball_dx > 10 && board[ball_y][ball_x+1] != 0)
        chk++;
    else
    if (ball_ox + ball_dx < 0 && board[ball_y][ball_x-1] != 0)
        chk--;

    b = &(board[ball_y][chk]);
    if (*b != 0)
    {
        ball_dx = -ball_dx;
        if (*b != 0xff)
        {
           SOUND_DIR_UP;
           PLAY_SOUND (0x600+(((int)*b)<<8), 0x700+(((int)*b)<<8));

            // shaded bricks require 2 hits
            if (*b == 7)
                (*b)--;
            else
            {
                // remove the block
                add_score (0, *b);
                *b = 0;
                put_bg (chk, ball_y);
            }
        }
        else
        {
           SOUND_DIR_UP;
           PLAY_SOUND (0x500, 0x600);
        }
    }
    
    // move the ball
    ball_ox += ball_dx;
    ball_oy += ball_dy;
            
    if (ball_ox > 15)
    {
        ball_ox -= 16;
        ball_x++;
    }
    else
    if (ball_ox < 0)
    {
        ball_ox += 16;
        ball_x--;
    }

    if (ball_oy > 8)
    {
        ball_oy -= 8;
        ball_y++;
    }
    else
    if (ball_oy < 0)
    {
        ball_oy += 8;
        ball_y--;
    }

    PUTSPRITE (0, ((XOFF+0+(ball_x<<1))<<3)+ball_ox-8, ((YOFF+3+(ball_y))<<3)+ball_oy);
    
    // move the bat
    bat_x += (inputs << 1);
    if (bat_x < 0) bat_x = 0;
    else if (bat_x > (11*16-1)) bat_x = (11*16-1);
            
    PUTSPRITE (1, ((XOFF+1)<<3)+bat_x,    (YOFF+30)<<3);
    PUTSPRITE (2, ((XOFF+1)<<3)+16+bat_x, (YOFF+30)<<3);
}

static ROM unsigned char outline_top[] =
{
    0x03, 0x08, 0x08, 0x08, 0x08, 0x04, 0x05, 0x06, 0x07, 0x08, 0x08, 0x08, 0x08, 0x08,
    0x08, 0x08, 0x08, 0x08, 0x08, 0x04, 0x05, 0x06, 0x07, 0x08, 0x08, 0x08, 0x08, 0x09
};

static ROM unsigned char outline_side[] =
{
    0x02, 0x01, 0x00, 0x0e, 0x0e,
    0x02, 0x01, 0x00, 0x0e, 0x0e,
    0x02, 0x01, 0x00, 0x0e, 0x0e,
    0x02, 0x01, 0x00, 0x0e, 0x0e,
    0x02, 0x01, 0x00, 0x0e, 0x0e,
    0x02, 0x01, 0x00, 0x0e
};

void arkanoid_init (void)
{
    int x, y;

#ifdef MULTI_MCU
    arkanoid_printat (32, 3, "IMITED");
    arkanoid_printat (32, 4, " W:8051 CORE");
    arkanoid_printat (32, 5, "EXAR 2004");
#else
    arkanoid_printat(XOFF, 3, "             NEXAR");
    arkanoid_printat(XOFF, 5, "    EMBEDDED SYSTEMS ON FPGAS");
    arkanoid_printat(XOFF, 7, "         ALTIUM LIMITED");
    arkanoid_printat(XOFF, 8, "MAKING ELECTRONICS DESIGN EASIER");
#endif
    arkanoid_printat(XOFF,11, "      EVALBOARD BOCANOID");

    arkanoid_printat (XOFF+3, YOFF+0, "1UP");
    arkanoid_printat (XOFF+9, YOFF+0, "HIGH SCORE");
    arkanoid_printat (XOFF+5, YOFF+1, "00");
    arkanoid_printat (XOFF+12, YOFF+1, "50000");

    for (x=0; x<28; x++)
        PUTTILE (outline_top[x], XOFF+x, YOFF+3);
    for (y=0; y<29; y++)
    {
        PUTTILE (outline_side[y], XOFF+27, YOFF+4+y);
        PUTTILE (outline_side[y], XOFF, YOFF+4+y);
    }

    // init board
    for (y=0; y<BOARD_Y; y++)
    {
        for (x=0; x<BOARD_X; x++)
        {
            if ((x == 0) || (x == BOARD_X-1) || (y == 0) || (y == BOARD_Y-1))
                board[y][x] = 0xff;
            else
            if ((y < 5) || (y > 10))
                board[y][x] = 0;
            else
            {
                if (y == 5)
                    board[y][x] = 12 - y;
                else
                    board[y][x] = 11 - y;
            }
        }
    }

    // draw screen
    for (y=0; y<29; y++)
    {
        for (x=0; x<13; x++)
        {
            if (board[y+1][x+1] == 0)
                put_bg (x+1, y+1);
            else
                put_brick (x+1, y+1);
        }
    }

    // ball
    SPRITE_N(0,0x1C);
    SPRITE_FLIP(0,0,0);
    SPRITE_COLOUR(0,COLOUR_BLACK,0,COLOUR_WHITE);
    
    // bat
    SPRITE_N(1,0x1D);
    SPRITE_N(2,0x1E);
    SPRITE_FLIP(1,0,0);
    SPRITE_FLIP(2,0,0);
    //SPRITE_COLOUR(1,COLOUR_WHITE,COLOUR_RED,COLOUR_GRAY);
    //SPRITE_COLOUR(2,COLOUR_WHITE,COLOUR_RED,COLOUR_GRAY);
    SPRITE_COLOUR(1,COLOUR_WHITE,COLOUR_GRAY,COLOUR_BLACK);
    SPRITE_COLOUR(2,COLOUR_WHITE,COLOUR_GRAY,COLOUR_BLACK);

    arkanoid_player_init ();
}

void arkanoid_player_init (void)
{
    freeze = 1;
    inputs = 0;

    ball_x = 7;
    ball_y = 26;
    ball_ox = 0;
    ball_oy = 0;
    ball_dx = 2;
    ball_dy = -2;
    
    // show the bat
    bat_x = ((ball_x-1)<<4)-12;
    bat_y = 29;

    // show the sprites
    PUTSPRITE (0, ((XOFF+0+(ball_x<<1))<<3)+ball_ox-8, ((YOFF+3+(ball_y))<<3)+ball_oy);
    PUTSPRITE(1, ((XOFF+1)<<3)+bat_x,    (YOFF+30)<<3);
    PUTSPRITE(2, ((XOFF+1)<<3)+16+bat_x, (YOFF+30)<<3);
}

int arkanoid_main (void)
{
#ifdef BUILD_MOUSE
    unsigned char mouse_hysteresis = 0;
#endif
    
    while (1)
    {
       arkanoid_printat (XOFF+11,YOFF+23, "ROUND 1");
       arkanoid_printat (XOFF+12,YOFF+25, "READY");
       
#ifndef BUILD_MOUSE
        while (lastKeyPressed != KEY_SPACE)
#else
       while ((lastMouseData[MOUSE_DATA_BTN] & MOUSE_BTN_LT) == 0)
#endif
           ;

       // wipe the text
       put_bg (6, 20); put_bg (7, 20); put_bg (8, 20); put_bg (9, 20);
       put_bg (6, 22); put_bg (7, 22); put_bg (8, 22);

       freeze = 0;

       while (!freeze)
       {
#ifdef MULTI_MCU
          static int exit_hysteresis = 0;
          int i;

          // need to do something in the foreground for a bit
          // otherwise the mouse plays up
          // - the hysteresis fudge won't work properly
          for (i=0; i<200; i++)
              PUTTILE (0x20, i&0x0F, 0);

          // both buttons need to register for 5 loops before we exit
          if ((lastMouseData[MOUSE_DATA_BTN] & (MOUSE_BTN_LT|MOUSE_BTN_RT)) ==
                                            (MOUSE_BTN_LT|MOUSE_BTN_RT))
          {
             if (++exit_hysteresis == 5)
                return (0);
          }
          else
             exit_hysteresis = 0;
#else
          if (key[KEY_ESC])
             return (0);

          show_debug ();
#endif

#ifndef BUILD_MOUSE
          if (key[KEY_LEFT]) inputs = -1;
          else if (key[KEY_RIGHT]) inputs = +1;
          else inputs = 0;
#else
          // add some hysteresis to the (dodgy) mouse input
          if (mouseAck)
             mouse_hysteresis = 8;
          else
          {
             if (mouse_hysteresis == 0)
             {
                inputs = 0;
                continue;
             }
             else
                mouse_hysteresis--;
          }
          mouseAck = 0;

          // handle new mouse data             
          if ((lastMouseData[MOUSE_DATA_BTN] & MOUSE_X_OV) == 0)
          {
             inputs = lastMouseData[MOUSE_DATA_X];
             continue;
          }
          inputs = 0;
#endif
       }

       arkanoid_player_init ();
    }
    
    return (0);
}
#endif

