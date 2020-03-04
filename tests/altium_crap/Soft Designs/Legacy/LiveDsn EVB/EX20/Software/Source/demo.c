#ifdef BUILD_DEMO

#include <string.h>
#include <stdlib.h>

#include "demo.h"
#include "osdepend.h"

#ifndef __GNUC__
#define BUILD_COLLISION
#endif

#ifdef __GNUC__
#define DEMO_TICK     10
#else
#define DEMO_TICK     2
#endif

/* type definitions */

/* global variables */

static volatile unsigned char tick = DEMO_TICK;

static volatile BIT freeze = 1;

/* routines */

#define PRINTAT(x,y,str) pacman_printat(x,y,str)

int pacman_printat(int x, int y, ROM char *str)
{
    unsigned char c;
    for (; *str; str++)
    {
        c = *str;
        if (*str >= '0' && *str <= ';')
            c = *str - '0' + 0x40;
        else
        if (*str >= 'A' && *str <= '[')
            c = *str - 'A' + 0x21;
        
        PUTTILE (c, x++, y);
    }
    
    return (x);
}

int VPRINTAT (int x, int y, ROM char *str)
{
    unsigned char c;
    for (; *str; str++)
    {
        c = *str;
        if (*str >= '0' && *str <= ';')
            c = *str - '0' + 0x40;
        else
        if (*str >= 'A' && *str <= '[')
            c = *str - 'A' + 0x21;
        else
        c &= 0x7F;        
        PUTTILE (c, x, y++);
    }
    
    return (y);
}

#define PRINTBYTE(x,y,str,v) pacman_printbyte(x,y,str,v)

void pacman_printbyte(int x, int y, ROM char *str, unsigned char value)
{
    unsigned char c;    
    int x2 = pacman_printat(x, y, str);

    c = value >> 4;
    PUTTILE ((c>9 ? c+23 : c+0x40), x2++, y);
    c = value & 0x0f;
    PUTTILE ((c>9 ? c+23 : c+0x40), x2++, y);
}

static XDATA int sx[8];
static XDATA int sy[8];
static XDATA int sdx[8];
static XDATA int sdy[8];
static XDATA int xtra[8];
static volatile int S = 0;  // currently selected sprite
static XDATA int N[8];
static XDATA int C[8];
static XDATA int F[8];
static volatile BIT K = 0;
static volatile BIT M = 0;
static volatile BIT A = 0;
static volatile BIT _B = 0;
static volatile BIT _E = 0;

static ROM unsigned char plyr_sprite[][4] =
{
    { 0x84, 0x82, 0x80, 0x82 },      // left
    { 0x04, 0x02, 0x00, 0x02 },      // right
    { 0x84, 0x83, 0x81, 0x83 },      // up
    { 0x04, 0x03, 0x01, 0x03 }       // down
};

static ROM unsigned char ghost_sprite[][4] =
{
    { 0x09, 0x0A, 0x09, 0x0A },      // left
    { 0x05, 0x06, 0x05, 0x06 },      // right
    { 0x0B, 0x0C, 0x0B, 0x0C },      // up
    { 0x07, 0x08, 0x07, 0x08 }       // down
};

#define SPRITE_FIELD_X  2
#define SPRITE_FIELD_Y  14
#define SPRITE_FIELD_W  28
#define SPRITE_FIELD_H  28

#define DBG_SPRITE_N(s,n)   \
    N[s] = (n&0x1f);        \
    SPRITE_N(s,(n&0x1f))
#define DBG_SPRITE_COLOUR(s,c1,c2,c3)         \
    C[s] = ((int)c3 << 8) | (c2 << 4) | c1;   \
    SPRITE_COLOUR(s,c1,c2,c3)
#define DBG_SPRITE_FLIP(s,x,y)                \
    F[s] = ((x)<<1)|(y);                          \
    SPRITE_FLIP(s,x,y)

// sound start/end
static XDATA unsigned char sb = 0x00;
static XDATA unsigned char se = 0x10;

static void show_debug (void)
{
    static BIT t = 0;
    
    int x, y;

    x = SPRITE_FIELD_X + SPRITE_FIELD_W - 23;
    y = SPRITE_FIELD_Y + SPRITE_FIELD_H + 4;
    
    PUTTILE ((t & !(K || M) ? 0x03 : 'S'-'A'+0x21), x+8, y);
    PRINTBYTE (x+ 9, y++,          "PRITE NO ", S);
    PRINTBYTE (x+16, y++,                 "N ", N[S]);
    PRINTBYTE (x+15, y,                  "C1 ", (C[S]>>0)&0x0F);
    PUTTILE ((C[S]>>0)&0x0F, x+21, y++);
    PRINTBYTE (x+15, y,                  "C2 ", (C[S]>>4)&0x0F);
    PUTTILE ((C[S]>>4)&0x0F, x+21, y++);
    PRINTBYTE (x+15, y,                  "C3 ", (C[S]>>8)&0x0F);
    PUTTILE ((C[S]>>8)&0x0F, x+21, y++);
    PRINTAT   (x+13, y,                "FLIP ");
    PRINTAT (x+18, y, (F[S] & (1<<1) ? "X" : " "));
    PRINTAT (x+19, y, (F[S] & (1<<0) ? "Y" : " "));
    y++;
    PRINTBYTE (x+16, y++,                 "X ", sx[S]);
    PRINTBYTE (x+16, y++,                 "Y ", sy[S]);

    x = SPRITE_FIELD_X + SPRITE_FIELD_W + 6;
    y = SPRITE_FIELD_Y + SPRITE_FIELD_H + 4;

    PUTTILE ((t & !M ? 0x03 : 'K'-'A'+0x21), x, y);
    PRINTAT (x+1, y, "EYBOARD CONTROL");
    PRINTAT (x+17, y++, (K ? "ON " : "OFF"));
    PUTTILE ((t & !K ? 0x03 : 'M'-'A'+0x21), x, y);
    PRINTAT (x+1, y, "OUSE CONTROL");
    PRINTAT (x+17, y++, (M ? "ON " : "OFF"));
    PUTTILE ((t & (K || M) ? 0x03 : 'X'-'A'+0x21), x, y);
    PRINTAT (x+1, y++, ";FLIP");
    PUTTILE ((t & (K || M) ? 0x03 : 'Y'-'A'+0x21), x, y);
    PRINTAT (x+1, y++, ";FLIP");
    if (!(K || M)) A = 1;
    PUTTILE ((t & (K || M) ? 0x03 : 'A'-'A'+0x21), x, y);
    PRINTAT (x+1, y, "NIMATE SPRITE");
    PRINTAT (x+17, y++, (A ? "ON " : "OFF"));
    PUTTILE ((t & (K || M) & !A ? 0x03 : 'N'-'A'+0x21), x, y);
    PRINTAT (x+1, y++, "EXT SPRITE");

    PUTTILE ((t ? 0x03 : 'P'-'A'+0x21), 43, SPRITE_FIELD_Y+18);
    if (t)
       PRINTAT (44, SPRITE_FIELD_Y+20, "\x03   \x03   \x03");
    else
       PRINTAT (44, SPRITE_FIELD_Y+20, "1   2   3");
    if (_B) { PUTTILE (0x20, 40 + sb, SPRITE_FIELD_Y+22); sb=(sb+1)%17; _B=0; }
    if (_E) { PUTTILE (0x20, 40 + se, SPRITE_FIELD_Y+24); se=(se+1)%17; _E=0; }
    PUTTILE ((t ? 0x03 : 'B'-'A'+0x21), 40 + sb, SPRITE_FIELD_Y+22);
    PUTTILE ((t ? 0x03 : 'E'-'A'+0x21), 40 + se, SPRITE_FIELD_Y+24);
    t = !t;
}

void demo_tick_handler (void)
{
    static int cell = 0;
    static int i = 0;

    int j;

    if (freeze)
        return;

    if (--tick != 0)
        return;

    tick = DEMO_TICK;

    // sprites have 4 different speeds
    for (j=0; j<i+2; j++)
    {
       // no auto update if under keyboard control
       if ((K || M) && j == S)
       {
       }
       else
       {          
          if ((sdx[j] < 0 && sx[j] < 2) ||
              (sdx[j] > 0 && sx[j] > ((SPRITE_FIELD_W-4)<<3)))
          {
             sdx[j] = -sdx[j];
             if (j != 6)
                xtra[j] = (sdx[j] < 0 ? 0 : 1);
          }
          if ((sdy[j] < 0 && sy[j] < 2) ||
              (sdy[j] > 0 && sy[j] > ((SPRITE_FIELD_H-4)<<3)))
          {
             sdy[j] = -sdy[j];
             if (j != 6)
                xtra[j] = (sdy[j] < 0 ? 2 : 3);
          }
          sx[j] += sdx[j];
          sy[j] += sdy[j];
       }
       
       // animate the sprite
       if (j < 6)
       {
          // a ghost!
          if ((!K && !M) || S != j || A)
          {
             DBG_SPRITE_N (j, ghost_sprite[xtra[j]][cell&0x03]);
          }
       }
       else
       if (j == 6)
       {
          // cycle the palette
          xtra[j] = (xtra[j] + 1) & 511;
          DBG_SPRITE_COLOUR (j, 
                         8 | (xtra[j] & 0x07),
                         8 | ((xtra[j] >> 3) & 0x07),
                         8 | ((xtra[j] >> 6) & 0x07));
       }
       if (j == 7)
       {
          if ((!K && !M) || S != 7 || A)
          {
             DBG_SPRITE_N (j, plyr_sprite[xtra[j]][cell/4]);
          }
          if (!(K || M) || S != 7)
          {
             DBG_SPRITE_FLIP (j, (sdx[j] < 0 ? 1 : 0), (sdy[j] < 0 ? 1 : 0));
          }
       }
       
       PUTSPRITE (j, ((SPRITE_FIELD_X+1)<<3)+sx[j], ((SPRITE_FIELD_Y+1)<<3)+sy[j]);
    }
    i = (i+2) & 7;
    cell = (cell + 1) & 15;

#ifdef BUILD_COLLISION    
    // check for collision
    if ((j = (P1 & 0x07)) == 0x07)
    {
       PRINTAT (SPRITE_FIELD_X+5, SPRITE_FIELD_Y+SPRITE_FIELD_H+1,   "  NO COLLISION[    ");
    }
    else
    {
       PRINTBYTE (SPRITE_FIELD_X+5, SPRITE_FIELD_Y+SPRITE_FIELD_H+1, "COLLISION DETECT ", j);
       *collide = 0;
    }
#endif
    
    show_debug ();
}

#define MY_SCREEN_W   64

void demo_init (void)
{
    int x, y, i;

#ifndef __GNUC__
    // setup some background tiles as pretty coloured squares
    for (i=0; i<16; i++)
       for (x=0; x<8*8/2; x++)
          tile[i*8*8/2+x] = (i<<4)|i;
#endif

    for (x=0; x<MY_SCREEN_W; x++)
       PUTTILE (0x09, x, 1);
    for (y=0; y<3; y++)
       PUTTILE (0x09, 1, y);
    PRINTAT (20, 0, "64 TILES ; 512 PIXELS");
    for (y=0; y<60; y++)
       PUTTILE (0x09, MY_SCREEN_W-2, y);
    for (x=0; x<3; x++)
       PUTTILE (0x09, MY_SCREEN_W-3+x, 58);
    VPRINTAT (MY_SCREEN_W-1, 20, "60 TILES ; 480 PIXELS");
           
    y = 6;    
    PRINTAT (2, y++, "EVALBOARD DEMONSTRATION");
    PRINTAT (2, y++, "ALTIUM LIMITED");
    PRINTAT (2, y++, "ALTERA CYCLONE W:8051 CORE");
    PRINTAT (2, y++, "POWERED BY NEXAR 2004");

    // show the tilemap
    PRINTAT (40, SPRITE_FIELD_Y, "\xCF\xCF\xCF TILE MAP \xCF\xCF\xCF");
    PRINTAT (40, SPRITE_FIELD_Y+1, "\xCF\xCF 128  TILES \xCF\xCF");
    PRINTAT (40, SPRITE_FIELD_Y+2, "\xCF\xCF 8X8 PIXELS \xCF\xCF");
    for (i=0; i<128; i++)
    {
       PUTTILE (i, 40+(i&0x0F), SPRITE_FIELD_Y+4+(i>>4));
    }

    // show some sound stuff
    PRINTAT (39, SPRITE_FIELD_Y+18, "\xCF\xCF\xCF PLAY SOUND \xCF\xCF\xCF");
    for (i=0; i<16; i++)
        PUTTILE (0x4F, 40+i, SPRITE_FIELD_Y+23);

    PUTTILE (0x51, SPRITE_FIELD_X, SPRITE_FIELD_Y);
    for (x=0; x<SPRITE_FIELD_W-2; x++)
    {
       PUTTILE (0x5A, SPRITE_FIELD_X+1+x, SPRITE_FIELD_Y);
       PUTTILE (0x5C, SPRITE_FIELD_X+1+x, SPRITE_FIELD_Y+SPRITE_FIELD_H-1);
    }
    PUTTILE (0x50, SPRITE_FIELD_X+SPRITE_FIELD_W-1, SPRITE_FIELD_Y);
    for (y=0; y<SPRITE_FIELD_H-2; y++)
    {
       PUTTILE (0x53, SPRITE_FIELD_X, SPRITE_FIELD_Y+1+y);
       PUTTILE (0x52, SPRITE_FIELD_X+SPRITE_FIELD_W-1, SPRITE_FIELD_Y+1+y);
    }
    PUTTILE (0x55, SPRITE_FIELD_X, SPRITE_FIELD_Y+SPRITE_FIELD_H-1);
    PUTTILE (0x54, SPRITE_FIELD_X+SPRITE_FIELD_W-1, SPRITE_FIELD_Y+SPRITE_FIELD_H-1);

    PRINTAT (SPRITE_FIELD_X+3, SPRITE_FIELD_Y+4+1,  "  8 ACTIVE SPRITES");
    PRINTAT (SPRITE_FIELD_X+3, SPRITE_FIELD_Y+4+3,  "  EACH 16X16 PIXELS");
    PRINTAT (SPRITE_FIELD_X+3, SPRITE_FIELD_Y+4+5,  "  FROM A BANK OF 32");
    PRINTAT (SPRITE_FIELD_X+3, SPRITE_FIELD_Y+4+9,  "  SPRITES PASS OVER ");
    PRINTAT (SPRITE_FIELD_X+3, SPRITE_FIELD_Y+4+11, "   BACKGROUND TILES");
    PRINTAT (SPRITE_FIELD_X+3, SPRITE_FIELD_Y+4+15, "SPRITE 7 \x90PACMAN\x90 HAS");
    PRINTAT (SPRITE_FIELD_X+3, SPRITE_FIELD_Y+4+17, " COLLISION DETECTION");

    for (i=0; i<6; i++)
    {
       static unsigned char ghost_colour[] = 
       {
          COLOUR_RED, COLOUR_MAGENTA, COLOUR_CYAN, COLOUR_GREEN, COLOUR_BLUE, COLOUR_WHITE
       };
       
        // show the ghost sprites
        DBG_SPRITE_COLOUR (i, BRIGHT(COLOUR_BLUE), BRIGHT(ghost_colour[i]), COLOUR_WHITE);
        DBG_SPRITE_N (i, 5);
    }

    // piece of fruit
    DBG_SPRITE_N (6, 15);
    DBG_SPRITE_COLOUR (6, 8, 9, 10);
        
    // pacman
    DBG_SPRITE_COLOUR (7, 0, BRIGHT(COLOUR_YELLOW), 0);
    DBG_SPRITE_N (7, 2);
    
    for (i=0; i<8; i++)
    {
       DBG_SPRITE_FLIP (i, 0, 0);
       // want an even coordinate
       while ((sx[i] = rand() & 510) > ((SPRITE_FIELD_W-4)<<3));
       while ((sy[i] = rand() & 510) > ((SPRITE_FIELD_H-4)<<3));
       sdx[i] = (rand() & 4) - 2;
       sdy[i] = (rand() & 4) - 2;
              
       PUTSPRITE (i, ((SPRITE_FIELD_X+1)<<3)+sx[i], ((SPRITE_FIELD_Y+1)<<3)+sy[i]);
    }

    // reset the sprite collision latch    
    *collide = 0;
}

int demo_main (void)
{
    BIT processedKey;

    freeze = 0;
    while (1)
    {
       processedKey = 1;
       if (lastKeyPressed == KEY_ESC)
          break;

       if (lastKeyPressed == KEY_S)   // <S> ($1B)
       {
          if (!(K || M))
             S = (S + 1) & 7;
       }
       else if (lastKeyPressed == KEY_K) // <K> ($42)
       {
          if (!M)
             K = !K;
       }
       else if (lastKeyPressed == KEY_M) // <M> ($3A)
       {
          if (!K)
             M = !M;
       }
       else
       if (lastKeyPressed == KEY_LEFT)
       {
          if (K)
          {
             if (sx[S] > 0)
                sx[S] -= 2;
          }
       }
       else
       if (lastKeyPressed == KEY_RIGHT)
       {
          if (K)
          {
             if (sx[S] < ((SPRITE_FIELD_W-4)<<3))
                sx[S] += 2;
          }
       }
       else
       if (lastKeyPressed == KEY_UP)
       {
          if (K)
          {
             if (sy[S] > 0)
                sy[S] -= 1;
          }
       }
       else
       if (lastKeyPressed == KEY_DOWN)
       {
          if (K)
          {
             if (sy[S] < ((SPRITE_FIELD_H-4)<<3))
                sy[S] += 1;
          }
       }
       else
       if (lastKeyPressed == KEY_X) // <X> ($22)
       {
          if (K || M)
          {
             F[S] ^= (1<<1);
             DBG_SPRITE_FLIP (S, (F[S]>>1), (F[S]&0x01));
          }
       }
       else
       if (lastKeyPressed == KEY_Y) // <Y> ($35)
       {
          if (K || M)
          {
             F[S] ^= (1<<0);
             DBG_SPRITE_FLIP (S, (F[S]>>1), (F[S]&0x01));
          }
       }
       else
       if (lastKeyPressed == KEY_A) // <A> ($1C)
       {
          if (K || M)
          {
             A = !A;
          }
       }
       else
       if (lastKeyPressed == KEY_N) // <N> ($31)
       {
          if ((K || M) & !A)
          {
             // note can't use the expression N[S]+1 in the macro
             int i = (N[S]+1) & 0x1f;
             DBG_SPRITE_N (S, i);
          }
       }
       else
       if (lastKeyPressed == KEY_B) // ($32)
          _B = 1;
       else
       if (lastKeyPressed == KEY_E) // ($24)
          _E = 1;
       else
       if (lastKeyPressed == KEY_1) // ($16)
       {
          SOUND_DIR_UP;
          PLAY_SOUND (0x0000, 0x04A9);
       }
       else
       if (lastKeyPressed == KEY_2) // ($1E)
       {
          SOUND_DIR_UP;
          PLAY_SOUND (0x04AA, 0x0DA0);
       }
       else
       if (lastKeyPressed == KEY_3) // ($26)
       {
          SOUND_DIR_UP;
          PLAY_SOUND (0x0DA1, 0x0FFF);
       }
       else
       if (lastKeyPressed == KEY_P) // ($4D)
       {
          SOUND_DIR (sb <= se ? 1 : 0);
          PLAY_SOUND (((int)sb)<<8, (((int)se)<<8)-1);
       }
       else processedKey = 0;
       if (processedKey)
          lastKeyPressed = 0;
#ifdef BUILD_MOUSE
       if (M & mouseAck)
       {
          // handle new mouse data
          if ((lastMouseData[MOUSE_DATA_BTN] & MOUSE_X_OV) == 0)
          {
             sx[S] += lastMouseData[MOUSE_DATA_X];
             if (sx[S] < 0)
                sx[S] = 0;
               else
               if (sx[S] > ((SPRITE_FIELD_W-4)<<3))
                sx[S] = (SPRITE_FIELD_W-4)<<3;
          }
          if ((lastMouseData[MOUSE_DATA_BTN] & MOUSE_Y_OV) == 0)
          {
             // ycoords upside down compared to screen coords!
             sy[S] -= lastMouseData[MOUSE_DATA_Y];
             if (sy[S] < 0)
                sy[S] = 0;
               else
               if (sy[S] > ((SPRITE_FIELD_W-4)<<3))
                sy[S] = (SPRITE_FIELD_W-4)<<3;
          }
          mouseAck = 0;
       }
#endif
       if (freeze)
       {
          return (1);
       }
    }
    
    return (0);
}
#endif

