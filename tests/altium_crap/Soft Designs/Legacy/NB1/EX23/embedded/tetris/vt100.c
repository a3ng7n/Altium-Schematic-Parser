/************************************************
**  FILE:   @(#)vt100.c	1.9 04/09/21
**  DESCRIPTION:                                *
**      Screen routines: VT100 terminal.        *
**	Tetris is displayed in a vt100 terminal *
**	This module is a wrapper on top of the  *
**	write commands				*
************************************************/
#include <osek/osek.h>
#if defined(DEMO)
#include "demo.h"
#else
#include "tetris.h"
#endif

#include <stdio.h>

#if defined(TETRIS) 

#if ( (TETRISOUT == SERIAL0) || (TETRISOUT == SERIAL1) || (TETRISOUT == STDIO) )
 
/* VT100 help defines */
#define VT100_SETCOMMAND        "\x1B["

#define VT100_POWER_ON           "\x1B\x63"
#define VT100_WHITE_ON_BLACK     "\x1B[?5l"
#define VT100_BLACK_ON_WHITE     "\x1B[?5h"  
#define VT100_NEWLINE            "\x1B[20h"
#define VT100_LINEFEED           "\x1B[20l"
#define VT100_SMOOTHSCROLL       "\x1B[?4h"
#define VT100_JUMPSCROLL         "\x1B[?4l"
#define VT100_AUTOWRAPON         "\x1B[?7h"  
#define VT100_AUTOWRAPOFF        "\x1B[?7l"  

#define VT100_ERASETOENDLINE     "\x1B[0K"
#define VT100_ERASETOSTARTLINE   "\x1B[1K"
#define VT100_ERASELINE          "\x1B[2K"
#define VT100_ERASETOENDSCREEN   "\x1B[0J"
#define VT100_ERASETOSTARTSCREEN "\x1B[1J"
#define VT100_ERASEENTIRESCREEN  "\x1B[2J"

#define VT100_CHAROFF             "\x1B[0m"
#define VT100_CHARBOLD            "\x1B[1m"
#define VT100_CHARUNDERSCORE      "\x1B[4m"
#define VT100_CHARBLINK           "\x1B[5m"

#define VT100_CURSORONEUP         "\x1B[1A" 
#define VT100_CURSORONEDOWN       "\x1B[1B" 
#define VT100_CURSORONERIGHT      "\x1B[1C" 
#define VT100_CURSORONELEFT       "\x1B[1D" 

#define VT100_SCROLLON            "\x1B[?6h"
#define VT100_SCROLLOFF           "\x1B[?6l"

/* x offset in cell units */
#define X_OFF             10
/* y offset in cell units */
#define Y_OFF             5

/* board is (H_BOARD+2)(W_BOARD+2) */
#define B_X0              X_OFF
#define B_X1              B_X0 + LASTBOARDROW+1
#define B_Y0              Y_OFF
#define B_Y1              B_Y0 + LASTBOARDLINE+1

/* statics */
static int first;
static void Screen_Init(void);
static void VT100_SetCursorPos(ColType, LineType);
static void VT100_SetCursorPosPutChar(ColType, LineType, char);
static void VT100_SetCursorPosPutInt(ColType, LineType, int);
static void VT100_SetScroll(LineType, LineType);

#define S_OFF_Y              4
#define S_POS_Y              B_Y1 + S_OFF_Y
#define S_OFF_X              2
#define S_POS_X              B_X0 + S_OFF_X
#define S_N_OFF_X            10
#define S_N_POS_X            10 + S_POS_X

void TargetDisplayScoreBoard(unsigned int po, unsigned int li,
			     unsigned int pi, unsigned char ph)
{
	VT100_SetCursorPosPutInt(S_N_POS_X, S_POS_Y, (int)po);
	VT100_SetCursorPosPutInt(S_N_POS_X, S_POS_Y+1, (int)li);
	VT100_SetCursorPosPutInt(S_N_POS_X, S_POS_Y+2, (int)pi);
	VT100_SetCursorPosPutInt(S_N_POS_X, S_POS_Y+3, (int)ph);
	outflush(TETRISOUT);
}

/* done in first Fill */
void ScreenInit(void) {}

void ClearScreen(color color)
{
	(void)color;
	/* Clear entire screen */
	outs(TETRISOUT,VT100_ERASEENTIRESCREEN);
	outs(TETRISOUT, VT100_CHAROFF);
	first = 1;
	outflush(TETRISOUT);
}

/* Fill square defined by two cells C0 and C1
 * with coordinates (xo,yo) - (x1,y1) with
 * given color */
void Fill(ColType xo, LineType yo,
	  ColType x1, LineType y1, color color)
{
    ColType  xmin = (xo>x1)?x1:xo;
    ColType  xmax = (xo>x1)?xo:x1;
    LineType ymin = (yo>y1)?y1:yo;
    LineType ymax = (yo>y1)?yo:y1;

    /* init screen in first 'fill' */
    if (first)
    {
        Screen_Init();
        first = 0;
    }

    for (yo=B_Y1-ymax;yo<B_Y1-ymin;yo++)
    {
        for (xo=xmin;xo<xmax;xo++)
        {
            if (color==black)
            {
                VT100_SetCursorPosPutChar((ColType)B_X0+2*xo, (LineType)yo, ' ');
            }
            else
            {
                VT100_SetCursorPosPutChar((ColType)B_X0+2*xo, (LineType)yo, '*');
            }
        }
    }
    
    return;
}

/* Displays end-of-game pop-up */
void EndGame(void)
{
        /* Set Characters Blink */
        outs(TETRISOUT,VT100_CHARBLINK);
        VT100_SetCursorPos(B_X0,B_Y1+1);
        outs(TETRISOUT, "!END GAME!   Press ENTER  !END GAME!");
	outflush(TETRISOUT);
        return;
}



/* called with the first 'Fill' */
static void Screen_Init(void)
{
	/* initializes terminal to 'power-on' settings */
        //outs(TETRISOUT,VT100_POWER_ON );
        /* Clear entire screen */
        outs(TETRISOUT,VT100_ERASEENTIRESCREEN);
        /* VT100 background color is black */
        //outs(TETRISOUT,VT100_WHITE_ON_BLACK);
        /* Set Characters Bold */
        //outs(TETRISOUT,VT100_CHARBOLD);
        /* Smooth/ON Scroll */
        //outs(TETRISOUT,VT100_SCROLLON);
        //outs(TETRISOUT,VT100_SMOOTHSCROLL);
        /* Set Scroll Area */
        //VT100_SetScroll(1,B_Y1+1);

        /* Init Score Board */
        VT100_SetCursorPos(S_POS_X,S_POS_Y);
        outs(TETRISOUT,"POINTS :");
        VT100_SetCursorPos(S_POS_X,S_POS_Y+1);
        outs(TETRISOUT,"LINES  :");
        VT100_SetCursorPos(S_POS_X,S_POS_Y+2);
        outs(TETRISOUT,"PIECES :");
        VT100_SetCursorPos(S_POS_X,S_POS_Y+3);
        outs(TETRISOUT,"PHASE  :");

         /* Help */
        VT100_SetCursorPos(S_N_POS_X +S_N_OFF_X,S_POS_Y);
        outs(TETRISOUT,"k/l   : left/right");
        VT100_SetCursorPos(S_N_POS_X +S_N_OFF_X,S_POS_Y+1);
        outs(TETRISOUT,"space : rotate");
        VT100_SetCursorPos(S_N_POS_X +S_N_OFF_X,S_POS_Y+2);
        outs(TETRISOUT,"o     : down");
        VT100_SetCursorPos(S_N_POS_X +S_N_OFF_X,S_POS_Y+3);
        outs(TETRISOUT,"esc   : pause");
        VT100_SetCursorPos(S_N_POS_X +S_N_OFF_X,S_POS_Y+4);
        outs(TETRISOUT,"tab   : continue");
        VT100_SetCursorPos(S_N_POS_X +S_N_OFF_X,S_POS_Y+5);
        outs(TETRISOUT,"enter : start");

        /* Altium Tetris */
        VT100_SetCursorPos(B_X0,B_Y0-3);
	
#if defined(__C51__)
        outs(TETRISOUT,"  ALTIUM TETRIS on C51 RTOS");
#elif defined(__CMB__)
        outs(TETRISOUT,"  ALTIUM TETRIS on Micro Blaze RTOS");
#elif defined(__CZ80__)
        outs(TETRISOUT,"  ALTIUM TETRIS on Z80 RTOS");
#elif defined(__PPC__)
        outs(TETRISOUT,"  ALTIUM TETRIS on Power PC RTOS");
#elif defined(__CTC__)
        outs(TETRISOUT,"  ALTIUM TETRIS on Tricore RTOS");
#elif defined(__CM16C__)
        outs(TETRISOUT,"  ALTIUM TETRIS on M16C RTOS");
#else
        outs(TETRISOUT,"  ALTIUM TETRIS on RTOS");
#endif
        return;
}

static void VT100_SetCursorPos(ColType c, LineType l)
{
    	char b[20];
    
	sprintf(b, "%s%d;%dH", VT100_SETCOMMAND, (int)l, (int)c);
	outs(TETRISOUT, b);
	
	return;    
}

static void VT100_SetCursorPosPutInt(ColType c, LineType l, int i)
{
	char b[24];

	sprintf(b, "%s%d;%dH%d", VT100_SETCOMMAND, (int)l, (int)c, i);
	outs(TETRISOUT, b);

	return;    
}

static void VT100_SetCursorPosPutChar(ColType c, LineType l, char ch)
{
	char b[20];

	sprintf(b, "%s%d;%dH%c", VT100_SETCOMMAND, (int)l, (int)c, ch);
	outs(TETRISOUT, b);

	return;    
}

static void VT100_SetScroll(LineType start, LineType end)
{
	char b[20];
    	
	sprintf(b, "%s%d;%dr%s", VT100_SETCOMMAND, (int)start, (int)end,VT100_SCROLLON);
	outs(TETRISOUT, b);
	return;  
}

#endif
#endif


