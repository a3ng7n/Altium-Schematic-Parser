/******************************************************************************
 * FILE:	@(#)tetris.h	1.17 04/10/11
 * DESCRIPTION:
 * 	Tetris configuration and interface.
 *
 *   1 The following macros
 *   	TETRISIN:  input for shell
 *   	TETRISOUT: output for shell
 *   need to be defined as:
 *      SERIAL0, SERIAL1, STDIO, LCD, KEYPAD
 *      (LCD, KEYPAD for Nexar applications)
 *   (or given as compiler options)
 *   
 *   2 The values given for previous macros depends on:
 *     target specific issues ( #if __target__ )
 * 
 *****************************************************************************/
#ifndef _H_TETRIS_H
#define _H_TETRIS_H

#include <osek/osek.h>
#include "output.h"

#if !defined(TETRISIN)
# if (  defined(__PPC__) || defined(__CMB__) || defined(__CTC__) || defined(__CZ80__) )
#  define TETRISIN		SIMIN
# elif defined(__C51__)
#  define TETRISIN		KEYPAD
# else
#  define TETRISIN		SERIAL0
# endif
#endif

#if !defined(TETRISOUT)
# if (  defined(__PPC__) || defined(__CMB__) || defined(__CTC__) || defined(__CZ80__) )
#  define TETRISOUT		STDIO
# elif defined(__C51__)
#  define TETRISOUT		NONE
# else
#  define TETRISOUT		SERIAL0
# endif
#endif

#if ( TETRISIN == SIMIN )
#define SIMINRX
#endif

#if ( TETRISIN == KEYPAD )
#define NEXAR
#endif

#if ( TETRISOUT == STDIO )
#define STDIOTX
#endif

#if ( (TETRISIN  == SERIAL0) || (TETRISIN  == SERIAL1)  || \
      (TETRISOUT == SERIAL0) || (TETRISOUT == SERIAL1) )
#define UART
#endif


/* application specific errors */
#define E_ERR_HOOK_IT		64
#define E_ERR_HOOK_IM		65
#define E_ERR_HOOK_IS		66
#define E_ERR_IO_OFLW		67
#define E_APP_ERR_MON       	68
#define E_APP_ERR_STAT      	69

/* gravity feeling */
/* first expiration interval in system ticks (500mscs) */
#define  GRAVITY_FIRST          50
/* decrement of expiration interval in every phase (50 mscs)  */ 
#define  GRAVITY_UNIT           4

/* number of completed lines to enter new phase */
#define NO_LINES_PHASE       	6

/* width and height of the board -in cell units-
 * (H_BOARD+2)*(W_BOARD+2) board ( 2 is for frame) */
#define W_BOARD           	14
#define H_BOARD           	21
#define W_TOTAL           	W_BOARD +2
#define H_TOTAL           	H_BOARD +2

#define FIRSTBOARDLINE    	1
#define LASTBOARDLINE     	H_BOARD
#define FIRSTBOARDROW     	1
#define LASTBOARDROW      	W_BOARD

/* ASCII codes */
#define SPACE              32
#define KEY_O              111
#define KEY_K              107
#define KEY_L              108
#define ESCAPE             27
#define ENTER              13
#define TAB                9

#if (TETRISIN==KEYPAD)

#define M_ROTATE           6 
#define M_DOWN             10
#define M_LEFT             5
#define M_RIGHT            7
#define M_PAUSE            2
#define M_CONTINUE         4
#define M_INIT             16

#else /* (TETRISIN==KEYPAD) */

#ifndef M_ROTATE 
#define M_ROTATE           SPACE
#endif
#ifndef M_DOWN
#define M_DOWN             KEY_O
#endif
#ifndef M_LEFT
#define M_LEFT             KEY_K
#endif
#ifndef M_RIGHT
#define M_RIGHT            KEY_L
#endif
#ifndef M_PAUSE
#define M_PAUSE            ESCAPE
#endif
#ifndef M_CONTINUE
#define M_CONTINUE         TAB
#endif
#ifndef M_INIT
#define M_INIT             ENTER
#endif

#endif /* (TETRISIN==KEYPAD) */

/* enumeration of possible colors - if apply - */ 
typedef enum {
        black = 0,
        darkred,
        darkgreen,
        darkyellow,
        darkblue,
        darkmagenta,
        darkcyan,
        darkgrey,
        grey,
        red,
        green,
        yellow,
        blue,
        magenta,
        cyan,
        white
}color ;

/* Col/Line types */
typedef uint_least8_t ColType;
typedef uint_least8_t LineType;
typedef int_least8_t ExtColType;
typedef int_least8_t ExtLineType;

/* enumeration of score-board types */
typedef enum
{
    S_POINT  = 0,
    S_LINE    = 1,
    S_PIECE   = 2,
    S_PHASE   = 3
} ScoreBoardType;

/* no seconds */
extern unsigned int no_seconds;

/* number of pieces (used in the SIMINTX mode) */
extern unsigned int no_pieces;

/*************************************************************************
 * this needs to be defined whether for
 * 	a vt100 terminal (vt100.c) or 
 * 	a vga driver (vga.c).
 * These are the interfaces.
 * 	Default: vt100
 ************************************************************************/

/* Displays the status of the score:
 * (points, pieces, lines and phase) */
void TargetDisplayScoreBoard(unsigned int, unsigned int,
			     unsigned int, unsigned char);
/* init the screen hardware: vga, vt100, etc.. */
void ScreenInit(void);
/* flush output stream */
void FlushOutput(void);
/* Fill square defined by two cells C0 and C1
 * with coordinates (xo,yo) - (x1,y1) with
 * given color */
void Fill(ColType xo, LineType yo,
	  ColType x1, LineType y1, color color);
/* Displays end-of-game pop-up */
void EndGame(void);
/* clear screen with given color */
void ClearScreen(color);

/************************************************************************ 
 * score.c 
 ***********************************************************************/

/* Initiales the score-data at the beginning
 * of every new match  */
void InitScoreBoard(void);

/* Updates scoreboard data when:
 * - A new piece hits bottom 
 * - A new line is built
 * - A new phase is entered */
void UpdateScoreBoard(ScoreBoardType,unsigned char);

/* Call to display the current scoreboard data */
void DisplayScoreBoard(void);


/*********************************************************************** 
 * board.c 
 **********************************************************************/

/* Starts the board with background and frame colors */
void BoardInit(color, color);
/* returns 1 if cell is free -no tetris-, 0 otherwise */
uint_least8_t FreeCell(LineType, ColType);
/* paints given cell with given color */
void PaintCell(LineType,ColType,color);
/* paints a line */
void PaintLine(LineType);
/* paints board within given lines */
void PaintBetweenLines(LineType,LineType);
/* returns 1 if line is full with Tetris pieces */
uint_least8_t FullLine(LineType);
/* Updates a full line */
void ShiftDownLines(LineType,LineType);

#endif /* _H_TETRIS_H */


