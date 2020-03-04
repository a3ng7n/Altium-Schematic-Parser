/********************************************************
 * FILE:	@(#)board.c	1.4 04/08/12
 * DESCRIPTION:
 * 	Source code for the 'board' module.
 *******************************************************/
#include "tetris.h"

/* internal board representation */
static color board[H_BOARD+2][W_BOARD+2];

static void CopyLine(LineType from,
		     LineType to);

void BoardInit(color bkg, color fr)
{
    unsigned char i,j;

     /* clear entire screen */
     ClearScreen(bkg);

     /* Draw Tetris outside frame */
     Fill(FIRSTBOARDROW-1, FIRSTBOARDLINE-1,
          FIRSTBOARDROW,   LASTBOARDLINE+2,fr);

     Fill(LASTBOARDROW+1,  FIRSTBOARDLINE-1,
          LASTBOARDROW+2,  LASTBOARDLINE+2,fr);

     Fill(FIRSTBOARDROW,   FIRSTBOARDLINE-1,
          LASTBOARDROW+1,  FIRSTBOARDLINE,fr);

     Fill(FIRSTBOARDROW,   LASTBOARDLINE+1,
          LASTBOARDROW+1, LASTBOARDLINE+2,fr);

     /* Initialize the whole board to free */
     for (i=FIRSTBOARDLINE;i<=LASTBOARDLINE;i++)
     {
          for(j=FIRSTBOARDROW;j<=LASTBOARDROW;j++)
          {
             board[i][j] = bkg;
          }
     }
     /* Initialize the frame to non-free */
     for (i=FIRSTBOARDROW-1;i<=LASTBOARDROW+1;i++)
     {
             board[FIRSTBOARDLINE-1][i] = fr;
     }
      for (i=FIRSTBOARDLINE-1;i<=LASTBOARDLINE+1;i++)
     {
             board[i][FIRSTBOARDROW-1] = fr;
     }
      for (i=FIRSTBOARDLINE-1;i<=LASTBOARDLINE+1;i++)
     {
             board[i][LASTBOARDROW+1] = fr;
     }

     return;
}


void PaintCell(LineType line, ColType row, color color)
{

   if (row>=FIRSTBOARDROW && row <=LASTBOARDROW  &&
       line>=FIRSTBOARDLINE && line <=LASTBOARDLINE )
   {

      board[line][row] = color;
      Fill(row,line,row+1,line+1,color);
   }
   return;
}

uint_least8_t FreeCell(LineType line, ColType  row)
{
    return (board[line][row]==(color)black);
}


void PaintLine(LineType line)
{
   ColType row;
   color color;
   if (line>=FIRSTBOARDLINE && line <=LASTBOARDLINE)
   {
      for (row=FIRSTBOARDROW;row<=LASTBOARDROW;row++)
      {
          color = board[line][row];
          PaintCell(line,row,color);
      }
   }
}


void PaintBetweenLines(LineType from ,LineType to)
{
    LineType l;
    for (l=from;l<=to;l++)
    {
           PaintLine(l);
    }
}

 /* returns 1 if line is full with Tetris pieces */
uint_least8_t FullLine(LineType line)
{
    ColType row;
    for (row=FIRSTBOARDROW;row<=LASTBOARDROW;row++)
    {
         if (FreeCell(line,row))
         {
            return 0;
         }
    }
    return 1;
}

/* Deletes a full Tetris line */
void ShiftDownLines(LineType down,LineType top)
{
    LineType l;
    for (l=down;l<=top;l++)
    {
       CopyLine(l+1,l);
    }

    return;
}


/* Copies a full Tetris line 'from' in the 
 * position of line 'to' */
static void CopyLine(LineType from,LineType to)
{
    ColType row;
    for (row=FIRSTBOARDROW;row<=LASTBOARDROW;row++)
    {
             board[to][row] = board[from][row];
    }

    return;
}
