/*************************************************
 * FILE:	@(#)score.c	1.3 04/06/08
 * DESCRIPTION:	
 * 	Definition for the score module.
 ************************************************/
#include <osek/osek.h>
#include "tetris.h"

/* protect global score data */
DeclareResource(r_score);

/* score data */
struct score
{
    unsigned int  points;
    unsigned int  pieces;
    unsigned int  lines;
    unsigned char phase;
};
static struct score s_board;


void InitScoreBoard(void)
{
    GetResource(r_score);
    s_board.points = 0;
    s_board.pieces = 0;
    s_board.lines = 0;
    s_board.phase = 0;
    ReleaseResource(r_score);
}

void UpdateScoreBoard(ScoreBoardType s,unsigned char update)
{
   GetResource(r_score);
   switch(s)
    {
       case S_PIECE:
           s_board.pieces += update;
           s_board.points += 2*update;
           break;
       case S_LINE:
           s_board.lines  += update;
           s_board.points += 7*update;
           break;
       case S_PHASE:
           s_board.phase  += update;
           break;
       default: break;
    }
     ReleaseResource(r_score);
}


void DisplayScoreBoard(void)
{
    unsigned int po,li,pi;
    unsigned char ph;

    GetResource(r_score);
    po= s_board.points;
    li= s_board.lines;
    pi= s_board.pieces;
    ph= s_board.phase;
    ReleaseResource(r_score);

    /* display score data: target */
    TargetDisplayScoreBoard(po,li,pi,ph);
    
    return;

}



