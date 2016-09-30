/******************************************************************************
 * FILE:	@(#)tetris.c	1.11 04/09/21
 * DESCRIPTION:	
 * 	Source tetris module	
 *****************************************************************************/
#include <osek/osek.h>
#include "tetris.h"

/* all possible tetris piece types: indexes in 
 * the 'rotate_map' */
typedef enum
{
    LeftSnake_A,
    LeftSnake_B,
    LeftSnake_C,
    LeftSnake_D,
    RightSnake_A,
    RightSnake_B,
    RightSnake_C,
    RightSnake_D,
    T_A,
    T_B,
    T_C,
    T_D,
    LeftGun_A,
    LeftGun_B,
    RightGun_A,
    RightGun_B,
    I_A,
    I_B,
    Square
} TetrisType;

#define NO_PIECES 19

/* A rotation map : j -> rotate_map(j).
 * Transformation after a 'rotate' action */
TetrisType   rotate_map[NO_PIECES] =
{
    LeftSnake_B,
    LeftSnake_C,
    LeftSnake_D,
    LeftSnake_A,
    RightSnake_B,
    RightSnake_C,
    RightSnake_D,
    RightSnake_A,
    T_B,
    T_C,
    T_D,
    T_A,
    LeftGun_B,
    LeftGun_A,
    RightGun_B,
    RightGun_A,
    I_B,
    I_A,
    Square
};

#define SHAPE3x3     9

/* 'shape_map' for all pieces: indexed by TetrisType.
 * All pices MUST fit in a 3x3 square. */
unsigned char   shape_map[NO_PIECES][SHAPE3x3] =
{
      {
        /* Left Snake A */
        0, 0, 1,
        0, 1, 1,
        0, 1, 1,
      },
      {
        /* Left Snake B */
        0, 0, 0,
        1, 1, 0,
        1, 1, 1,
      },
      {
        /* Left Snake C */
        1, 0, 1,
        1, 0, 1,
        0, 0, 1,
      },
      {
        /* Left Snake D */
        0, 1, 1,
        0, 0, 0,
        1, 1, 1,
      },
      {
        /* Right Snake A */
        0, 1, 1,
        0, 1, 1,
        0, 0, 1,
      },
      {
        /* Right Snake B */
        0, 0, 0,
        0, 1, 1,
        1, 1, 1,
      },
      {
        /* Right Snake C */
        0, 0, 1,
        1, 0, 1,
        1, 0, 1,
      },
      {
        /* Right Snake D */
        1, 1, 0,
        0, 0, 0,
        1, 1, 1,
      },
      {
        /* T A */
        0, 1, 1,
        0, 0, 1,
        0, 1, 1,
      },
      {
        /* T B */
        0, 0, 0,
        1, 0, 1,
        1, 1, 1,
      },
      {
        /* T C */
        1, 0, 1,
        0, 0, 1,
        1, 0, 1,
      },
      {
        /* T D */
        1, 0, 1,
        0, 0, 0,
        1, 1, 1,
      },
      {
        /* Left Gun A */
        1, 0, 1,
        0, 0, 1,
        0, 1, 1,
      },
      {
        /* Left Gun B*/
        0, 0, 1,
        1, 0, 0,
        1, 1, 1,
      },
      {
        /* Right Gun A*/
        0, 1, 1,
        0, 0, 1,
        1, 0, 1,
      },
      {
        /* Right Gun B*/
        1, 0, 0,
        0, 0, 1,
        1, 1, 1,
      },
      {
        /*  I A */
        1, 0, 1,
        1, 0, 1,
        1, 0, 1,
      },
      {
        /* I B*/
        1, 1, 1,
        0, 0, 0,
        1, 1, 1,
      },
      {
        /* Square */
        0, 0, 1,
        0, 0, 1,
        1, 1, 1,
      }
};

/* 'color_map' for all pieces: indexed by TetrisType */
color   color_map[NO_PIECES] =
{
    white,
    white,
    white,
    white,
    red,
    red,
    red,
    red,
    blue,
    blue,
    blue,
    blue,
    green,
    green,
    yellow,
    yellow,
    cyan,
    cyan,
    magenta
};

/* external declarations */
DeclareTask(tetris);
DeclareTask(timeslot);

DeclareEvent(e_rotate);
DeclareEvent(e_gravity);
DeclareEvent(e_down);
DeclareEvent(e_right);
DeclareEvent(e_left);
DeclareEvent(e_kill);
DeclareEvent(e_pause);
DeclareEvent(e_resume);

DeclareCounter(c_speed);

/* paint me (tetristype) at given position with given color */ 
static void PaintPiece(ExtLineType,ExtColType,TetrisType,color);

/* being me (tetristype) at a given position ,
 * can a move after given event? */ 
static unsigned char IsMovePossible(ExtLineType ,ExtColType,
                     TetrisType,EventMaskType);

/* random generator of a tetris piece */
static TetrisType RandomType(void);

/* high stores the highest line occupied by a tetris */ 
static ExtLineType high = FIRSTBOARDLINE;

/****************************************************************************
   Tetris Tetris Tetris Tetris Tetris Tetris Tetris Tetris
   This task goberns the lifetime of one single tetris piece since its
   birth at top of tetris panel. It first paints the tetris piece and
   then waits for one of the following events:
   (from stroke)
   - e_kill:            end the task.
   - e_left:            move tetris to left.
   - e_right:           move tetris to right.
   - e_down:            move tetris all the way down.
   - e_rotate:          rotate tetris.
   (from timeslot)
   - e_gravity:		gravity hit.
  ***************************************************************************/
TASK (tetris)
{
     /* coordinates of center of figure */
     ExtLineType line = LASTBOARDLINE-1;
     ExtLineType l;
     ExtColType row  = LASTBOARDROW/2;
     /* get random figure */
     TetrisType type = RandomType();
     uint_least8_t terminate = 0;
     int  col = color_map[type];

     EventMaskType event;
     
     /* paint new one or again the old one */
     PaintPiece(line,row,type,col);


     while (1)
     {

           WaitEvent(e_rotate | e_down | e_right | e_left | e_kill | e_gravity );
           GetEvent(tetris,&event);
           ClearEvent(event);
	  
	   /* task must terminate */
	   if (event & e_kill)
	   {
		/* we abandon */   
		TerminateTask();
	   }
           
	   /* gravity hit */
	   if (event & e_gravity)
           {
               if (IsMovePossible(line,row,type,e_down))
                {
                   line -= 1;
                }
                else
                {
                  terminate = 1;
                  high = (high<line+1)?line+1:high;
                }
           }
	   /* move tetris to right */
	   if (event & e_right)
           {
                if (IsMovePossible(line,row,type,e_right))
                {
                   row += 1;
                }
           }
	   
	   /* move tetris to left */
           if (event & e_left)
           {
                if (IsMovePossible(line,row,type,e_left))
                {
                   row -= 1;
                }
           }
	   
	   /* rotate tetris */
           if (event & e_rotate)
           {
                if (IsMovePossible(line,row,type,e_rotate))
                {
                   type = rotate_map[type];
                }
           }
           
	   /* all the way down */
	   if (event & e_down)
           {
	   	/* suspend gravity feeling */
	   	SetEvent(timeslot,e_pause);
		/* down until we hit uppre part of other Tetris */
                while (IsMovePossible(line,row,type,e_down))
                {
                   line -= 1;
                }
                terminate = 1;
                high = (high<line+1)?line+1:high;
	   	/* resume gravity feeling */
	   	SetEvent(timeslot,e_resume);
           }
           
           if (high==LASTBOARDLINE)
           {
             	/* game is over: highest Tetris touches upper 
	     	 * part of the board */
             	high =  FIRSTBOARDLINE;
	     	/* tell user what to do for a new game*/
             	EndGame();
	   	/* suspend gravity feeling */
	   	SetEvent(timeslot,e_pause);
	        
	     	/* end of this task */
             	TerminateTask();
           }
           else
           {
               /* paint new one or again the old one */
               PaintPiece(line,row,type,col);
	       
               /* a tetris piece has gone all the way down */
               if (terminate)
               {
		  /* end of tetris piece life */
                  UpdateScoreBoard(S_PIECE,1);
		  
		  /* check possible completed lines */ 
                  for (l=line+1;l>=FIRSTBOARDLINE && l>=line-1;l--)
                   {
                       if(FullLine(l))
                       {
			     /* line is completed */  
                             UpdateScoreBoard(S_LINE,1);
			     /* increase difficulty */
                             IncrementCounter(c_speed);
			     /* erase completed line, update board */
                             ShiftDownLines(l,high);
			     /* paint ir */
                             PaintBetweenLines(l,high);
			     /* high is one less */
                             high--;
			     /* increase difficulty */
                             IncrementCounter(c_speed);
                       }
                   }
		 
		   /* new score */
                   DisplayScoreBoard();

	   	   /* resume gravity feeling */
	   	   SetEvent(timeslot,e_resume);
		   TerminateTask();
               }
           }
	   
     } // while 1

     /* compiler warning */
    return;
}



/*******************************************************
* static routines movements  static routines movements *
*******************************************************/
static void PaintPiece(ExtLineType yo,ExtColType xo,TetrisType type,color color )
{
  	 ExtLineType y;
   	ExtColType  x;

   	  unsigned char* ptr = (  unsigned char*)&shape_map[type];

    for (y = yo+1; y>= yo-1;y--)
    {
    	for (x=xo-1; x<=xo+1;x++)
    	{
        	if (*ptr++ == 0)
        	{
        	    if ( y>= FIRSTBOARDLINE && y<= LASTBOARDLINE &&
        	         x>=FIRSTBOARDROW && x<=LASTBOARDROW )
        	         {
        	           PaintCell(y,x,color);
        	         }
        	}
    	}
    }

    outflush(TETRISOUT);
  
}

static unsigned char IsMovePossible(ExtLineType yo, ExtColType xo,
            TetrisType type,EventMaskType event)
{
  ExtColType xc=xo; ExtLineType yc=yo;
  ExtColType x ;ExtLineType y;
  
    unsigned char* ptr;

  /* erase: paint it black */
  PaintPiece(yo,xo,type,black);

  if (event == e_rotate)
  {
	  /* position does not change, yes type */
         type = rotate_map[type];
  }
  else if (event == e_down)
  {
	  /* one down */
          yc = yo - 1;
  }
  else if (event==e_right)
  {
	  /* one right */
          xc = xo + 1;
  }
  else if (event==e_left)
  {
	  /* one left */
          xc = xo - 1;
  }

  /* do I fit in my new position ? */
  ptr = (  unsigned char*)&shape_map[type];

  for (y = yc+1; y>=yc-1 ;y--)
  {
    for (x= xc-1; x<=xc+1;x++)
    {
        if (*ptr++ == 0 && y>=FIRSTBOARDLINE-1 && x>=FIRSTBOARDROW-1 &&
            x<=LASTBOARDROW+1 && !FreeCell(y,x) )
        {   
            return 0;
        }
      }
    }

  return 1;
}

#if defined(SIMINRX)
unsigned int no_pieces;

/*
	  case 0: tet = LeftSnake_A;
          case 1: tet = RightSnake_A;
          case 2: tet = T_A;
          case 3: tet = I_A;
          case 4: tet = LeftGun_A;
          case 5: tet = RightGun_A;
          case 6: tet = Square;
*/

TetrisType pieces[] = {
	/* 1 */ LeftSnake_A,
	/* 2 */ RightSnake_A,
	/* 3 */ T_A,
	/* 4 */ I_A,
	/* 5 */ LeftGun_A,
	/* 6 */ RightGun_A,
	/* 7 */ Square,
	/* 8 */ LeftSnake_A,
	/* 9 */ RightSnake_A,
	/* 10 */ T_A,
	/* 11 */ I_A,
	/* 12 */ LeftGun_A,
	/* 13 */ RightGun_A,
	/* 14 */ Square,
	/* 15 */ LeftSnake_A,
	/* 16 */ RightSnake_A,
	/* 17 */ T_A,
	/* 18 */ I_A,
	/* 19 */ LeftGun_A,
	/* 20 */ RightGun_A,
	/* 21 */ Square,
	/* 22 */ LeftSnake_A,
	/* 23 */ RightSnake_A,
	/* 24 */ T_A,
	/* 25 */ I_A,
	/* 26 */ LeftGun_A,
	/* 27 */ RightGun_A,
	/* 28 */ Square,
	/* 29 */ LeftSnake_A,
	/* 30 */ RightSnake_A,
	/* 31 */ T_A,
	/* 32 */ I_A,

	/* 33 */ LeftGun_A,
	/* 34 */ RightGun_A,
	/* 35 */ Square,
	/* 36 */ LeftSnake_A,
	/* 37 */ RightSnake_A,
	/* 38 */ T_A,
	/* 39 */ I_A,
	/* 40 */ LeftGun_A,
	/* 41 */ RightGun_A,
	/* 42 */ Square,
	/* 43 */ LeftSnake_A,
	/* 44 */ RightSnake_A,
	/* 45 */ T_A,
	/* 46 */ I_A,
	/* 47 */ LeftGun_A,
	/* 48 */ RightGun_A,
	/* 49 */ Square,
	/* 50 */ LeftSnake_A,
	/* 51 */ RightSnake_A,
	/* 52 */ T_A,
	/* 53 */ I_A,
	/* 54 */ LeftGun_A,
	/* 55 */ RightGun_A,
	/* 56 */ Square,
	/* 57 */ LeftSnake_A,
	/* 58 */ RightSnake_A,
	/* 59 */ T_A,
	/* 60 */ I_A,
	/* 61 */ LeftGun_A,
	/* 62 */ RightGun_A,
	/* 63 */ Square,
	/* 64 */ LeftSnake_A,
	/* 65 */ RightSnake_A,
	/* 66 */ T_A,
	/* 67 */ I_A,
	/* 68 */ LeftGun_A,
	/* 69 */ RightGun_A,
	/* 70 */ Square,
	/* 71 */ LeftSnake_A,
	/* 72 */ RightSnake_A,
	/* 73 */ T_A,
	/* 74 */ I_A,
	/* 75 */ LeftGun_A,
	/* 76 */ RightGun_A,
	/* 77 */ Square,
	/* 78 */ LeftSnake_A,
	/* 79 */ RightSnake_A,
	/* 80 */ T_A,
	/* 81 */ I_A,
	/* 82 */ LeftGun_A,
	/* 83 */ RightGun_A,
	/* 84 */ Square,
	/* 85 */ LeftSnake_A,
	/* 86 */ RightSnake_A,
	/* 87 */ T_A,
	/* 88 */ I_A,
	/* 89 */ LeftGun_A,
	/* 90 */ RightGun_A,
	/* 91 */ Square,
	/* 92 */ LeftSnake_A,
	/* 93 */ RightSnake_A,
	/* 94 */ T_A,
	/* 95 */ I_A,
	/* 96 */ LeftGun_A,
	/* 97 */ RightGun_A,
	/* 98 */ Square,
	/* 99 */ LeftSnake_A,
	/* 100 */ RightSnake_A,
	/* 101 */ T_A,
	/* 102 */ I_A,
	/* 103 */ LeftGun_A,
	/* 104 */ RightGun_A,
	/* 105 */ Square,
	/* 106 */ LeftSnake_A,
	/* 107 */ RightSnake_A,
	/* 108 */ T_A,
	/* 109 */ I_A,
	/* 110 */ LeftGun_A,
	/* 111 */ RightGun_A,
	/* 112 */ Square,
	/* 113 */ LeftSnake_A,
	/* 114 */ RightSnake_A,
	/* 115 */ T_A,
	/* 116 */ I_A,
	/* 117 */ LeftGun_A,
	/* 118 */ RightGun_A,
	/* 119 */ Square,
	/* 120 */ LeftSnake_A,
	/* 121 */ RightSnake_A,
	/* 122 */ T_A,
	/* 123 */ I_A,
	/* 124 */ LeftGun_A,
	/* 125 */ RightGun_A,
	/* 126 */ Square,
	/* 127 */ LeftSnake_A,
	/* 128 */ RightSnake_A,
	/* 129 */ T_A,
	/* 130 */ I_A,
	/* 131 */ LeftGun_A,
	/* 132 */ RightGun_A,
	/* 133 */ Square,
	/* 134 */ LeftSnake_A,
	/* 135 */ RightSnake_A,
	/* 136 */ T_A,
	/* 137 */ I_A,
	/* 138 */ LeftGun_A,
	/* 139 */ RightGun_A,
	/* 140 */ Square,
	/* 141 */ LeftSnake_A,
	/* 142 */ RightSnake_A,
	/* 143 */ T_A,
	/* 144 */ I_A,
	/* 145 */ LeftGun_A,
	/* 146 */ RightGun_A,
	/* 147 */ Square,
	/* 148 */ LeftSnake_A,
	/* 149 */ RightSnake_A,
	/* 150 */ T_A,
	/* 151 */ I_A,
	/* 152 */ LeftGun_A,
	/* 153 */ RightGun_A,
	/* 154 */ Square,
	/* 155 */ LeftSnake_A,
	/* 156 */ RightSnake_A,
	/* 157 */ T_A,
	/* 158 */ I_A,
	/* 159 */ LeftGun_A,
	/* 160 */ RightGun_A,
	/* 161 */ Square,
	/* 162 */ LeftSnake_A,
	/* 163 */ RightSnake_A,
	/* 164 */ T_A,
	/* 165 */ I_A,
	/* 166 */ LeftGun_A,
	/* 167 */ RightGun_A,
	/* 168 */ Square,
	/* 169 */ LeftSnake_A,
	/* 170 */ RightSnake_A,
	/* 171 */ T_A,
	/* 172 */
	/* 173 */
	/* 174 */
	/* 175 */
	/* 176 */
	/* 177 */
	/* 178 */
	/* 179 */
	/* 255 */
};

static TetrisType RandomType(void)
{
	TetrisType tet;

	if (no_pieces < sizeof(pieces)/sizeof(pieces[0]))
	{
		tet = pieces[no_pieces++];
	}
	else
	{
		no_pieces = 0;
		tet = pieces[0];
	}

	return tet;
}
#else
static TetrisType RandomType(void)
{
       unsigned int var;
       /* default */
       TetrisType tet = LeftSnake_A;
       
       /* get a random number with timer interrupt */
       DisableAllInterrupts();
       var = no_seconds % 7;
       EnableAllInterrupts();
       
       switch(var)
       {
          case 0: break;
          case 1: tet = RightSnake_A; break;
          case 2: tet = T_A; break;
          case 3: tet = I_A; break;
          case 4: tet = LeftGun_A; break;
          case 5: tet = RightGun_A; break;
          case 6: tet = Square; break;
          default: break ;
       }

       return tet;
}
#endif




