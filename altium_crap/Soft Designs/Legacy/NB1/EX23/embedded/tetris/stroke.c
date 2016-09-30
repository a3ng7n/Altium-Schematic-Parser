/****************************************************************************
 *  FILE:	@(#)stroke.c	1.8 04/08/27
 *  DESCRIPTION:   
 *  The user has input an action to be taken 
 ***************************************************************************/
#include <osek/osek.h>
#include "tetris.h"

DeclareTask(tetris);
DeclareTask(init);
DeclareTask(timeslot);
DeclareEvent(e_kill);
DeclareEvent(e_rotate);
DeclareEvent(e_left);
DeclareEvent(e_right);
DeclareEvent(e_down);
DeclareEvent(e_resume);
DeclareEvent(e_pause);

 /****************************************************************************
   Key stroke handler key stroke handler
   This task is activated after user's input.
   The input reader, an interrupt or a polling task, sends a message to
   the stroke task (with activate as notification).
   The contents of the message define the action to be taken. It could be
   one the below actions:
   - move tetris to right
   - move tetris to left
   - move tetris all the way down
   - rotate tetris
   (send event to tetris task)
   - pause
   - resume
   (send event to timeslot)
   - reset
   ( send kill event to both tasks and start init again)
  ***************************************************************************/

#define M_ROTATE_ACTION()	if (OkayToSetEvent(tetris)) { SetEvent(tetris,e_rotate); }
#define M_DOWN_ACTION()		if (OkayToSetEvent(tetris)) { SetEvent(tetris,e_down); }
#define M_LEFT_ACTION()		if (OkayToSetEvent(tetris)) { SetEvent(tetris,e_left); }
#define M_RIGHT_ACTION()	if (OkayToSetEvent(tetris)) { SetEvent(tetris,e_right); }
#define M_PAUSE_ACTION()	if (OkayToSetEvent(timeslot)) { SetEvent(timeslot,e_pause); }
#define M_CONTINUE_ACTION()	if (OkayToSetEvent(timeslot)) { SetEvent(timeslot,e_resume); }
#define M_INIT_ACTION()		if (OkayToSetEvent(timeslot)) { SetEvent(timeslot,e_kill); }	\
				if (OkayToSetEvent(tetris)) { SetEvent(tetris,e_kill); }	\
				ActivateTask(init)

static void stroke_action( char key);
	
TASK (stroke)
{
	char key;
	while (1)
	{
		/* read one character from input */	
		ins(TETRISIN,&key,1);
		/* stroke actions */
		stroke_action(key);
	}
}

int OkayToSetEvent(TaskType task)
{
	TaskStateType state;

	if (GetTaskState(task, &state) == E_OK)
	{
		if (state != SUSPENDED)
		{
			return 1;
		}
	}
	return 0;
}

static void stroke_action( char key)
{

#if USE_VT100_ARROW_KEYS
       /*
 	* arrow keys in terminal generate the following sequences
 	*	up	= esc '[' 'A'
 	*	left	= esc '[' 'D'
	*	right	= esc '[' 'C'
 	*	down	= esc '[' 'B'
 	*/
	static unsigned char in_escape_mode;
	static unsigned char in_arrow_keys_mode;

	if (key == 0x1B)	/* esc */
	{
		in_escape_mode = 1;
	}
	else if (in_escape_mode == 1)
	{
		if (key == '[')
		{
			in_arrow_keys_mode = 1;
			in_escape_mode = 2;	/* the next character is the arrow key 'A', 'B', 'C' or 'D' */
		}
		else
		{
			/* don't understand the sequence, so start listening again */
			in_escape_mode = 0;
		}
	}
	else if (in_escape_mode == 2)
	{
		if (in_arrow_keys_mode == 1)	/* key is which arrow */
		{
			switch (key)
			{
				/* rotate Tetris */
				case M_ROTATE: M_ROTATE_ACTION(); break;
				/* Tetris down */
				case M_DOWN: M_DOWN_ACTION(); 	  break;
				/* Tetris right */
				case M_RIGHT: M_RIGHT_ACTION();   break;
				/* Tetris left */
				case M_LEFT: M_LEFT_ACTION(); 	  break;
				default: break;
			}
			in_arrow_keys_mode = 0;
			in_escape_mode = 0;
		}
	}
	else
	{
		switch (key)
		{
			/* Tetris in pause */
			case M_PAUSE: M_PAUSE_ACTION(); 	  break;
			/* Tetris continue */
			case M_CONTINUE: M_CONTINUE_ACTION(); 	  break;
			/* Init new game */
			case M_INIT:
				M_INIT_ACTION();
				TerminateTask(); /* because init will restart us */
				break;
			default: break;
		}
	}

#else /* USE_VT100_ARROW_KEYS */

		switch (key)
		{
			/* rotate Tetris */
			case M_ROTATE: M_ROTATE_ACTION(); break;
			/* Tetris down */
			case M_DOWN: M_DOWN_ACTION(); break;
			/* Tetris right */
			case M_RIGHT: M_RIGHT_ACTION(); break;
			/* Tetris left */
			case M_LEFT: M_LEFT_ACTION();break;
			/* Tetris in pause */
			case M_PAUSE:M_PAUSE_ACTION();break;
			/* Tetris continue */
			case M_CONTINUE: M_CONTINUE_ACTION();break;
			/* Init new game */
			case M_INIT:
				M_INIT_ACTION();
				TerminateTask(); /* because init will restart us */
				break;
			default:break;
		}

#endif /* USE_VT100_ARROW_KEYS */
	return;
}


