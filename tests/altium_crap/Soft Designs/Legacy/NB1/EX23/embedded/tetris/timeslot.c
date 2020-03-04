/****************************************************************************
 * FILE:     @(#)timeslot.c	1.4 04/08/27
 * DESCRIPTION:
 *    Time processing related issues.
 **************************************************************************/
#include <osek/osek.h>
#include "tetris.h"

DeclareTask(timeslot);
DeclareTask(tetris);
DeclareEvent(e_speed);
DeclareEvent(e_kill);
DeclareEvent(e_resume);
DeclareEvent(e_pause);
DeclareEvent(e_gravity);
DeclareEvent(e_down);

DeclareAlarm(a_gravity);
DeclareAlarm(a_speed);

unsigned int no_seconds = 0;

/****************************************************************************
   Timeslot timeslot timeslot timeslot timeslot
   This tasks controls "time processing". It is permanently waiting for
   at least one of the following events:
   - e_kill: 	stop all alarms.
   - e_resume:	resume time processing.
   - e_pause:   stop time processing
   - e_speed:   increase 'a_ts' frequency (+ difficulty).
   - e_time:    'a_ts' expired (next action).

  STarted by 'init' task.

 **************************************************************************/
TASK (timeslot)
{
	EventMaskType e;
	unsigned char counter=0;
	TaskStateType state;
	TickType      tick;

	/* start alarms */
	SetRelAlarm(a_speed,NO_LINES_PHASE,NO_LINES_PHASE);
	SetRelAlarm(a_gravity,GRAVITY_FIRST,GRAVITY_FIRST);

	while(1)
	{
		/* wait for event */
		WaitEvent(e_speed | e_kill | e_resume | e_pause | e_gravity);
		GetEvent(timeslot,&e);
		ClearEvent(e);

		/* what needs being done */

		/* stop alarms */
		if (e & e_kill)
		{
			/* in case of previous 'end of game' the 'a_gravity' alarm
			 * is not running anymore */
			if (E_OK == GetAlarm(a_gravity,&tick))
			{
				CancelAlarm(a_gravity);
			}

			/* this one runs always */
			CancelAlarm(a_speed);

#if defined(SIMINRX)
			/* start new run at the start */
			no_pieces = 0;
#endif

			/* and I am done */
			TerminateTask();
		}

		/* stop gravity feeling */
		if (e & e_pause)
		{
			/* no more gravity feeling */
			CancelAlarm(a_gravity);
		}

		/* again gravity feeling */
		if (e & e_resume)
		{
			if (GRAVITY_FIRST-counter*GRAVITY_UNIT <= 0)
			{
				SetRelAlarm(a_gravity, 1, 1);
			}
			else
			{
				SetRelAlarm(a_gravity,GRAVITY_FIRST-counter*GRAVITY_UNIT,
					GRAVITY_FIRST-counter*GRAVITY_UNIT);
			}
		}

		/* new phase: heavier gravity */
		if (e & e_speed)
		{
			counter++;
			/* new phase reached */
			UpdateScoreBoard(S_PHASE,1);
		}

		/* gravity hit  = down one line*/
		if (e & e_gravity)
		{
			GetTaskState(tetris,&state);
			if (state==SUSPENDED)
			{
				/* a new Tetris piece must come up */
				ActivateTask(tetris);
			}
			else
			{
				/* Tell Tetris piece to move one position down */
				SetEvent(tetris,e_gravity);
			}
		}
	} /* while */
}

ALARMCALLBACK(one_more_second)
{
	no_seconds += 1;
}



