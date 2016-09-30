/******************************************************************************
 * FILE:	@(#)init.c	1.8 04/09/15	
 * DESCRIPTION:
 * 	Source for init task
 *****************************************************************************/
#include <osek/osek.h>

#if defined(DEMO)
#include "demo.h"
#endif
#if defined(TETRIS)
#include "tetris.h"
#endif

#if defined(DEMO)
DeclareComAppMode(COMINTERNAL);
DeclareTask(monitor);
#if defined(SIMINRX)
DeclareTask(tsiminnk);
#endif
#endif

#if defined(TETRIS)
DeclareTask(timeslot);
DeclareTask(tetris);
DeclareTask(stroke);
#if defined(SIMINRX)
DeclareTask(tsiminnk);
#endif
#endif


/************************************************
 * Always autostart of the system.      	*
 ***********************************************/
 TASK (init)
 {       
#if defined(DEMO) 
        /* start com component */
        StartCOM(COMINTERNAL);
	/* Activate monitor shell */
	ActivateTask(monitor);
#if defined(SIMINRX)
	ActivateTask(tsiminnk);
#endif
#endif
	
#if defined(TETRIS)
	/* 'tetris' does not run now */
    	BoardInit(black,white);
	InitScoreBoard();
	/* Activate tasks */
    	ActivateTask(stroke);
    	ActivateTask(timeslot);
    	ActivateTask(tetris);
#if defined(SIMINRX)
	ActivateTask(tsiminnk);
#endif
#endif	
	TerminateTask();
 }


