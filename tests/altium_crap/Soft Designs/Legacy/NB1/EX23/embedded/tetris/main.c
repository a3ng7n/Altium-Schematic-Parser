/******************************************************************************
 *  FILE:		@(#)main.c	1.5 04/08/11
 *  DESCRIPTION:
 *      Source code for the main module of demo/tetris examples.
 *	Both projects are used mainly as development aid.
 *****************************************************************************/
#include <osek/osek.h>
#if defined(DEMO)
#include "demo.h"
#endif
#if defined(TETRIS)
#include "tetris.h"
#endif

volatile AppModeType mode;

DeclareAppMode(APPMODE1);

/******************************************************
 * you reach the main label with the standard
 * startup code added automatically by the toolchain.
 * (Or write your own startup code)   
 *****************************************************/
int main(void)
{
    /* let us start with the first of the two modes */
    mode = APPMODE1;

    /************************************************
     * Start RTOS for mode APPMODE1 and when
     * system undergoes a 'shutdown' the RTOS shall
     * start again with the application mode 'mode'
     ***********************************************/
        while (1)
        {
            StartOS(mode);
        }

    /* this return to exit: never happens */
        return 1;
}


