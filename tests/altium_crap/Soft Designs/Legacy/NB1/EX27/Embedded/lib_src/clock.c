/**************************************************************************
**                                                                        *
**  FILE        :  clock.c                                                *
**                                                                        *
**  DESCRIPTION :  The clock function returns the current processor time. *
**                 To determine the time in seconds, the value returned   *
**                 by the clock function should be divided by the value   *
**                 of the macro CLOCKS_PER_SEC, defined in <time.h>.      * 
**                                                                        *
**  Copyright 1996-2008 Altium BV                                         *
**                                                                        *
**************************************************************************/

#include <time.h>

/* The _clock() function is used by CrossView to simulate a timer tick register. */
/* This function must have 'extern' scope, to allow the simulator to set a breakpoint. */

clock_t _clock ( clock_t t )
{
        clock_t         cycles;
        
        __asm( "clk_loop:\n"
               "        mfc0    $a0,$4\n"
               "        mfc0    %0.1,$3\n"
               "        mfc0    %0.0,$4\n"
               "        bne     %0.0,$a0,clk_loop\n"
               "        nop\n" : "=R"(cycles) :: "$a0" );

        return cycles;
}

clock_t clock ( void )
{
        return _clock( (clock_t)-1 );
}
