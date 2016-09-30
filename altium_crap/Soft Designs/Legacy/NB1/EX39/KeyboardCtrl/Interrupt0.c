#include "Interrupt0.h"

/*****************************************************************************
|*
|*  Function:           InterruptIntHandler
|*
|*  Description:
|*
|*      This interrupt function will be called every time Int0
|*  occured. 
|*  This interrupt function just calls the function Interrupt0(), an
|*  external function to be defined in another module.
|*
 */
__interrupt(9*8+3) void
Interrupt0IntHandler( void )
{
   EAL = 0 ;
    Interrupt0();
}

/*****************************************************************************
|*
|*  Function:           InitInterrupt0
|*
|*  Description: Enable Int0
|*
 */
void
InitInterrupt0( void )
{
   I2FR = 1 ;
   EX2 = 1 ;
//    IT0 = 1;
//    EX0 = 1;    /* Enable Int0 interrupt */
}

/*****************************************************************************
|*
|*  Function:           EnableInt0Interrupt
|*
|*  Description:
|*
 */
void EnableInt0Interrupt( void )
{
    EX0 = 1;    /* Enable timer 0 interrupt */
}

/*****************************************************************************
|*
|*  Function:           DisableInt0Interrupt
|*
|*  Description:
|*
 */
void DisableInt0Interrupt( void )
{
    EX0 = 0;    /* Disable Int 0 interrupt */
}

