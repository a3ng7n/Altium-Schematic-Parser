#include "Interruptnm.h"

/*****************************************************************************
|*
|*  Function:           InterruptIntHandler
|*
|*  Description:
|*
|*      This interrupt function will be called every time Int0
|*	occured. 
|*	This interrupt function just calls the function Interrupt0(), an
|*	external function to be defined in another module.
|*
 */
__interrupt(-1) void InterruptNmIntHandler(void) __at(0x66)
{
    InterruptNm();
}

//__interrupt(0x66) void InterruptNmIntHandler( void )
//{
//    InterruptNm();
//}
