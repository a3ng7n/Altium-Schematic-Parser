/*********************************************************************
|*
|*  Function    : Accumulate
|*
|*  Parameters  : VALUE_I, TOTAL_O
|*
|*  Returns     : none
|*
|*  Description : Adds VALUE_I to the internal total. The total is
|*                placed in TOTAL_O.
 */

#include <stdint.h>

static uint32_t s_Total = 0;

void Accumulate(uint32_t VALUE_I, uint32_t* TOTAL_O)
{
    s_Total += VALUE_I;
    *TOTAL_O = s_Total;
}
