#include <stdint.h>
#include <stdbool.h>

void xormachine(bool INPORT0, bool INPORT1, bool * OUTPORT)
{
        *OUTPORT = INPORT0 ^ INPORT1;
}

