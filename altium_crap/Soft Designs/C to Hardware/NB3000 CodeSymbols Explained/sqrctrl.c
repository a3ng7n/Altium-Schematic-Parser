#include <stdint.h>
#include <stdbool.h>

__input  float FIFO_DATA;
__output bool  FIFO_RE = 0;
__input  bool  FIFO_EMPTY;
__output bool  FIFO_RE;

// Note: This function is intended to demonstrate a pipeline of C Code Symbols.
//       Normally you would compute the square of the float by multiplying
//       the variable containing floating point number.

// Function "void unpack_float(float f)" is an external function. Its prototype
// is "imported" via the C Code Symbol's dialog "External Functions" pane.
// Function "void unpack_float(float f)" is implemented using a C Code Symbol.
// The wires required to connect to that code symbol are created as a result of
// importing this function.
// To summarize: a function prototype is translated into a set of ports.

float do_sqr(float DAT, uint18_t ADR, uint4_t SEL, bool WE)
{
    if (WE)
    {   // Path taken if core writes data to this code symbol
        __debug_printf("DAT=%f\n", DAT);
        unpack_float(DAT);
        return 0.0;
    }
    else
    {   // Path taken if core read data from this code symbol
        float result;
        while (FIFO_EMPTY)
        {
            FIFO_RE=0;
        }
        FIFO_RE=1;
        result = FIFO_DATA;
        FIFO_RE=0;
        __debug_printf("result = %f\n", result);
        return result;
    }
}

