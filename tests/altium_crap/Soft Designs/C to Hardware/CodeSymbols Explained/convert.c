#include <stdint.h>
#include <stdbool.h>

int convert( int16_t int16, int8_t int8, float f, double d, char command )
{
    int retval = 0;
    switch ( command )
    {
        case 1:  retval = (int)int16;  break;
        case 2:  retval = (int)int8;   break;
        case 3:  retval = (int)f;      break;
        case 4:  retval = (int)d;      break;
        default: retval = -1;          break;
    }
    return retval;
}

