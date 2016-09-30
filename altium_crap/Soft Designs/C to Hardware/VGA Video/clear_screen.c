#include "video.h"

/*
 * This component is used to clear the video memory after a reset.
 */

__output          bool    BUS_REQUEST;                                          // Bus request flag
__OMEM            pixel_t vga[VGA_X_RES * VGA_Y_RES] __at(0);                   // Video memory

void clear_screen( void )
{
    BUS_REQUEST = 1;                                                             // Request the video bus

    for ( int addr = 0; addr < VGA_X_RES * VGA_Y_RES; addr++ )                   // Fill the video memory with black pixels
    {
        vga[addr] = 0;
    }

    BUS_REQUEST = 0;                                                             // Release the video bus
}

