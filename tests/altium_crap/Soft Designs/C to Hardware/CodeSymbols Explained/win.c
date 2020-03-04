#include <stdint.h>
#include <stdbool.h>

void win()
{
    // Print to virtual debug instrument "CODE_SYMBOL_CONSOLE"
    // Open the intrument on the "Devices View" page.
    // __debug_printf supports the same flag characters, modifiers, and conversion specifiers as printf().
    //    Note: pointer dereferences are not supported.
    //    Supported: flag characters: -,+
    //               modifiers: h, l, ll, hh, j, z, L
    //               conversion specifiers: d,i,o,u,x,X,c,f,F,e,E,g,G,a,A

    __debug_printf("goal\n");
}

