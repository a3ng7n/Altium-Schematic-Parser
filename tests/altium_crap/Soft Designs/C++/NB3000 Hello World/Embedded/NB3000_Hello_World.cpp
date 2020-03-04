/*****************************************************************************\
|*
|*  IN PACKAGE:         C++ examples
|*
|*  COPYRIGHT:          Copyright (c) 2009, Altium
|*
|*  DESCRIPTION:        "Hello World" C++ program
|*
|*                      Standard output (stdout) and standard error (stderr)
|*                      are connected to the graphics driver
 */

// clear screen escape sequence
#define CLRSCR          "\x1B[2J"

#include <iostream>

using namespace std;

int main( void )
{
    cout << CLRSCR << "Hello world!" << endl;
    return 0;
}

