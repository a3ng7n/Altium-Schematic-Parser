#include <stdbool.h>
#include <stdint.h>
#include "swplatform.h"

#define BASE (*((uint32_t volatile *) WB_MULTI_MANUAL_BASEADDRESS))

// Interface to convert() implemented in file convert.c
// We could use qualifiers to let the embedded compiler generate
// the interface stubs, like for swap().
//
// __rtl __export __CC(wishbone, 0)
//  int convert( int16_t int16, int8_t int8, float f, double d, char c );
//
// But here we pass the parameters and return values manually over the io-bus.
// Static stack: parameters are written in order of appearance and aligned:
//
//         BASE     | 4   5 |6   |7  | 8 .. 11 |  12..19   | 20       |
// write:  startbit | int16 |int8|   | float f |  double d | char cmd |
// read:   donebit  | int32 retval   |
//
// For more information about the calling convention see the C-to-Hardware Compiler User Manual

static void codesymbol_write_parameters( int16_t i16, int8_t i8, float f, double d, char command )
{
    *(int16_t volatile *)(WB_MULTI_MANUAL_BASEADDRESS +  4) = i16;
    *(int8_t volatile *) (WB_MULTI_MANUAL_BASEADDRESS +  6) = i8;
    *(float volatile *)  (WB_MULTI_MANUAL_BASEADDRESS +  8) = f;
    *(double volatile *) (WB_MULTI_MANUAL_BASEADDRESS + 12) = d;
    *(char *)            (WB_MULTI_MANUAL_BASEADDRESS + 20) = command;
}

// The static stack is not cleared or overwritten by return values,
// so we can safely reuse old parameters.
static void codesymbol_write_command( char command )
{
    *(char *)(WB_MULTI_MANUAL_BASEADDRESS + 20) = command;
}

static void codesymbol_start( void )
{
    BASE = 1;             // write 0-th start bit
}

static bool codesymbol_is_done( void )
{
    return BASE & 1;      // test 0-th done-bit
}

static int codesymbol_return_value( void )
{
    return *(int32_t volatile *)(WB_MULTI_MANUAL_BASEADDRESS +  4);
}

/* The full function call. This function is equivalent to what the compiler would generate
 * if a function call was used.
 */
static int codesymbol_convert( int16_t i16, int8_t i8, float f, double d, char cmd )
{
    codesymbol_write_parameters( i16, i8, f, d, cmd); // parameters can be written while codesymbol is busy
    while ( !codesymbol_is_done()) ;                  // codesymbol needs to be idle, otherwise start is ignored.
    codesymbol_start();
    while ( !codesymbol_is_done()) ;                  // returnvalues are ready when done is set.
    return codesymbol_return_value();
}

/* The same function call, but now only the 'command' parameter is written.
 * the values of the other parameters are reused from the previous call.
 */
static int codesymbol_command( char cmd )
{
    codesymbol_write_command( cmd);     // other parameters are reused
    while ( !codesymbol_is_done()) ;    // codesymbol needs to be idle, otherwise start is ignored.
    codesymbol_start();
    while ( !codesymbol_is_done()) ;    // returnvalues are ready when done is set.
    return codesymbol_return_value();
}


int  test_convert( void )
{
    int one   = codesymbol_convert( 1, 2, 3.f, 4., 1);
    int two   = codesymbol_command( 2);
    int three = codesymbol_command( 3);
    int four  = codesymbol_command( 4);

    return one + two + three + four;
}
