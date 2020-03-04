//-------------------------------------------------------------
//    _____ _______ _____  _____ ____
//   / ____|__   __|  __ \|_   _/ __ \
//  | (___    | |  | |__) | | || |  | |
//   \___ \   | |  |  _  /  | || |  | |
//   ____) |  | |  | | \ \ _| || |__| |
//  |_____/   |_|  |_|  \_\_____\____/
//
// Copyright (c) 2004 Altium
// Started: 13.07.04 Ch.W.
//          08.10.04 Ch.W. added support for unsigned long ints
// String I/O routines
//-------------------------------------------------------------

#ifndef __STRIO_H__
#define __STRIO_H__

#define __rom   // for TSK3000

#define LONG_SUPPORT 1  // define this to be able to handle longs instead of ints
                        // undefine this to save space and only handle ints

//#define __Out_Char(x)  LCD_WriteChar(x)  // use string I/O routines for LCD output

extern void __Out_Char(unsigned char c);

// ------------------------------------------------------------
// outputs 'data' with number format 'radix'
// output gets padded with 'pad' character until 'width' is reached
// binary, octal decimal and hex are handled correctly
// hex is upper case only
// calls __Out_Char(x) to perform actual character output
// __OutChar() must be user supplied eg.
// #define __Out_Char(x)  putchar(x)
// ------------------------------------------------------------
#ifndef LONG_SUPPORT
void OutInt(unsigned int data, unsigned char radix, unsigned char width, unsigned char pad);
#else
void OutInt(unsigned long data, unsigned char radix, unsigned char width, unsigned char pad);
#endif

// ------------------------------------------------------------
// lean version of printf for single integer argument
// current support is for unsigned integers only
// hex is upper case only
// width and zero padding are implemented
// %u : decimal
// %x : hex (upper case)
// %b : binary
// %o : octal
// %c : character
// calls __Out_Char(x) to perform actual character output
// __OutChar() must be user supplied eg.
// #define __Out_Char(x)  putchar(x)
// ------------------------------------------------------------
#ifndef LONG_SUPPORT
void OutStr(const char __rom * format, unsigned int data);
#else
void OutStr(const char __rom * format, unsigned long data);
#endif


#endif // __STRIO_H__

