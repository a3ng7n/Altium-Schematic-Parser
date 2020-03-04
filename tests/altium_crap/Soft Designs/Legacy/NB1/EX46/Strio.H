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
// String I/O routines
//-------------------------------------------------------------

#ifndef __STRIO_H__
#define __STRIO_H__

#define __Out_Char(x)  LCD_WriteChar(x)  // use string I/O routines for LCD output

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
void OutInt(unsigned int data, unsigned char radix, unsigned char width, unsigned char pad);


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
void OutStr(const char __rom * format, unsigned int data);


#endif // __STRIO_H__

