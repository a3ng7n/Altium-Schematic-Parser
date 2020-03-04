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


#include "strio.h"

// ------------------------------------------------------------
// outputs 'data' with number format 'radix'
// output gets padded with 'pad' character until 'width' is reached
// binary, octal decimal and hex are handled correctly
// hex is upper case only
// calls __Out_Char(x) to perform actual character output
// __OutChar() must be user supplied eg.
// #define __Out_Char(x)  putchar(x)
// ------------------------------------------------------------
void OutInt(unsigned int data, unsigned char radix, unsigned char width, unsigned char pad)
{
   unsigned int digits = 1;  // counter for number of digits
   unsigned int divider = 1;
   while(data / divider >= radix)  // determine number of digits
   {
     divider *= radix;
     digits ++;
   }
   while(digits < width--)
     __Out_Char(pad);
   while(divider)    // output digits starting from most significant digit
   {
       unsigned char outchar;
       outchar = (data/divider);
       outchar +='0';
       if(outchar >'9')
          outchar += 'A' - '0' - 10;
       __Out_Char(outchar);
       data %= divider;
       divider /= radix;
   }
}


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
void OutStr(const char __rom * format, unsigned int data)
{
   char c;        // current format string character
   unsigned char radix = 10; // radix  for integer output
   unsigned int digits = 1;  // counter for number of digits
   unsigned int divider = 1;
   while(c=*format++)  // read next character
   {
      if(c != '%')
         __Out_Char(c);
      else          // found format character ?
      {
         unsigned char width = 0;  // minimum width of format string
         unsigned char IgnoreZerosForPadding = 0;
         unsigned char PadCharacter = ' ';
         unsigned char FormatCharActive = 1;  // flag while processing % directive
         while(FormatCharActive)
         switch(c=*format++)
         {
           case '%':
              __Out_Char(c);          // two /%'s in a row -> output one
              FormatCharActive = 0;   // and return to normal string output
              break;
           case 'x':
           case 'X':
              radix = 16;             // hex characters to follow
              OutInt(data,radix,width,PadCharacter);  // output in hex
              FormatCharActive = 0;   // and return to normal string output
              break;
           case 'u':
           case 'd':                  // TODO: signed decimal is not supported, yet
              radix = 10;
              OutInt(data,radix,width,PadCharacter);  // output in decimal
              FormatCharActive = 0;   // and return to normal string output
              break;
           case 'c':
              radix = 10;
              __Out_Char((unsigned char)data);  // output in character
              FormatCharActive = 0;   // and return to normal string output
              break;
           case 'o':
              radix = 8;
              OutInt(data,radix,width,PadCharacter);  // output in octal
              FormatCharActive = 0;   // and return to normal string output
              break;
           case 'b':
              radix = 2;
              OutInt(data,radix,width,PadCharacter);  // output in binary
              FormatCharActive = 0;   // and return to normal string output
              break;
           case '0':
              if(!IgnoreZerosForPadding)
              {
                 PadCharacter = '0';   // set flag for padding with 0;
              }                        //
           default:
              if((c >= '0') && (c <='9'))  // is it a character?
              {
                 width = width * 10 + (c - '0');
                 IgnoreZerosForPadding = 1;         // ignore 0 padding after 1st non-0 digit
              }
              else                      // end of format token?
              {
               __Out_Char(c);          // output it
               FormatCharActive = 0;   // and return to normal string output
              }
              break;
         }
      }
   }
}


