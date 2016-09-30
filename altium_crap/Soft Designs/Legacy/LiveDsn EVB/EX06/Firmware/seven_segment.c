#include "seven_segment.h"

#define DATA_PORT P3
#define CONTROL_PORT P2

//------------------------------------------
// Write a raw pattern to a display digit
//------------------------------------------

void seven_setdigit(char position, char data)
{
       //Set digit position
       DATA_PORT = 1<<position;
       CONTROL_PORT &= 0xFD;    //Pos low
       CONTROL_PORT |= 0x02;    //Pos high

       //Set digit data
       DATA_PORT = data;
       CONTROL_PORT &= 0xFB;    //WR low
       CONTROL_PORT |= 0x04;    //WR high
}

//------------------------------------------
// Fast clear of entire display
//------------------------------------------

void seven_clearall(void)
{
      DATA_PORT = 0x3f;     /* Select all digits */
      CONTROL_PORT &= 0xFD;    //Pos low
      CONTROL_PORT |= 0x02;    //Pos high

      DATA_PORT = 0;
      CONTROL_PORT &= 0xFB;    //WR low
      CONTROL_PORT |= 0x04;    //WR high
}

//------------------------------------------
// Display an ASCII character from a limited
// Set of displayable charaters
//------------------------------------------

void seven_putch(char position, char symbol)
{
     unsigned char data = 0;
     switch (symbol)
     {
            case '0': data = 0x3F;         break;
            case '1': data = 0x06;         break;
            case '2': data = 0x5B;         break;
            case '3': data = 0x4F;         break;
            case '4': data = 0x66;         break;
            case '5': data = 0x6D;         break;
            case '6': data = 0x7D;         break;
            case '7': data = 0x07;         break;
            case '8': data = 0x7f;         break;
            case '9': data = 0x6f;         break;
            case 'A': data = 0x77;         break;
            case 'a': data = 0x77;         break;
            case 'B': data = 0x7C;         break;
            case 'b': data = 0x7C;         break;
            case 'C': data = 0x39;         break;
            case 'c': data = 0x58;         break;
            case 'D': data = 0x05e;        break;
            case 'd': data = 0x05e;        break;
            case 'E': data = 0x79;         break;
            case 'e': data = 0x79;         break;
            case 'F': data = 0x71;         break;
            case 'f': data = 0x71;         break;
            case 'G': data = 0x3D;         break;
            case 'g': data = 0x3D;         break;
            case 'H': data = 0x76;         break;
            case 'h': data = 0x76;         break;
            case 'I': data = 0x30;         break;
            case 'i': data = 0x30;         break;
            case 'J': data = 0x1E;         break;
            case 'j': data = 0x1E;         break;
            case 'L': data = 0x38;         break;
            case 'l': data = 0x38;         break;
            case 'N': data = 0x37;         break;
            case 'n': data = 0x37;         break;
            case 'O': data = 0x5c;         break;
            case 'o': data = 0x5c;         break;
            case 'P': data = 0x73;         break;
            case 'p': data = 0x73;         break;
            case 'R': data = 0x31;         break;
            case 'r': data = 0x31;         break;
            case 'S': data = 0x6D;         break;
            case 's': data = 0x6D;         break;
            case 'U': data = 0x3E;         break;
            case 'u': data = 0x3E;         break;
            case 'V': data = 0x3E;         break;
            case 'v': data = 0x3E;         break;
            case 'Y': data = 0x6e;         break;
            case 'y': data = 0x6e;         break;
            case '-': data = 0x40;         break;
            case '_': data = 0x08;         break;
            case '/': data = 0x64;         break;
            case '.': data = 0x80;         break;
            case ',': data = 0x04;         break;
            case '=': data = 0x48;         break;
            case '\'': data = 0x02;        break;
            case '"': data = 0x22;         break;
            case '`': data = 0x20;         break;
            case '#': data = 0x09;         break;
     }
     seven_setdigit(position,data);
}

//------------------------------------------
// Write a string of text to the display
//------------------------------------------

void seven_puts(char pos,  const char __rom *text)
{
     while (pos<6 && *text)
     {
           seven_putch(pos,*text);
           pos++;
           text++;
     }
}

void seven_scroll_puts(char pos,  const char __rom *text)
{
     const char __rom *message;
     char i;
     message = text+pos;

     for (i=0; i<6; i++)
     {
         if (*message==0)
            message = text; // Wrap around text
         seven_putch(i,*message);
         message++;
     }
}

//------------------------------------------
// Display a long value on the display
//------------------------------------------
void seven_putlong(unsigned long value)
{
    char digit;
    char pos=5;

    while (value>0 && pos>=0)
    {
          digit = value % 10;
          value /=10;
          seven_putch(pos,'0'+digit);
          pos--;
    }
    while (pos>=0)
    {
          seven_putch(pos,'0');
          pos--;
    }
    return;
}

