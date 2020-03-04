#include "conio.h"

//--------------------------------
// Send a character to the console
//--------------------------------

char putch(char symbol)
{
    while (P2&0x1); // Wait untill console is ready
    P1 = symbol;
    P2 &=0xfe;
    P2 |=1;
    return symbol;
}

//------------------------------
// Send a string to the console
//------------------------------

int cputs(char __rom *p)
{
     while (*p != 0)
     {
           putch(*p);
           p++;
     }
     return 0;
}

//--------------------------------
// Clear the console
//--------------------------------

void clrscr(void)
{
    putch(1);
}

//--------------------------------
// Set the cursor position
//--------------------------------

void gotoxy(char x, char y)
{
    putch(6);
    putch(x);
    putch(7);
    putch(y);
}

//--------------------------------
// Set the Text Colour
//--------------------------------

void settextcolor(char color)
{
    putch(2);
    putch(color);
}

//--------------------------------
// Set the background text colour
//--------------------------------

void settextbackground(char color)
{
    putch(17);
    putch(color);
}

//--------------------------------
// Enable/Disable the cursor
//--------------------------------

void setcursormode(char mode)
{
   putch(4|(mode &1));
}

//---------------------------------
// Check keyboard to see if key hit
//---------------------------------

static char lastkey = 0;

char kbhit(void)
{
    char code;
    char firstcode = (~P0)&0x3f;

    lastkey = 0;
    if (firstcode) /* Key pressed */
    {
       delay(300,2);
       code = (~P0)&0x3f;
       if (code ==firstcode)
       {
           if (code == 0x01)
             lastkey = '1';
         else if (code & 0x02)
             lastkey = '2';
          else if (code & 0x05)
             lastkey = '3';
          else if (code & 0x08)
             lastkey = '4';
          else if (code & 0x10)
             lastkey = '5';
          else if (code & 0x20)
             lastkey = '6';
          else
             lastkey = 0;
       }
       else
         lastkey = 0;
    }
    return (lastkey>0);
}

//-----------------------------------------------
// Wait until key is pressed and then return code
//-----------------------------------------------

char getch(void)
{
     char key;
     while (lastkey==0)
           kbhit();
     key=lastkey;
     while (lastkey !=0)
           kbhit();
     return key;
}


void delay (int loops, int scale)
{
    for (int j = 0; j<scale; j++){
        for (int i = 0; i<loops; i++){
          __asm ("nop");
        }
    }
}
