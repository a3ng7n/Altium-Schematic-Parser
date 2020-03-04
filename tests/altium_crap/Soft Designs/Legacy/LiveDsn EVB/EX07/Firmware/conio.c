#include "conio.h"

//----------------------------------------------------
// Interface ports for console controller and keyboard
//----------------------------------------------------

#define DATA_PORT P3
#define CONTROL_PORT P2
#define KEY_PORT P3

//--------------------------------
// Send a character to the console
//--------------------------------

char putch(char symbol)
{
    while (CONTROL_PORT&0x80); // Wait until console is ready
    DATA_PORT = symbol;
    CONTROL_PORT &=0xfe;
    CONTROL_PORT |=1;
    return symbol;
}

//------------------------------
// Send a string to the console
//------------------------------

int cputs(const char __rom *p)
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
// Set the Border Colour
//--------------------------------

void setbordercolor(char color)
{
    putch(20);
    putch(color);
}

//--------------------------------
// Enable/Disable the cursor
//--------------------------------

void setcursormode(char mode)
{
   putch(4|(mode &1));
}

//-------------------------------------
// Store the cursor position and colour
//-------------------------------------

void storecursor(void)
{
   putch(15);
}

//--------------------------------------
// Recall the cursor position and colour
//--------------------------------------

void recallcursor(void)
{
   putch(16);
}

//--------------------------------
// Set Display Page
//--------------------------------

void settextpage(char page)
{
   putch(18|(page &1));
}


//---------------------------------
// Check keyboard to see if key hit
//---------------------------------

static char lastkey = 0;

char kbhit(void)
{
    char code;
    char firstcode = (~KEY_PORT)&0x3f;

    lastkey = 0;
    if (firstcode) /* Key pressed */
    {
       delay(300,2);
       code = (~KEY_PORT)&0x3f;
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


void putHex(char value)
{
    char nibble = (value >>4) & 0xf;
    putch((nibble<10)?('0'+nibble):('A'+nibble-10));
    nibble = value &0xf;
    putch((nibble<10)?('0'+nibble):('A'+nibble-10));
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
