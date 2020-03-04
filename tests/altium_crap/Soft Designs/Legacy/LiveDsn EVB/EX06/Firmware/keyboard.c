#include "keyboard.h"

#define KEY_PORT P3

static char lastkey = 0;

//-------------------------------------
// Delay function for keyboard timing
//-------------------------------------

void delay (int loops, int scale)
{
    for (int j = 0; j<scale; j++){
        for (int i = 0; i<loops; i++){
          __asm ("nop");
        }
    }
}

//----------------------------------------
// Check to see if a key has been pressed
//----------------------------------------

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

//-----------------------------------------------------
// Wait until key is pressed and then return ASCII code
//-----------------------------------------------------

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

