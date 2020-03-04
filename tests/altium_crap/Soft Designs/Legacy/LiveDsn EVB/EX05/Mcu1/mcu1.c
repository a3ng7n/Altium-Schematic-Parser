/* Console Demo */

#include "conio.h"
#include <string.h>

void displayTitle(void)
{
    static char __rom titleStr[] = "---- Console Demo  - TSK51 ----\n\n";
    settextcolor(YELLOW);
    settextbackground(BLACK);
    setcursormode(0);
    clrscr();
    cputs(titleStr);
    settextcolor(LIGHTGRAY);
}

void displayOption1(void)
{
    char key;
    static char __rom menuStr[] = "Option #1 - Key entry demo. \n\n"
                                  "Use Keys [1..5] to type\n"
                                  "key [6] to exit.\n\n";
    displayTitle();
    cputs(menuStr);
    setcursormode(1);
    settextcolor(LIGHTGREEN);

    while ((key=getch()) && (key!='6'))
          putch(key);
}

void displayOption2(void)
{
    char i;
    static char __rom menuStr[] = "Option #2 - Text Colour\n\n";
    static char __rom colourStr[] = "Text Colour #";
    static char __rom pressanyStr[] = "\n<Press any Key to Continue>";

    displayTitle();
    cputs(menuStr);

    for(i=0; i<16; i++)
    {
        settextcolor(i);
        cputs(colourStr);
        putch((i<10)?('0'+i):('A'+i-10));
        putch('\n');
    }
    settextcolor(LIGHTGRAY);
    cputs(pressanyStr);
    getch();
}

void displayOption3(void)
{
    char i;
    static char __rom menuStr[] = "Option #3 - Background Colour\n\n";
    static char __rom colourStr[] = "Background Colour #";
    static char __rom pressanyStr[] = "\n<Press any Key to Continue>";

    displayTitle();
    cputs(menuStr);

    for(i=0; i<16; i++)
    {
        settextbackground(i);
        cputs(colourStr);
        putch((i<10)?('0'+i):('A'+i-10));
        putch('\n');
    }
    settextbackground(BLACK);
    cputs(pressanyStr);
    getch();
}

void displayOption4(void)
{
    static char __rom menuStr[] = "Option #4 - Cursor Demo\n\n";
    static char __rom point1Str[] = "(10,10)";
    static char __rom point2Str[] = "(20,10)";
    static char __rom point3Str[] = "(10,20)";
    static char __rom pressanyStr[] = "\n<Press any Key to Continue>";

    displayTitle();
    cputs(menuStr);

    gotoxy(10,10);
    cputs(point1Str);

    gotoxy(20,10);
    cputs(point2Str);

    gotoxy(10,20);
    cputs(point3Str);

    gotoxy(0,30);
    cputs(pressanyStr);
    getch();
}


void displayMainMenu(void)
{
    static char __rom menuStr[] =  "Main menu :-\n\n"
                                   "[1] Key entry demo\n"
                                   "[2] Text Colour demo\n"
                                   "[3] Background Colour Demo\n"
                                   "[4] Cursor Demo\n"
                                   "\nPress a key [1..4]";
    displayTitle();
    cputs(menuStr);
}

void main (void)
{
    PSW = 0x00;
    char key;
    displayMainMenu();

    for (;;)
    {
        if (kbhit())
        {
           key = getch();
           switch (key)
           {
                  case '1':   displayOption1();           break;
                  case '2':   displayOption2();           break;
                  case '3':   displayOption3();           break;
                  case '4':   displayOption4();           break;
                  case '5':   /* Spare */                 break;
                  case '6':   /* Spare */                 break;
           }
           displayMainMenu();
        }
    }
}
