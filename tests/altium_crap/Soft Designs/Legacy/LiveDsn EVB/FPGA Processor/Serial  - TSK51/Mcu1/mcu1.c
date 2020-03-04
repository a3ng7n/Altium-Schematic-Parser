/* EB1 Terminal Demo - TSK */

#include "conio.h"
#include "serial.h"
#include <string.h>

//-----------------------------
// Macro button  definitions
//-----------------------------

static char __rom macro1[] = "ATDT 1234\r\n";                   //Dial AT modem command
static char __rom macro2[] = "ATH\r\n";                         //Hang up AT Modem command
static char __rom macro3[] = "ATA\r\n";                         //Answer AT modem command
static char __rom macro4[] = "USER fred\r\nPASS 1234\r\n";      //Log into system
static char __rom macro5[] = "ATS0?\r\n";                       //Get modem status

//------------------------------------------
// Message string constants
//------------------------------------------

static char __rom pressanyStr[] = "\n<Press any Key to Return>";
static char __rom offStr[] = " OFF";
static char __rom onStr[] =  " ON ";

//------------------------------------------
// Terminal configuration variables
//------------------------------------------

char local_echo = 1;        // Local echo flag
char speed = 3;             // Comms speed setting

void displayTitle(void)
{
    static char __rom titleStr[] = "-- EvalBoard Terminal (TSK51) --\n\n";
    settextcolor(YELLOW);
    setcursormode(0);
    clrscr();
    cputs(titleStr);
}

void clearTerminalScreen(void)
{
     static char __rom helpStr[] = "Press key 6 to configure.\n\n";
     settextpage(0);
     settextbackground(BLACK);
     setbordercolor(BLACK);
     displayTitle();
     settextcolor(LIGHTGRAY);
     cputs(helpStr);
     setcursormode(1);
}

void displayMacro(char symbol, char __rom *text)
{
     settextcolor(YELLOW);
     putch('[');
     settextcolor(WHITE);
     putch('K');
     putch('E');
     putch('Y');
     putch(' ');
     putch(symbol);
     settextcolor(YELLOW);
     putch(']');
     putch('\n');
     settextbackground(LIGHTCYAN);
     settextcolor(BLACK);
     cputs(text);
     putch('\n');
     settextbackground(GREEN);
}

void displayMacroButtons(void)
{
    static char __rom menuStr[] = "Configuration - View Macro keys. \n\n";

    //Default Attributes
    settextbackground(GREEN);
    settextcolor(YELLOW);

    displayTitle();
    cputs(menuStr);
    displayMacro('1',macro1);
    displayMacro('2',macro2);
    displayMacro('3',macro3);
    displayMacro('4',macro4);
    displayMacro('5',macro5);
    settextcolor(YELLOW);
    cputs(pressanyStr);
    getch();
}


void displayConfigurationMenu(void)
{
    char exit=0;
    char refresh=1;
    static char __rom menuStr[] = "Configuration - Main Menu.\n\n"
                                  " [1] View Macros.\n\n"
                                  " [2] Local Echo\n\n"
                                  " [3] Speed\n\n"
                                  " [6] Enter Terminal mode.";

   static char __rom speed0[] = " 1200  ";
   static char __rom speed1[] = " 2400  ";
   static char __rom speed2[] = " 4800  ";
   static char __rom speed3[] = " 9600  ";
   static char __rom speed4[] = " 19200 ";

    settextpage(1);
    storecursor();

    while (!exit)
    {
        if (refresh)
        {
            settextbackground(GREEN);
            setbordercolor(GREEN);
            displayTitle();
            cputs(menuStr);

            //Display settings
            settextbackground(LIGHTCYAN);
            settextcolor(BLACK);

            //Display echo mode
            gotoxy(16,6);
            cputs(local_echo?onStr:offStr);

            //Display speed
            gotoxy(12,8);
            switch (speed)
            {
                   case 0:  cputs(speed0); break;
                   case 1:  cputs(speed1); break;
                   case 2:  cputs(speed2); break;
                   case 3:  cputs(speed3); break;
                   case 4:  cputs(speed4); break;
            }

            refresh = 0;
        }

        if (kbhit()) // Function key pressed
        {
              switch (getch())
              {
                     case '1':  displayMacroButtons();        refresh=1;        break;
                     case '2':  local_echo = !local_echo;     refresh=1;        break;
                     case '3':  speed = (speed+1) %5;         refresh=1;        break;
                     case '4':                                                  break;
                     case '5':                                                  break;
                     case '6':  exit = 1;                                       break;
              }
        }
    }

    //Return to terminal screen
    settextpage(0);
    setcursormode(1);
    setbordercolor(BLACK);
    recallcursor();

    serial_init(speed);
};


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

void sendMacro(char __rom *p)
{
   char symbol;
   while (*p != 0)
   {
          serial_putch(*p);
          if (local_echo)
             putch(*p);
          p++;
          if ((symbol=serial_getch()))
              putch(symbol);
   }
}

void main (void)
{
    PSW = 0x00;
    char symbol;

    displayConfigurationMenu();
    clearTerminalScreen();

    for(;;)
    {
           if (kbhit()) // Function key pressed
           {
              switch (getch())
              {
                     case '1':  sendMacro(macro1);             break;
                     case '2':  sendMacro(macro2);             break;
                     case '3':  sendMacro(macro3);             break;
                     case '4':  sendMacro(macro4);             break;
                     case '5':  sendMacro(macro5);             break;
                     case '6':  displayConfigurationMenu();    break;
              }
           }
           if ((symbol=serial_getch()))
              putch(symbol);
    }
}
