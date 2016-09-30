
#ifndef CONIO_H
#define CONIO_H

//----------------------------------
// Colour constants
//----------------------------------

#define BLACK        0
#define BLUE         1
#define GREEN        2
#define CYAN         3
#define RED          4
#define MAGENTA      5
#define BROWN        6
#define LIGHTGRAY    7
#define DARKGRAY     8
#define LIGHTBLUE    9
#define LIGHTGREEN   10
#define LIGHTCYAN    11
#define LIGHTRED     12
#define LIGHTMAGENTA 13
#define YELLOW       14
#define WHITE        15

//----------------------------------
// Display functions
//----------------------------------

char putch(char symbol);
int  cputs(char __rom *p);
void gotoxy(char x, char y);
void clrscr(void);

void settextbackground(char color);
void settextcolor(char color);
void setbordercolor(char color);

void setcursormode(char mode);
void storecursor(void);
void recallcursor(void);

void settextpage(char page);


//----------------------------------
// Keyboard functions
//----------------------------------

char kbhit(void);
char getch(void);

//----------------------------------
// Other functions
//----------------------------------

void delay (int loops, int scale);
void putHex(char value);

#endif
