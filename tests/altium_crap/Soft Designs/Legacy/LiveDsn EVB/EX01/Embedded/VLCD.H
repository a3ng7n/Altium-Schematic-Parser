//      __      __ _       _____  _____
//      \ \    / /| |     / ____||  __ \
//       \ \  / / | |    | |     | |  | |
//        \ \/ /  | |    | |     | |  | |
//         \  /   | |____| |____ | |__| |
//          \/    |______|\_____||_____/
//
// (c) 2004 Altium
// Started: 20.08.2004 Ch.W.
// Virtual LCD driver Routines for EVBoard Tester
// made to be compatible with 'real' LCD.H to simplify
// conversion of older projects

#ifndef __VLCD_H__
#define __VLCD_H__

#define LCD_READ_BUSY_FLAG  // Comment this out for time delay based waits
                            // otherwise busy flag is read to determine LCD status

void Delay10Us(unsigned char time);
void DelayMs(unsigned int time);

//----------------------------------------------------------------
// initialises the LCD controller
void LCD_Init(void);


//-------------------------------------
// turns LCD Backlight on or off
void LCD_SetBacklight(unsigned char On);

//--------------------------------------
// returns state of LCD backlight
unsigned char LCD_GetBacklight(void);



//------------------------------------------
// Clears the screen
void LCD_ClrScr(void);

void LCD_WriteChar(unsigned char c);

void LCD_GotoXY(unsigned char X, unsigned char Y);
//------------------------------------------------

#endif // ifndef __VLCD_H__



