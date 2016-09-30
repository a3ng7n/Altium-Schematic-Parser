//      _   __ ____  ______     __    ______ ____      __  __
//     / | / // __ )/_  __/    / /   / ____// __ \    / / / /
//    /  |/ // __  | / /      / /   / /    / / / /   / /_/ /
//   / /|  // /_/ / / /      / /___/ /___ / /_/ /_  / __  /
//  /_/ |_//_____/ /_/______/_____/\____//_____/(_)/_/ /_/
//                   /_____/
// (c) 2003 Altium
// Started: 20.11.2003 Ch.W.
// LCD driver Routines for NanoBoard Tester

#ifndef __NBT_LCD_H__
#define __NBT_LCD_H__

#define LCD_READ_BUSY_FLAG  // Comment this out for time delay based waits

void Delay10Us(unsigned char time);
void DelayMs(unsigned int time);

//----------------------------------------------------------------   
// initialises the LCD controller
void LCD_Init(void);

//------------------------------------------------------------
// controls Cursor appearance
// Visible: 0     = no cursor
//          non-0 = cursor visible
// Blink  : 0     = don't blink
//          non-0 = cursor blinks
//------------------------------------------------------------
void LCD_SetCursor(unsigned char Visible, unsigned char Blink);


//------------------------------------------
// Clears the screen
void LCD_ClrScr(void);

void LCD_WriteChar(unsigned char c);

extern void putch(unsigned char c);

void LCD_GotoXY(unsigned char X, unsigned char Y);
//------------------------------------------------

// -------------------------------------------------------------------------------------------
// programs custom character bitpattern for one of 8 custom characters
// data points to an array of 8 characters making up the bitpattern of the custom character
// *data, bits 5..0 are visible only, the top row is *data, set bits are dark, lsb is on right
// -------------------------------------------------------------------------------------------
void LCD_SetCustomChar(unsigned char CharNo, const char __rom* data);


#endif // ifndef __NBT_LCD_H__

