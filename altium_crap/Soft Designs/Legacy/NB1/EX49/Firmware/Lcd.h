#ifndef __LCD_H__
#define __LCD_H__

#define LCD_READ_BUSY_FLAG                                 // Comment this out for time delay based waits
                                                           // otherwise busy flag is read to determine LCD status

void Delay10Us(unsigned char time);
void DelayMs(unsigned int time);

//----------------------------------------------------------------
// initialises the LCD controller
void LCD_Init(void);

//------------------------------------------
// Clears the screen
void LCD_ClrScr(void);

void LCD_WriteChar(unsigned char c);

//extern void putch(unsigned char c);

void LCD_GotoXY(unsigned char X, unsigned char Y);
//------------------------------------------------

#endif // ifndef __LCD_H__

