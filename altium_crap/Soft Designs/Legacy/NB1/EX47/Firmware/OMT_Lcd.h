#ifndef __NBT_LCD_H__
#define __NBT_LCD_H__

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

extern void putch(unsigned char c);

void LCD_GotoXY(unsigned char X, unsigned char Y);
//------------------------------------------------

void _rom_LCD_WriteString(char __rom *s);
//----------------------------------

void LCD_WriteString(char *s);
//----------------------------------

//---------------------------------------------
// outputs byte as two digit hex string on LCD
//---------------------------------------------
void LCD_HexByte_Write(unsigned char x);

//------------------------------------------------
// outputs integer as two digit hex string on LCD
//------------------------------------------------
void LCD_HexInt_Write(unsigned int i);
      
#endif // ifndef __NBT_LCD_H__

