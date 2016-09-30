//     _      ____  ____
//    | |    / ___||  _ \
//    | |   | |    | | | |
//    | |___| |___ | |_| |
//    |_____|\____||____/
//
// (c) 2003 Altium
// Started: 20.11.2003 Ch.Weimann
// LCD driver Routines for NanoBoard Tester
// 23.01.04 fixed bug in LCD_GotoXY that did not work properly for line 2


#include "hware.h"
#include "lcd.h"

void Delay10Us(unsigned char time)
{
  unsigned long i;                      // this is very imprecise but only used for
  for(i=0;i<(FOSC/180000L);i++)         // non-critical delays
  {
     __asm( "NOP\n\t");
  }
}

void DelayMs(unsigned int time)
{
  register unsigned char i;
  while (time --)
    for(i=0; i<5; i++)
      Delay10Us(20);
}

static void LCD_WaitWhileBusy(unsigned int Del10Us)
{
#ifndef LCD_READ_BUSY_FLAG  
  if(Del10Us > 200)
    DelayMs(Del10Us/100);
  else
    Delay10Us(Del10Us);
#else
  while(LCD_CTRL_R & 0x80);       // wait for busy flag to be low
#endif

}

//----------------------------------------------------------------
// initialises the LCD controller
void LCD_Init(void)
{
  RAM_LCD = SELECT_LCD;
  DelayMs(20);  // wait for more than 15ms after powerup
  LCD_CTRL_W = 0x38;  // Function set 8 Bit
  DelayMs(8);        // wait for another 8 ms
  LCD_CTRL_W = 0x38;  // Function set 8 Bit
  DelayMs(2);   // wait for another 2 ms
  LCD_CTRL_W = 0x38;  // Function set 8 Bit
  Delay10Us(10);
  LCD_CTRL_W = 0x38;  // Function set 8 Bit, 2 lines, 5X7 dots
  Delay10Us(10);
  LCD_CTRL_W = 0x06;  // Display Off, Cursor off, Blink off
  Delay10Us(10);
  LCD_CTRL_W = 0x0E;   // Display on, Cursor On
  LCD_WaitWhileBusy(1000);
  LCD_CTRL_W = 0x01;   // Clear Display
  LCD_WaitWhileBusy(17*100);       // This takes at least 1.64ms to execute
  RAM_LCD = SELECT_RAM;
}

void LCD_SetBacklight(unsigned char On)
//-------------------------------------
// turns LCD Backlight on or off
{
  LCD_BACKLIGHT = On;
}

unsigned char LCD_GetBacklight(void)
//--------------------------------------
// returns state of LCD backlight
{
  return LCD_BACKLIGHT;
}

//------------------------------------------
// Clears the screen
void LCD_ClrScr(void)
{
  RAM_LCD = SELECT_LCD;
  LCD_CTRL_W = 0x01;
  LCD_WaitWhileBusy(300);
  RAM_LCD = SELECT_RAM;
}


void LCD_GotoXY(unsigned char X, unsigned char Y)
//------------------------------------------------
{
  RAM_LCD = SELECT_LCD;
  LCD_CTRL_W = 0x80 | (X + 0x40* Y);
  LCD_WaitWhileBusy(4*100);
  RAM_LCD = SELECT_RAM;
}

void LCD_WriteChar(unsigned char c)
//-------------------------------------------------
{
//  LCD_DATA_W = 0x55;
  RAM_LCD = SELECT_LCD;
  LCD_DATA_W = c;
  LCD_WaitWhileBusy(4);
  RAM_LCD = SELECT_RAM;
}

