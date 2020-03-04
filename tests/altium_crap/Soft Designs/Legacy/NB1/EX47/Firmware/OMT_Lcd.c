#include "hware.h"
#include "OMT_Lcd.h"

void Delay10Us(unsigned char time)
{
  unsigned long i;                                         // this is very imprecise but only used for
  for(i=0;i<(FOSC/180000L);i++)                            // non-critical delays
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
  while(LCD_CTRL_R & 0x80);                                // wait for busy flag to be low
#endif

}

//----------------------------------------------------------------
// initialises the LCD controller
void LCD_Init(void)
{
  RAM_LCD = SELECT_LCD;
  DelayMs(20);                                             // wait for more than 15ms after powerup
  LCD_CTRL_W = 0x38;                                       // Function set 8 Bit
  DelayMs(8);                                              // wait for another 8 ms
  LCD_CTRL_W = 0x38;                                       // Function set 8 Bit
  DelayMs(2);                                              // wait for another 2 ms
  LCD_CTRL_W = 0x38;                                       // Function set 8 Bit
  Delay10Us(10);
  LCD_CTRL_W = 0x38;                                       // Function set 8 Bit, 2 lines, 5X7 dots
  Delay10Us(10);
  LCD_CTRL_W = 0x06;                                       // Display Off, Cursor off, Blink off
  Delay10Us(10);
  LCD_CTRL_W = 0x0E;                                       // Display on, Cursor On
  LCD_WaitWhileBusy(1000);
  LCD_CTRL_W = 0x01;                                       // Clear Display
  LCD_WaitWhileBusy(17*100);                               // This takes at least 1.64ms to execute
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
  RAM_LCD = SELECT_LCD;
  LCD_DATA_W = c;
  LCD_WaitWhileBusy(4);
  RAM_LCD = SELECT_RAM;
}

void _rom_LCD_WriteString(char __rom *s)
//----------------------------------
{
  while(0!=*s)
  {
    LCD_WriteChar(*s++);
  }
}

void LCD_WriteString(char *s)
//----------------------------------
{
  while(0!=*s)
  {
    LCD_WriteChar(*s++);
  }
}

//---------------------------------------------
// outputs byte as two digit hex string on LCD
//---------------------------------------------
void LCD_HexByte_Write(unsigned char x)
{
  unsigned char temp;
  temp = x>>4;
  temp +='0';
  if (temp >'9') temp += ('A' - '9' - 1);
  LCD_WriteChar(temp);
  temp = x & 0x0F;
  temp +='0';
  if (temp >'9') temp += ('A' - '9' - 1);
  LCD_WriteChar(temp);
}

//------------------------------------------------
// outputs integer as two digit hex string on LCD
//------------------------------------------------
void LCD_HexInt_Write(unsigned int i)
{
  LCD_HexByte_Write(i>>8);
  LCD_HexByte_Write(i&0xFF);
}


