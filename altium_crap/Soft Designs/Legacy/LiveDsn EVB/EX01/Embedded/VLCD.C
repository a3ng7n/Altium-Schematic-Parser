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

#include "hware.h"
#include "vlcd.h"

#define MaxX 16
#define MaxY 2

#define CLEAR_CHAR ' '

static unsigned char CurrentAddress=0;
static unsigned char CurrentBacklight=0;

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


//----------------------------------------------------------------
// initialises the LCD controller
void LCD_Init(void)
{
    CurrentAddress = 0;
    LCD_ClrScr();
}

void LCD_SetBacklight(unsigned char On)
//-------------------------------------
// turns LCD Backlight on or off
{
  if(On)
    CurrentBacklight = VLCD_MASK_BL;
  else
    CurrentBacklight = 0;
}

unsigned char LCD_GetBacklight(void)
//--------------------------------------
// returns state of LCD backlight
{
  return CurrentBacklight ? 1 : 0;
}

//------------------------------------------
// Clears the screen
void LCD_ClrScr(void)
{
  unsigned char i;
  LCD_GotoXY(0,0);
  LCD_GotoXY(0,0);
  for(i=0;i<(MaxX * MaxY);i++)
  {
//    DelayMs(200);
    LCD_WriteChar(CLEAR_CHAR);
  }
  LCD_GotoXY(0,0);
}


void LCD_GotoXY(unsigned char X, unsigned char Y)
//------------------------------------------------
{
  CurrentAddress = X + (Y*MaxX);
}

void LCD_WriteChar(unsigned char c)
//-------------------------------------------------
{
  unsigned char temp;
  VLCDDATA_PORT = c;
  temp = CurrentBacklight;
  temp |= CurrentAddress;
  temp |= VLCD_MASK_WR ;
  VLCDCTRL_PORT = temp;
  __asm( "NOP\n\t");
  temp &= ~VLCD_MASK_WR;
  VLCDCTRL_PORT = temp;
  CurrentAddress++;
  if(CurrentAddress > (MaxX * MaxY))
    CurrentAddress = 0;
}


