//    _  _  ___  _____   ___    _    ___   ___  ___    _    ___  _  _
//   | \| || _ )|_   _| | _ )  /_\  | _ \ / __|| _ \  /_\  | _ \| || |
//   | .` || _ \  | |   | _ \ / _ \ |   /| (_ ||   / / _ \ |  _/| __ |
//   |_|\_||___/  |_|___|___//_/ \_\|_|_\ \___||_|_\/_/ \_\|_|  |_||_|
//                  |___|
// (c) 2004 Altium
// Started: 27.01.2004 Ch.W.
// LCD Bargraph driver Routines for NanoBoard Tester

#include "nbt_bargraph.h"
#include "nbt_lcd.h"

//-------------------------------------------------------------------------------
// draws bargraph on LCD screen
// Parameters:
//   Value:     current bargraph value
//   MaxValue:  Value for 'MaxLength'
//   x,y  :     origin for graph (in LCD characters)
//   MaxLength: Maximum Length of Graph in increments per LCD pixel (default 3)
//-------------------------------------------------------------------------------
void LCD_BarGraph( unsigned int  Value,     unsigned int  MaxValue,
                   unsigned char x,         unsigned char y,
                   unsigned char MaxLength, unsigned char direction)
{
  unsigned char i;
  unsigned char c;
  unsigned char Pixels;
  LCD_GotoXY(x,y);
  Pixels = (Value * MaxLength) / MaxValue;
  for(i=0;i<(MaxLength/BG_RES_PERDIGIT);i++)
  {
    if(Pixels ==  0)     // end of bargraph ?
    {
      c = ' ';           // fill with blanks
    }
    else
    {
      if(Pixels >BG_RES_PERDIGIT - 1)   // calculate special character in CG-RAM position
      {
        c = BG_RES_PERDIGIT - 1 ;
      }
      else
      {
        c = Pixels-1;
      }
    }
    if(Pixels>(BG_RES_PERDIGIT - 1))  // count down length of bargraph
      Pixels -= BG_RES_PERDIGIT;
    else
      Pixels = 0;
    if(!direction)                    // add offset for left pointing graph
    {
      LCD_GotoXY(--x,y);
      if(' ' != c) c += BG_RES_PERDIGIT;
    }
    LCD_WriteChar(c);
  }
}

//-------------------------------------------------------------------------------
// draws bidirectional bargraph on LCD screen
// Parameters:
//   Value:     current bargraph value
//   MaxValue:  Value for 'MaxLength'
//   x,y  :     origin for graph (in LCD characters)
//   MaxLength: Maximum Length of Graph in increments per LCD pixel (default 3)
//-------------------------------------------------------------------------------
void LCD_BarGraph_Bi( signed int  Value,       unsigned int  MaxValue,
                      unsigned char x,         unsigned char y,
                      unsigned char MaxLength)
{
  unsigned char i;
  unsigned char pc= 0, mc= 0;
  unsigned char Pixels;
  unsigned int AbsValue;
  unsigned char Neg;
  if(Value <0)
  {
    AbsValue = -Value;
    Neg = 1;
  }
  else
  {
    AbsValue = Value;
    Neg = 0;
  }
  Pixels = (AbsValue * MaxLength) / MaxValue;
  for(i=0;i<(1+(MaxLength)/BG_RES_PERDIGIT);i++)
  {
    if(0==i)
    {
      if(0==Pixels)   // bar length = 0
      {
        pc = mc =  6; // centre only
      }
      else
      {
        Pixels -= 1;
        if(Neg)
          pc = mc = 1;  // left and centre
        else
          pc = mc = 4;  // right and centre
      }
    }
    else
    {
      if(Pixels)
      {
        pc = BG_RES_PERDIGIT - 1 ;
        mc = pc + BG_RES_PERDIGIT;
        if(Pixels >BG_RES_PERDIGIT - 1)   // calculate special character in CG-RAM position
        {
          pc = BG_RES_PERDIGIT - 1 ;
          Pixels -=  BG_RES_PERDIGIT;
        }
        else
        {
          pc = Pixels-1;
          Pixels = 0;
        }
        mc = pc + BG_RES_PERDIGIT;
      }
      else
      {
        pc = ' ';
        mc = ' ';
      }
      if(Neg)
      {
         pc = ' ';
      }
      else
      {
         mc = ' ';
      }
    }
    LCD_GotoXY(x+i,y);
    LCD_WriteChar(pc);
    LCD_GotoXY(x-i,y);
    LCD_WriteChar(mc);
  }
}


/*

// for 5 ticks per character
void GenerateBarGraphCustomCharacters(void)
{
   LCD_SetCustomChar(0, "\x10\x10\x10\x10\x10\x10\x10\x10");
   LCD_SetCustomChar(1, "\x18\x18\x18\x18\x18\x18\x18\x18");
   LCD_SetCustomChar(2, "\x1C\x1C\x1C\x1C\x1C\x1C\x1C\x1C");
   LCD_SetCustomChar(3, "\x1E\x1E\x1E\x1E\x1E\x1E\x1E\x1E");
   LCD_SetCustomChar(4, "\x1F\x1F\x1F\x1F\x1F\x1F\x1F\x1F");
   LCD_SetCustomChar(5, "\x10\x18\x1C\x1E\x1C\x18\x10\x00");
   LCD_SetCustomChar(6, "\x18\x1C\x1E\x1F\x1E\x1C\x18\x00");
   LCD_SetCustomChar(7, "\xFF\x11\x11\x11\x11\x11\x1F\x00");
}

*/
///*
// for three ticks per character
void GenerateBarGraphCustomCharacters(void)
{
   LCD_SetCustomChar(0, "\x10\x10\x10\x10\x10\x10\x10\x10");    // left only
   LCD_SetCustomChar(1, "\x14\x14\x14\x14\x14\x14\x14\x14");    // left and centre
   LCD_SetCustomChar(2, "\x15\x15\x15\x15\x15\x15\x15\x15");    // all three lines
   LCD_SetCustomChar(3, "\x01\x01\x01\x01\x01\x01\x01\x01");    // right only
   LCD_SetCustomChar(4, "\x05\x05\x05\x05\x05\x05\x05\x05");    // right and centre
   LCD_SetCustomChar(5, "\x15\x15\x15\x15\x15\x15\x15\x15");    // all three lines
   LCD_SetCustomChar(6, "\x04\x04\x04\x04\x04\x04\x04\x04");    // centre line
   LCD_SetCustomChar(7, "\xFF\x11\x11\x11\x11\x11\x11\xFF");    // empty Box
}

//*/



