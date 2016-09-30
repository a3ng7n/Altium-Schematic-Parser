//    _       ______  _____  ______
//   | |     |  ____||  __ \|____  |
//   | |     | |__   | |  | |   / /
//   | |     |  __|  | |  | |  / /
//   | |____ | |____ | |__| | / /
//   |______||______||_____/ /_/
//
// (c) 2004 Altium
// Started: 24.08.2004 Ch.W.
// 7 Segment LED driver for NEB

#include "hware.h"
#include "led7.h"


#define MaxDigit 5

// lookup table for 7 segment digit patterns 0..F
const __rom char Seg_Pattern[]={0x3F,0x06,0x5B,0x4F,0x66,0x6D,0x7D,0x07,0x7F,0x6F,0x77,0x7C,0x39,0x5E,0x79,0x71,0x00};

static unsigned char CurrentDigit = 0;

//------------------------------------------
// Sets cursor position
//------------------------------------------
void Seg7_SetCurrentPosition(unsigned char NewPos)
{
    CurrentDigit = NewPos;
    if(CurrentDigit > MaxDigit) CurrentDigit = 0;
}

//------------------------------------------
// Gets cursor position
//------------------------------------------
unsigned char Seg7_GetCurrentPosition(void)
{
    return CurrentDigit;
}


//-------------------------------------------------------------------------
// writes current digit to digit pattern and advances cursor to next digit
//-------------------------------------------------------------------------
void Seg7_WriteDigitP(unsigned char Pattern)
{
   (*(__bsfr volatile unsigned char *) (Seg7_START_ADDR + (8*CurrentDigit)) ) = Pattern;
   if(++CurrentDigit > MaxDigit) CurrentDigit = 0;
}

//-------------------------------------------------------------------------
// writes current digit to decimal digit and advances cursor to next digit
// if the current character is '.' the decimal point is set on the previous
// digit
//-------------------------------------------------------------------------
void Seg7_WriteDigitN(unsigned char c)
{
  unsigned char pattern=0;
  switch(c)
  {
    case ' ':   // clear
      pattern = 0;
    break;
    case '.':   // TODO: implement decimal point
    break;
    default:
    if((c>='0') && (c<='9'))
      pattern = c-'0';
    else
      if((c>='A') && (c<='F'))
        pattern = c-('A'-10);
      else
        pattern = 0x10; // catch all: blank
  }
  pattern = Seg_Pattern[pattern];
  Seg7_WriteDigitP(pattern);  // this handles cursor advance as well
}

//---------------------------------------------------------------------
// Sets all segments to values starting at 'Pattern'
// Segment A = Bit0 ... Segment G = Bit6, Decimal Point is Bit 7
//---------------------------------------------------------------------
void Seg7_Set(char *Pattern)
{
    unsigned char SfrAddr = Seg7_START_ADDR;   // initialise to address of digit A
    unsigned char i;
    for(i=0;i<6;i++)
    {
      (*(__bsfr volatile unsigned char *) SfrAddr ) = *Pattern++;
      SfrAddr +=8;
    }
}

//------------------------------------------------------------------------
// Sets All digits to bitpattern 'DigitValue'
// Segment A = Bit0 ... Segment G = Bit6, Decimal Point is Bit 7
//---------------------------------------------------------------------
void Seg7_SetAll(unsigned char DigitValue)
{
    unsigned char i;
    char Pattern[6];
    for(i=0;i<6;i++)
    Pattern[i]=DigitValue;
    Seg7_Set(Pattern);
}


