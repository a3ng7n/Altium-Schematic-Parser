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

#ifndef __LED7_H__
#define __LED7_H__

// lookup table for 7 segment digit patterns 0..F
extern const __rom char Seg_Pattern[];

//------------------------------------------
// Sets cursor position
//------------------------------------------
void Seg7_SetCurrentPosition(unsigned char NewPos);

//------------------------------------------
// Gets cursor position
//------------------------------------------
unsigned char Seg7_GetCurrentPosition(void);

//-------------------------------------------------------------------------
// writes current digit to decimal digit and advances cursor to next digit
// if the current character is '.' the decimal point is set on the previous
// digit
//-------------------------------------------------------------------------
void Seg7_WriteDigitN(unsigned char c);

//-------------------------------------------------------------------------
// writes current digit to digit pattern and advances cursor to next digit
//-------------------------------------------------------------------------
void Seg7_WriteDigitP(unsigned char Pattern);

//---------------------------------------------------------------------
// Sets all segments to values starting at 'Pattern'
// Segment A = Bit0 ... Segment G = Bit6, Decimal Point is Bit 7
//---------------------------------------------------------------------
void Seg7_Set(char *Pattern);

//------------------------------------------------------------------------
// Sets All digits to bitpattern 'DigitValue'
// Segment A = Bit0 ... Segment G = Bit6, Decimal Point is Bit 7
//---------------------------------------------------------------------
void Seg7_SetAll(unsigned char DigitValue);

#endif  // __LED7_H__






