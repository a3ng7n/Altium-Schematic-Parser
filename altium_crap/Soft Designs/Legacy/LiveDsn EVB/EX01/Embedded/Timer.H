//-----------------------------------------------------------------
//      _____  ___  __  __  _____  ____
//     |_   _||_ _||  \/  || ____||  _ \
//       | |   | | | |\/| ||  _|  | |_) |
//       | |   | | | |  | || |___ |  _ <
//       |_|  |___||_|  |_||_____||_| \_\
//
//
// (c) 2003 Altium
// Started: 18.11.2003 Ch.W.
// Timer related stuff
//-----------------------------------------------------------------

#ifndef __TIMER_H__
#define __TIMER_H__


#include "hware.h"

#define T0_INT_FREQ (((FOSC / 12.0) / 0x1FFF)+.5)    // ~203.47Hz@20MHz
                                                     // so maximum time is 600ms @20MHz s

#define TIMER_SECONDS(x) ((unsigned int) ((x) * (unsigned long)FOSC / (12 * 0x1FFFL)))

enum {TIMER_0,TIMER_1,TIMER_LCD, TIMER_BEEP, TIMER_NO_OF};  // These get counted down in the timer0_ISR

extern __idata volatile unsigned char Timer[TIMER_NO_OF];

/*-----------------------------------------------------
Init timer0 in Mode 0 with maximum prescaler
------------------------------------------------------*/
void Timer0Init(void);


/*------------------------------------------------
Timer 0 Interrupt Service Routine.
------------------------------------------------*/
__interrupt(INTVEC_T0) void timer0_ISR (void);


extern volatile unsigned char KbHit;  // true if key was pressed

//-------------------------------------
// returns last keyboard pattern
//-------------------------------------
unsigned char GetKey(void);

//-------------------------------------------------------------------------------
// SOUND related Stuff
//-------------------------------------------------------------------------------

#define END_MARKER     0x00
#define STOP_MARKER    0x00
#define REPEAT_MARKER  0x01

//  each each entry consists of note duration and note number
//  valid note numbers are 0..95 0 is A(octave 0 27.5Hz) 95 is F#(octave7 6644.875Hz)
//  Note number N = (12/ln(2))*ln(f/27.5)
//  frequency   f = 27.5*2^(N/12)
//  Therefore middle C is N=51

typedef
struct
{
    unsigned char Duration;
    unsigned char NoteNumber;
} NoteType;

extern __rom NoteType Silence[];  // for turning background melody off

//--------------------------------------------------------------
// sets background melody
//--------------------------------------------------------------
void SetMelody(__rom NoteType *Left ,__rom NoteType *Right);


#endif // __TIMER_H__

