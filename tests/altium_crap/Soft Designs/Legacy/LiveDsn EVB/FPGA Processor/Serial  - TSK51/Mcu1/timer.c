//-----------------------------------------------------------------
//      _____  ___  __  __  _____  ____
//     |_   _||_ _||  \/  || ____||  _ \
//       | |   | | | |\/| ||  _|  | |_) |
//       | |   | | | |  | || |___ |  _ <
//       |_|  |___||_|  |_||_____||_| \_\
//
//
// (c) 2003 Altium
// Started: 27.08.2004 Ch.W.
// 31.08.04 Ch.W. Added Interrupt driven music capability
// Timer related stuff for EvalBoard Tester
//-----------------------------------------------------------------

#include "hware.h"
#include "timer.h"

#define TRANSPOSE_SEMITONES 0

volatile unsigned char KbHit;
static volatile unsigned char LastKey;


static volatile unsigned int NoteIndexL = 0;
static volatile unsigned int NoteIndexR = 0;
static volatile unsigned int NoteDurationR = 0;
static volatile unsigned int NoteDurationL = 0;


__rom NoteType Silence[]={{1,0xFF},{END_MARKER, STOP_MARKER}};  // all sounds off
static __rom NoteType *SoundR = Silence;    // pointer to current melody for right speaker
static __rom NoteType *SoundL = Silence;    // pointer to current melody for left  speaker


//--------------------------------------------------------------
// sets background melody
//--------------------------------------------------------------
void SetMelody(__rom NoteType *Left ,__rom NoteType *Right)
{
    EA = 0;                         // make sure interrupts are off
    SoundL = Left;                  // set both pointers for left and right
    SoundR = Right;
    NoteDurationL = NoteDurationR = 0;  // reset note duration counters
    NoteIndexL = NoteIndexR = 0;    // start at beginning
    EA = 1;                         // restore interrupts
}

/*
#define T0_INT_FREQ (((FOSC / 12.0) / 0x1FFF)+.5)
enum {TIMER_0,TIMER_1,TIMER_NO_OF};  // These get counted down in the timer0_ISR
*/
__idata volatile unsigned char Timer[TIMER_NO_OF];

/*-----------------------------------------------------
Init timer0 in Mode 0 with maximum prescaler
------------------------------------------------------*/
void Timer0Init(void)
{
  /*-------------------------------------
  Set the Timer0 Run control bit.
  --------------------------------------*/
  TMOD = (TMOD & 0xF0) | 0x00;  /* Set T/C0 Mode 13 bit counter */
  TL0  = 0xFF;                  /* Set prescaler to maximum */
  ET0 = 1;                      /* Enable Timer 0 Interrupts */
  TR0 = 1;                      /* Start Timer 0 Running */
  SetMelody(Silence,Silence);
}

#define ScaleFactor 64

/*------------------------------------------------
Timer 0 Interrupt Service Routine.
------------------------------------------------*/
__interrupt(INTVEC_T0) void timer0_ISR (void)
{
  unsigned char i;
  if(NoteDurationR) NoteDurationR--;
  if(!NoteDurationR)   // play next note
  {
    NOTER_PORT = SoundR[NoteIndexR].NoteNumber + TRANSPOSE_SEMITONES;
    NoteDurationR = SoundR[NoteIndexR].Duration;
    NoteDurationR *=ScaleFactor;   // scale factor
    NoteIndexR++;
  }
  if(SoundR[NoteIndexR].Duration==END_MARKER)
  {
    if(SoundR[NoteIndexR].NoteNumber==REPEAT_MARKER) NoteIndexR = 0;
    else
    NoteIndexR--;
  }

  if(NoteDurationL) NoteDurationL--;
  if(!NoteDurationL)   // play next note
  {
    NOTEL_PORT = SoundL[NoteIndexL].NoteNumber + TRANSPOSE_SEMITONES;
    NoteDurationL = SoundL[NoteIndexL].Duration;
    NoteDurationL *=ScaleFactor;   // scale factor
    NoteIndexL++;
  }
  if(SoundL[NoteIndexL].Duration==END_MARKER)
  {
    if(SoundL[NoteIndexL].NoteNumber==REPEAT_MARKER) NoteIndexL = 0;
    else
    NoteIndexL--;
  }
  for(i=0;i < TIMER_NO_OF; i++)     // handle software timers
  {
    if(Timer[i]) Timer[i]--;
  }
  if(KEY_PORT != 0xFF)
  {
    LastKey = KEY_PORT;
    KbHit = 1;
  }
}

//-------------------------------------
// returns last keyboard pattern
//-------------------------------------
unsigned char GetKey(void)
{
    if(KEY_PORT ==0xFF)
      KbHit = 0;
    return(LastKey);
}



