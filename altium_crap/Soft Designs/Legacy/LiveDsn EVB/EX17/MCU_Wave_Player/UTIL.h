#ifndef __UTIL_H__
#define __UTIL_H__

//-------------------------------------------------------
// writes to 32bit wide SFR register
// write is done in 5 steps:
// writes 1..4 set up the data value, write 5 sets up the register
// number and latches it into the output latch on the falling
// edge of the write pulse
//-------------------------------------------------------
void Write32BitReg(unsigned char RegNumber, unsigned long value);

//-------------------------------------------------------------------
// sets up 32bit sample rate divider based on Wav file sample rate
//-------------------------------------------------------------------
void SetSampleRateDivider(unsigned long SamplesPerSecond);

//------------------------------------------------------------------
// compares 'count' bytes from two character pointer positions:
// r points to a rom area, p points to a ram area
// returns: 0 if they are identical
//          1 if they are not
//------------------------------------------------------------------
unsigned char romstrcmp(char __rom *r, unsigned char *p, unsigned char count);




#endif
