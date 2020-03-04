//-------------------------------------------------------------------------
//       _____    _____  ___
//      |  __ \  / ____||__ \
//      | |__) || (___     ) |
//      |  ___/  \___ \   / /
//      | |      ____) | / /_
//      |_|     |_____/ |____|
//
//
// (c) 2003 Altium
// Started: 04.12.2003 Ch.W.
// PS2 Keyboard Interface
//-------------------------------------------------------------------------
#include "lcd.h"     // for delay
#include "ps2.h"
#include "hware.h"
#define PS2_TIMEOUT  0x0FFF

//---------------------------------------------------------------------------------------------

enum {PS2KB_IDLE,PS2KB_CLKDATA,PS2KB_WAITSTOP,PS2KB_WAITPARITY};

static unsigned char PS2KB_state;
unsigned int PS2_LastChar = 0;
__bit PS2_KeyPressed = 0;

// ------------------------------------------------------
// External Interrupt 0 Interrupt Service Routine
// triggered by PS2 Keyboard clock
// reads keyboard scancodes
// keyboard will send one-byte make code on keypress
// two-byte break code on key release
// ------------------------------------------------------
__interrupt (INTVEC_EXT0) void ExtInt0_ISR(void)
{
  static __bit parity = 0;
  __bit inbit;
  static unsigned char bitcount;
  static unsigned char PS2InChar;
  inbit = PS2_DTA_RD;    // read bit from port first before it gets changed
  switch (PS2KB_state)//  do nothing
  {
    case PS2KB_IDLE:
      if(0==inbit)    // valid start bit?
      {
        bitcount = 0; // initialise bit count
        parity = 0;   // initialise parity bit
        PS2KB_state = PS2KB_CLKDATA;
      }
    break;
    case PS2KB_CLKDATA:
      PS2InChar >>=1;
      if(inbit == 1)          // shift data is sent lsb first
      {
        parity ^= 1;
        PS2InChar|=0x80;
      }
      bitcount++;
      if (bitcount >= 8)      // last data bit received?
      {
        PS2KB_state = PS2KB_WAITPARITY;
      }
    break;
    case PS2KB_WAITPARITY:
      parity ^= inbit;  // check parity, should be odd
      PS2KB_state = PS2KB_WAITSTOP;  // TODO: check parity
    break;
    case PS2KB_WAITSTOP:
      if((inbit == 1) && (parity == 1))
      {
        LED_PORT = 0xFF;
        PS2_LastChar = PS2InChar;  // update last character
        PS2_KeyPressed = 1;        // set flag to indicate new character
      }
      PS2KB_state = PS2KB_IDLE;
    break;
  } // switch
  LED_PORT = 0x00;
}

//------------------------------------------------------------------------------------------





//----------------------------------------------------------------
// Initialises clock and data lines to default states : all high
//----------------------------------------------------------------
void PS2_Init(void)
{
  PS2_DTA_WR = 1;  // release data line
  PS2_CLK_WR = 1;  // release clock line
  PS2_DTA_WR = 1;  // same for other port
  PS2_CLK_WR = 1;  //
}

// returns 0 if success
//         1 if timeout
unsigned char PS2_WaitForClockHigh(unsigned int timeout)
{
  while(0 == PS2_CLK_RD)
  {
    if(0==timeout--)
      return 1;   // timeout
//    Delay10Us(1); // check again in 10us 
  }
  return 0;
}

// returns 0 if success
//         1 if timeout
unsigned char PS2_WaitForClockLow(unsigned int timeout)
{
  while(1 == PS2_CLK_RD)
  {
    if(0==timeout--)
      return 1;   // timeout
//    Delay10Us(1); // check again in 10us 
  }
  return 0;
}

// returns 0 if success
//         1 if timeout
unsigned char PS2_WaitForDataHigh(unsigned int timeout)
{
  while(0 == PS2_DTA_RD)
  {
    if(0==timeout--)
      return 1;   // timeout
//    Delay10Us(1); // check again in 10us 
  }
  return 0;
}

// waits 'timeout*10)us for clock line to go low
// returns 0 if success
//         1 if timeout
unsigned char PS2_WaitForDataLow(unsigned int timeout)
{
  while(1 == PS2_DTA_RD)
  {
    if(0==timeout--)
      return 1;   // timeout
//    Delay10Us(1); // check again in 10us
  }
  return 0;
}

//-------------------------------------------------------------------------
// Transmits byte data to PS2 Port A
// returns 0    : success
//         non-0: timeout
//-------------------------------------------------------------------------
unsigned char PS2_TxByte(unsigned char port, unsigned char data)
{
  __bit parity = 1;
  register unsigned char timeout;
  register unsigned char i; // bit loop counter
  PS2_SEL = port;   // select correct multiplexer position

//################ debug stuff on
/*
  PS2_CLK_WR = 0x0;
  PS2_DTA_WR = 0x0;
  PS2_CLK_WR = 0x01;
  PS2_DTA_WR = 0x01;
  PS2_CLK_WR = 0x0;
  PS2_DTA_WR = 0x0;
  PS2_CLK_WR = 0x01;
  PS2_DTA_WR = 0x01;
*/

//################ debug stuff off


  PS2_CLK_WR = 0;
  Delay10Us(50);    // Bring clock line low for at least 100us
  PS2_DTA_WR = 0;  // bring data line low
  PS2_CLK_WR = 1;  // release the clock line again
  timeout = 100;    // wait no longer than 1ms
  if(PS2_WaitForClockHigh(PS2_TIMEOUT)) // wait for clock line to go high
    return 1;                   // (might take a few cycles) for pullup resistor
  if(PS2_WaitForClockLow(PS2_TIMEOUT))  // wait for Keyboard to pull clock line low
    return 2;                   // probably indicates no keyboard present
  for(i=0;i<8;i++)
  {
    __bit TXbit;
    TXbit = data &0x01;    // send LSB 1st
    parity ^= TXbit;       // keep track of parity bit
    PS2_DTA_WR = TXbit;
    data >>= 1;                   // prepare for next bit
    if(PS2_WaitForClockHigh(PS2_TIMEOUT)) // wait for KBD to bring clock line high
      return 3;                   // (might take a few cycles) for pullup resistor
    if(PS2_WaitForClockLow(PS2_TIMEOUT))  // wait for Keyboard to pull clock line low
      return 4;                   // probably indicates no keyboard present
  }
  PS2_DTA_WR = parity;           // send parity bit
  if(PS2_WaitForClockHigh(PS2_TIMEOUT)) // wait for KBD to bring clock line high
    return 5;                   // (might take a few cycles) for pullup resistor
  if(PS2_WaitForClockLow(PS2_TIMEOUT))  // wait for Keyboard to pull clock line low
    return 6;                   // probably indicates no keyboard present
//  done
  PS2_DTA_WR = 1;                // release data line again
  if(PS2_WaitForDataLow(PS2_TIMEOUT))     // wait for Keyboard to pull data line low (Ack)
    return 7;                     // No Ack
  if(PS2_WaitForDataHigh(PS2_TIMEOUT))    // wait for Keyboard to release data line
    return 8;                     //
  if(PS2_WaitForClockHigh(PS2_TIMEOUT))    // wait for Keyboard to release clock line
    return 9;                     //
  return 0;
}

//-----------------------------------------------------------------------
// Sends Set Led Command (0xED) to Keyboard with the bitpattern 'pattern'
// Bit set = LED on
// Bit 0: Scroll Lock
// Bit 1: Caps Lock
// Bit 2: Num Lock
// returns 0: success
//         non-0: error code
//-----------------------------------------------------------------------
unsigned char PS2_SetLEDs(unsigned char port, unsigned char pattern)
{
  unsigned char i;
  __bit OldEA = EA;
  unsigned char retval = 0;
  unsigned char temp;
  temp = pattern & 0x01;            // Scroll lock is already in correct position
  if(pattern & 0x02) temp |= 0x04;  // move capslock bit to bit2
  if(pattern & 0x04) temp |= 0x02;   // move numlock bit to bit1
  EA = 0;
  retval |= PS2_TxByte(port, 0xED);
  PS2_KeyPressed = 0;          // reset Keyboard scan state machine
  PS2KB_state = PS2KB_IDLE;
  EA = OldEA;
  for(i=0;i<50;i++)   // wait for ACK from keyboard or timeout, whichever comes 1st
  {
    if(PS2_KeyPressed)
      if(PS2_LastChar == 0xFA)
        break;
    DelayMs(1);
  }
//  DelayMs(50);                    // wait for response to go away
  EA = 0;
  retval |= PS2_TxByte(port,temp);
  PS2_KeyPressed = 0;          // reset Keyboard scan state machine
  PS2KB_state = PS2KB_IDLE;
  EA = OldEA;
  for(i=0;i<50;i++)   // wait for ACK from keyboard or timeout, whichever comes 1st
  {
    if(PS2_KeyPressed)
      if(PS2_LastChar == 0xFA)
        break;
    DelayMs(1);
  }
  return retval;
}


