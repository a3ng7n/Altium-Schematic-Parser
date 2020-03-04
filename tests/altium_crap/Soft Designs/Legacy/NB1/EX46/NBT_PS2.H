//-------------------------------------------------------------------------
//
// (c) 2003 Altium
// Started: 04.12.2003 Ch.Weimann
// PS2 Keyboard Interface for
// the Altium Nanoboard Tester
//-------------------------------------------------------------------------
#ifndef  __NBT_PS2_H__
#define  __NBT_PS2_H__
        
//----------------------------------------------------------------
// Initialises clock and data lines to default states : all high
//----------------------------------------------------------------
void PS2_Init(void);

//-------------------------------------------------------------------------
// Transmits byte data to PS2 Port A
// port :  0: PS2 port A
//         1: PS2 port B
// returns 0    : success
//         non-0: timeout
//-------------------------------------------------------------------------
unsigned char PS2_TxByte(unsigned char port, unsigned char data);

//-----------------------------------------------------------------------
// Sends Set Led Command (0xED) to Keyboard with the bitpattern 'pattern'
// Bit set = LED on
// Bit 0: Scroll Lock
// Bit 1: Caps Lock
// Bit 2: Num Lock
// returns 0: success
//         non-0: error code 
//-----------------------------------------------------------------------
unsigned char PS2_SetLEDs(unsigned char port, unsigned char pattern);


extern unsigned int PS2_LastChar;  // last scancode that was received from PS2 Keyboard
extern __bit PS2_KeyPressed;       // gets set whenever valid scan code is received


#endif   // __NBT_PS2_H__










