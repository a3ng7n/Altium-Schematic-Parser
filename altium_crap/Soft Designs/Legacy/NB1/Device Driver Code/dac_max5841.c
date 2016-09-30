//..............................................................................
#include "dac_max5841.h"
#include "wb_i2c.h"
//..............................................................................

//..............................................................................
void max5841_send_bytes(unsigned int base, unsigned char byte1, unsigned char byte2)
{
  i2c_send_address(base,MAX5841_ADDRESS_WRITE);
  i2c_send_byte   (base,byte1,0);
  i2c_send_byte   (base,byte2,1);
}
//..............................................................................

//..............................................................................
// initialises the DAC to power up all four channels
//..............................................................................
void max5841_open(unsigned int base)
{
  i2c_init(base);
  max5841_send_bytes(base,MAX5841_EXTENDED_COMMAND_MODE,0x3C); // Power up all four DACs
}
//..............................................................................

//..............................................................................
// Performs all bis mangling and calls max5841_send_bytes to shift them out
// output Voltage is (Vref * 'max5841_Value')/2048
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//
//          ValueHi                     ValueLo
//  -------------------------   -------------------------
//  |XX|XX|XX|XX|XX|XX|D9|D8|   |D7|D6|D5|D4|D3|D2|D1|D0|
//
//  Must be translated to =>
//
//     Commandbyte1                 Commandbyte2
//  -------------------------   -------------------------
//  |C3|C2|C1|C0|D9|D8|D7|D6|  |D5|D4|D3|D2|D1|D0|S1|S0|
//
//  S0 -> Zero
//  S1 -> Zero
//  C3..C0 = Command
//...............................................................................
void max5841_set_output_channel(unsigned int base, unsigned char channel, unsigned int Value)
{
  static unsigned char byte1, byte2;

  channel = channel & 0x03;                // only 0..3 are valid channel numbers
  Value   = Value   & 0x3ff;               // 10 Bits only for DAC data
  byte2   = Value   & 0x00ff;              // shift bits into the right positions
  byte2   = byte2  << 2;
  byte1   = (channel << 4);
  byte1   = (byte1 | (Value >> 6) & 0x0f);

  max5841_send_bytes(base, byte1, byte2);  // and send them out
}
//..............................................................................

