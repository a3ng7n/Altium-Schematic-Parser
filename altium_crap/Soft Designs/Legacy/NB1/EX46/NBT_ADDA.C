//    _   _  ____  _____        _     ____   ____     _        ____
//   | \ | || __ )|_   _|      / \   |  _ \ |  _ \   / \      / ___|
//   |  \| ||  _ \  | |       / _ \  | | | || | | | / _ \    | |
//   | |\  || |_) | | |      / ___ \ | |_| || |_| |/ ___ \  _| |___
//   |_| \_||____/  |_|_____/_/   \_\|____/ |____//_/   \_\(_)\____|
//                    |_____|
//
// (c) 2003 Altium
// Started: 17.11.2003 Ch.W.
// MAX1037 D/A and MAX 5841 A/D Routines
// the Altium Nanoboard Tester


#include "nbt_i2c.h"
#include "nbt_adda.h"
#include "hware.h"

//...............................................................................
// Command Sequence to write a command and 10 Bit data is:
//   1. Start Bit
//   2. First Byte  - A6 A5 A4 A3 A2 A1 A0 R/W      Write => 0111 1010     Read => 0111 1011
//   3. Check For ACK
//   4. Second Byte - C3 C2 C1 C0 D9 D8 D7 D6       See 5841 commands above
//   5. Check For ACK
//   6. Third Byte  - D5 D4 D3 D2 D1 D0 S1 S0       S1 & S0 are always zero
//   7. Check For ACK
//...............................................................................


//...............................................................................
//          ValueHi                     ValueLo
//  -------------------------   -------------------------
//  |XX|XX|XX|XX|XX|XX|D9|D8|   |D7|D6|D5|D4|D3|D2|D1|D0|
//
//  Must be translated to =>
//
//     CommandByte1                 CommandByte2
//  -------------------------   -------------------------
//  |C3|C2|C1|C0|D9|D8|D7|D6|  |D5|D4|D3|D2|D1|D0|S1|S0|
//
//  S0 -> Zero
//  S1 -> Zero
//  C3..C0 = Command
//...............................................................................


//------------------------------------------------------------------
// Shifts out the two bytes on the I2C bus to the DAC
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//------------------------------------------------------------------
static unsigned char DAC_Send_Bytes(unsigned char Byte1, unsigned char Byte2)
{
  I2C_Start();
  if(NACK == I2C_TxByte(Max5841_Addr<<1)) return NACK;
  if(NACK == I2C_TxByte(Byte1)) return NACK;
  if(NACK == I2C_TxByte(Byte2)) return NACK;
  I2C_Stop();
  return(ACK);
}

//----------------------------------------------------------------
// Initialises the DAC to power up all four channels
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------
unsigned char DAC_Init(void)
{
  I2C_Init();
  return DAC_Send_Bytes(Max5841_ExtendedCommandMode, 0x3C); // Power up all four DACs
}

//----------------------------------------------------------------------
// Outputs 'DAC_Value' to DAC_Cannel [0..3]
// Performs all bis mangling and calls DAC_Send_Bytes to shift them out
// Output Voltage is (Vref * 'DAC_Value')/2048
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------------
unsigned char DAC_OutData(unsigned char DAC_Channel, unsigned int DAC_Value)
{
  register unsigned char byte1, byte2;
  DAC_Channel &= 0x03;   // only 0..3 are valid DAC numbers
  DAC_Value  &= 0x3FF;  // 10 Bits only for DAC data
  byte2 = DAC_Value & 0x00FF;  // shift bits into the right positions
  byte2 <<= 2;
  byte1 = DAC_Channel << 4;
  byte1 |= (DAC_Value >> 6)& 0x0F;
  return DAC_Send_Bytes(byte1, byte2);  // and send them out
}


//----------------------------------------------------------------
// Initialises the ADC: 
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------
unsigned char ADC_Init(void)
{
  I2C_Init();
  I2C_Start();
  if(NACK == I2C_TxByte(Max1037_Addr<<1)) return NACK;
  if(NACK == I2C_TxByte(Max1037_Default_Setup)) return NACK;
  I2C_Stop();
  return(ACK);
}

//----------------------------------------------------------------
// Sets ADC up for single ended inputs and selects input 'Channel' [0..3]
// returns : ACK for success
//         : NACK for missing ACK from DAC chip
//----------------------------------------------------------------
unsigned char ADC_Config(unsigned char Channel)
{
  register unsigned char ConfigByte;
  Channel &= 0x03;   // We only have four channels
  Channel <<= 1;     // shift into correct bit positions
  ConfigByte = Max1037_Default_Config;
  ConfigByte |= Channel;
  I2C_Start();
  if(NACK == I2C_TxByte(Max1037_Addr<<1)) return NACK;
  if(NACK == I2C_TxByte(Max1037_Default_Setup)) return NACK;
  if(NACK == I2C_TxByte(ConfigByte)) return NACK;
  I2C_Stop();
  return(ACK);
}


//----------------------------------------------------------------
// Performs single conversion on selects channel and returns value
// ADC is only 8 bits wide, so results are returns << 2
// bottom two bits are always 0
// if result if greater than 0x3FC an error has occurred:
//     ADC_NO_ACK        (0xFFFF)  -> No I2C Ack received
//     ADC_READ_TIMEOUT  (0xFFFE)  -> timeout on SCL release
//----------------------------------------------------------------
unsigned int ADC_Read(void)
{
  register unsigned int retval;
  register unsigned char OutByte;
  unsigned int SCL_WaitHighCounter = Max1037_MAX_SCL_WAIT;
  I2C_Start();  // send start condition
  OutByte = Max1037_Addr;
  OutByte <<= 1;
  OutByte |= I2C_R;
//  if(NACK == I2C_TxByte(Max1037_Addr<<1) | I2C_R) return ADC_NO_ACK;  // send out ADC's I2C Address for read
  if(NACK == I2C_TxByte(OutByte)) return ADC_NO_ACK;  // send out ADC's I2C Address for read  
  SCL_H;        // release SCL line
  while(0 == SCL)       // wait for SCL to be released by ADC before reading
  {
    if(0 == SCL_WaitHighCounter--) 
      return (ADC_READ_TIMEOUT);
  }
  retval =  I2C_RxByte(0);  // read byte 1 and generate Ack
  retval <<= 2;             // scale it up to 10 bits
  I2C_Stop();
  return retval;
}

//-----------------------------------------------------------------
// Assumes that ADC[0..4] is looped to DAC[0..4]
// Assumes Reference voltage is set to 3V3
// Outputs test voltages on each DS/A channel sucessively and checks that
// the corresponding A/D Channel reads back the same value within
// the tolerance +-'Tolerance' bits;
// Test voltages are stored in a 0-terminated array of integers
// 1 bit = 2mV
// All other Channels are checked to be within 'Tolerance' of 0;
// Returns: 0    : Success
//          Non-0: Bitpattern of offending channels
//                 (eg Bit2 set -> faulty Channel 2)
//---------------------------------------------------------------------------
unsigned char DAC_ADC_Test(unsigned int Tolerance, unsigned int * PTestValues)
{
  const unsigned char MaxChannel = 4;
  signed int TestValue = 0x1FF;
  signed int InValue;
  unsigned char retval = 0;   // return value
  unsigned char OutChannel, InChannel;
  for(OutChannel = 0; OutChannel < MaxChannel; OutChannel ++)  // set all D/A channels to 0
     DAC_OutData(OutChannel,0);
  while(*PTestValues)
  {      
    TestValue = *PTestValues++;   // read next TestValue
    TestValue &= 0x3FF;           // we only have 10 bits to play with
    for(OutChannel = 0; OutChannel < MaxChannel; OutChannel ++)
    {
       unsigned int ui;
       DAC_OutData(OutChannel,TestValue);            // output testvalue on current channel
       for(InChannel = 0; InChannel < MaxChannel; InChannel++)
       {
         ADC_Config(InChannel);
         ui = ADC_Read();
         if(InChannel == OutChannel)  // check for voltage present
         {
           InValue = TestValue - ui;
           if(InValue < 0)
             InValue = -InValue;
           if(InValue > Tolerance)
             retval |= 1 << InChannel;  // set error flag  
         }
         else                         // check for voltage absent
         {
            if(ui > Tolerance)
             retval |= 1 << InChannel;  // set error flag
         }
       }  // for (InChannel)
    DAC_OutData(OutChannel,0);   // reset output value to 0 for current channel
    } // for (OutChannel)
  } // while
  return retval;
}

