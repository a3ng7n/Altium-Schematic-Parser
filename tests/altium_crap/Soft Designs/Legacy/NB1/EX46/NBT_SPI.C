//-------------------------------------------------------------
//        _   _  ____  _____     ____   ____  ___     ____
//       | \ | || __ )|_   _|   / ___| |  _ \|_ _|   / ___|
//       |  \| ||  _ \  | |     \___ \ | |_) || |   | |
//       | |\  || |_) | | |      ___) ||  __/ | |  _| |___
//       |_| \_||____/  |_|_____|____/ |_|   |___|(_)\____|
//                       |_____|
//
// (c) 2003 Altium
// Started: 04.12.2003 Ch.W.
// SPI driver Routines for NanoBoard Tester
// Adapted from Michael Rodway's Code
//-------------------------------------------------------------

#include "nbt_spi.h"
#include "hware.h"

#include "NBT_LCD.h"
#include "nbt_lcd.h"
#include "nbt_bargraph.h"
#include "nbt_kbd.h"
#include "strio.h"

#ifndef BYTE
 #define BYTE unsigned char
#endif

#ifndef BOOL
 #define BOOL unsigned char
#endif

#ifndef TRUE
 #define TRUE 1
#endif

#ifndef FALSE
 #define FALSE 0
#endif


#define RESET_INTERNAL 0
#define RESET_EXTERNAL 1

/*----------- Platform specific functions for TSK51 processor ------------*/

inline void SPI_ModePin(BOOL state)
{
   SPI_MODE = state;
}

inline void SPI_SelPin(BOOL state)
{
  SPI_SEL = state;
}

inline void SPI_ClkPinHigh(void)
{
  SPI_CLK = 1;
}

inline void SPI_ClkPinLow(void)
{
  SPI_CLK = 0;
}

inline void SPI_DoutPin(BOOL state)
{
  SPI_DOUT = state;
}

inline BOOL SPI_DinPin(void)
{
    return SPI_DIN;
};

/*
// Reset CPU with internal or external memory selected.
void Reset(BOOL useExternal)
{
    //Set desired
    if (useExternal)
       P0  |= 0x20;
    else
       P0  &= 0xdf;

    // Activate reset
    P0  &= 0x7f;

    //Should never reach this line
    for(;;);
}
*/

void SPI_startCommand(void);


/*
// Writes to external data memory goes to external program memory when set
void ProgramPin(BOOL state)
{ 
    if (state)
       P0  |= 0x40;
    else
       P0  &= 0xbf;
}
*/

//...............................................................................//

// Start an SPI command
void SPI_startCommand(void)
{
    SPI_SelPin(FALSE);
    SPI_ClkPinLow();
    SPI_DoutPin(FALSE);
}

//...............................................................................//

// End an SPI command
void SPI_endCommand(void)
{
    SPI_SelPin(TRUE);
    SPI_ClkPinHigh();
    SPI_DoutPin(TRUE);
}

//--------------------------------------------------------------------------------
//  Send and receive a byte to the SPI device
//--------------------------------------------------------------------------------
unsigned char SPI_sendReceiveByte(unsigned char in)
{
    BYTE i;
    BYTE data = in;

    SPI_ClkPinLow();
    for(i=0; i<8; i++)
    {
       SPI_DoutPin(data & 0x80);        // Send MSB
       data = (data<<1) | SPI_DinPin(); // Read next bit
       SPI_ClkPinHigh();
       SPI_ClkPinLow();
    }
    return data;
}

//--------------------------------------------------------------------------------
//  This writes to the SPI bus multiplexer on the Nanobord's FPGA and selects the
//  SPI device.
//--------------------------------------------------------------------------------
void SPI_open(unsigned char device)
{
  if(DEVICE_AUDIO_CODEC == device)  // special case: select via multiplexer
  {
    SPI_ModePin(FALSE);   // disable writes to Spartan 100
    SPI_SEL_AUDIO = 1;    // select SPI multiplexer for MAX1104EUA
    return;
  }
  else
  {
    SPI_SEL_AUDIO = 0;    // select SPI multiplexer for Nanoboard SPI bus
  }
  device |= 0x80;    //Enable bus request bit in address register
  SPI_ModePin(TRUE); //Access SPI address register in Nanoboard Controller
  SPI_sendReceiveByte(device);
  while (SPI_DinPin()==1) /* wait */;
  SPI_ModePin(FALSE);
}

//--------------------------------------------------------------------------------
// Close SPI channel and clear the address register
//--------------------------------------------------------------------------------
void SPI_close(void)
{
    SPI_SEL_AUDIO = 0;    // select SPI multiplexer for Nanoboard SPI bus
    SPI_ModePin(TRUE);      //Release bus back to address select mode
    SPI_sendReceiveByte(0); //Clear address latch
}

//----------------------------------------------------------------------------------//

// unsigned long flash_length = 0xffff; //Default length
// long flash_address = 0;     //Default start address
// BYTE flash_device = DEVICE_FLASH_EMBEDDED;       //Default memory device

//----------------------------------------------------------------------------------//
// read the electronic signature to wake the device
// also takes chip out of deep powerdown
//----------------------------------------------------------------------------------//
unsigned char Flash_readElectronicSignature(void)
{
    BYTE result;
    SPI_startCommand();
    SPI_sendReceiveByte(M25P40_RES);       // Send RES command
    SPI_sendReceiveByte(0);                // send three dummy bytes
    SPI_sendReceiveByte(0);
    SPI_sendReceiveByte(0);
    result = SPI_sendReceiveByte(0);       // Read signature
    SPI_endCommand();
    return result;
}

//----------------------------------------------------------------------------------//
//  Send a 24 bit address to memory
//----------------------------------------------------------------------------------//
void Flash_sendAddress24(unsigned long flash_address)
{    
    SPI_sendReceiveByte((unsigned char) (flash_address>>16));   // Send bits 23..16
    SPI_sendReceiveByte((unsigned char) (flash_address>>8));    // Send bits 15..8
    SPI_sendReceiveByte((unsigned char)  flash_address);        // Send bits 7..0
}

//----------------------------------------------------------------------------------//
// Open a flash file for reading starting at 'address'
// subsequent SPI_sendReceiveByte commands will autoincrement the address
// leaves chip enable active
//----------------------------------------------------------------------------------//
void Flash_openRead(unsigned long address)
{
    SPI_startCommand();                  // set Chip enable line low
    SPI_sendReceiveByte(M25P40_READ);
    Flash_sendAddress24(address);
}

//----------------------------------------------------------------------------------//

/*

void Flash_copyToRAM(void)
{
    BYTE data;
    char __xdata *destAddress = (char __xdata *)0;

    SPI_open(flash_device); //Open embedded flash memory
    Flash_readElectronicSignature();
    Flash_openRead();

    while (flash_length>0)
    {    
        data  = SPI_sendReceiveByte(0); //Read next byte from flash
        P1 = data; //Echo to LEDS
        *(destAddress++) = data;
        flash_length--;
       }
    SPI_close();
    P1 = 0;
}

//----------------------------------------------------------------------------------//
//Load program and data from flash memory
void Flash_loader(void)
{
    //Load program into memory
    ProgramPin(TRUE);
    flash_address = 0;       //Start of program in flash
    flash_length = 0xffff;   //Length of program
    flash_device = DEVICE_FLASH_CONFIGURATION;
    Flash_copyToRAM();

    //Load data into memory
    ProgramPin(FALSE);
    flash_address = 0x10000;  //Start of data in flash
    flash_length = 0xffff;    //Length of data in RAM
    flash_device = DEVICE_FLASH_EMBEDDED;    
    Flash_copyToRAM();
    
    Reset(RESET_EXTERNAL);    //Reset CPU and execute from external program memory        
}

//----------------------------------------------------------------------------------//


void main(void)
{    
    Flash_loader();
}

*/

//------------------------------------------------------------------------------
// Programs the ICS307 to generate a new output frequency
// Output Frequency for CLK1 = Fref * 2 * (VDW + 8) / ((RDW + 2) * OD)
// Parameters: 
//   Config: Configuration word :
//
//         MSB                                 LSB
//        +----+----+-----+----+----+----+----+----+
//        | C1   C0 | TTL | F1   F0 | S2   S1   S0 |
//        +----+----+--+--+--+-+-+--+-+--+----+--+-+
//           |   |     |     |   |    |          |
//           +-+-+     |     +-+-+    +-----+----+
//             |       |       |            |
//        Crystal    Output  CLK2      Output Divide
//        Load       Duty    Output
//        Impedance  Cycle   Select
//
//  VDW    : VCO Divider Word  (9 significant bits)
//  RDW    : Reference Divider Word  (7 significant bits)
//------------------------------------------------------------------------------
void ICS307_Program(unsigned char Config, unsigned int VDW, unsigned char RDW)
{
  register unsigned char temp;
  VDW &= 0x1FF;            // only 9 bits count
  RDW &= 0x7F;             // only 7 bits count
  temp = ((unsigned char) VDW) << 7;   // bit-mangle 3rd byte
  temp |= RDW;
  SPI_open(DEVICE_CLOCK);
  SPI_SelPin(0);
  SPI_sendReceiveByte(Config);
  SPI_sendReceiveByte(VDW >> 1);
  SPI_sendReceiveByte(temp);
  SPI_SelPin(1);
//  SPI_SelPin(0);
//  SPI_SelPin(1);
  SPI_close();
}

//-------------------------------------------------------------
// Programs command word 'data' into the clock generator chip
//-------------------------------------------------------------
void ICS307_ProgramW(unsigned long data)
{
  SPI_open(DEVICE_CLOCK);   // select spi channel
  SPI_SelPin(0);            // select device
  SPI_sendReceiveByte((data >>16)&0xFF);
  SPI_sendReceiveByte((data>>8)&0xFF);
  SPI_sendReceiveByte(data & 0xFF);
  SPI_SelPin(1);
  SPI_close();
}

//---------------------------------------------------------------------------------------------

//------------------------------------------------------------------
// Configures MAX1104
// Leaves ADC and DAC on and enabled, both in continuous conversion mode
// leaves SPI channel set to MAX1104 and the device selected
//------------------------------------------------------------------
void MAX1104_Open()
{
  SPI_open(DEVICE_AUDIO_CODEC);
  SPI_SelPin(0);
//  SPI_sendReceiveByte(MAX1104_START | MAX1104_A1 | MAX1104_A0 | MAX1104_C0 | MAX1104_E1 | MAX1104_E2);
  SPI_sendReceiveByte(MAX1104_START | MAX1104_A1 | MAX1104_A0 | MAX1104_C0 | MAX1104_E1| MAX1104_E0);
  DelayMs(1);
}


//------------------------------------------------------------------
// Configures MAX1104
// Powers down ADC and DAC
// deselects the device closes SPI channel
//------------------------------------------------------------------
void MAX1104_Close()
{
  SPI_SelPin(1); // terminate contiuous conversion
  DelayMs(1);
  SPI_SelPin(0);
  SPI_sendReceiveByte(MAX1104_START | MAX1104_A1 | MAX1104_A0);  // power down
  SPI_SelPin(1); // deselect
  SPI_close();
}



#define MAX1104_TEST_AMPLITUDE 50
//-----------------------------------------------------------------------------
//  outputs a squarewave and digitises it again to compare the amplitudes
//  ' cycles' is the number of cycles over which the measurement is averaged
//-----------------------------------------------------------------------------
static unsigned char Max1104ReadWrite(unsigned char cycles)
{
  volatile unsigned char InByte;
  register unsigned char temp;
  register unsigned char min=0xFF;
  register unsigned char max=0;
  signed int difference;
  MAX1104_Open();
  EA = 0;
  difference = 0;
  for(cycles=0; cycles<20; cycles++)
  {
    temp = 127 + MAX1104_TEST_AMPLITUDE;
    InByte = SPI_sendReceiveByte(temp);     // send byte out to D/A
    InByte = SPI_sendReceiveByte(temp);     // send byte out to D/A
    Delay10Us(10);
    InByte = SPI_sendReceiveByte(temp);     // read byte back after A/D conversion
    if(InByte > max) max = InByte;
    temp = 127 - MAX1104_TEST_AMPLITUDE;
    InByte = SPI_sendReceiveByte(temp);     // send byte out to D/A
    InByte = SPI_sendReceiveByte(temp);     // send byte out to D/A
    Delay10Us(10);
    InByte = SPI_sendReceiveByte(temp);     // read byte back after A/D conversion
    if(InByte < min) min = InByte;
    difference += (max-min);
  }
  EA = 1;
  MAX1104_Close();
  difference /= cycles;
  return (unsigned char) difference;
}

#define AVERAGING 8
#define SCALING 2
//-----------------------------------------------------------------------------
//  Allows adjustment of the analog gain trimmer
// displays the adjustment direction graphically
//-----------------------------------------------------------------------------
void MAX1104_Adjust(void)
{
  do
  {
    signed int result;
    register unsigned char i;
    signed int position;
    result = 0;
    for (i=0; i<AVERAGING; i++) // calculate average over a few readings to reduce jitter
      result += Max1104ReadWrite(5);
    result /= AVERAGING;
    position = (result - MAX1104_TEST_AMPLITUDE);
    position /= SCALING;
    LCD_GotoXY(0,0);
    if(0 == position)
    {
      OutStr("OK Press Any Key",0);
    }
    else
    {
      OutStr("Adjust VR2 ",0);
      {
        if(position > 0)
          OutStr("left ",0);
        else
          OutStr("right",0);
      }
    }
    LCD_BarGraph_Bi(position,BG_RES_PERDIGIT*8,8,1,BG_RES_PERDIGIT*8);
 /*                             // this is the old text-based bar-graph routine
    LCD_GotoXY(0,1);            // draw bargraph
    if(position < 0)
      position = 0;
    for(i=0;i<16;i++)
    {
      if(position < MIDSCALE)
      {
        if(i<position)
        {
          LCD_WriteChar('.');
        }
        else
        {
          if(i<=MIDSCALE)
            LCD_WriteChar('>');
          else
            LCD_WriteChar('.');
        }
      }
      else
      {
        if((i<position) && (i>MIDSCALE))
          LCD_WriteChar('<');
        else
          LCD_WriteChar('.');
      }
    }
*/
  }
  while (!(KbHit));   // wait for any key
  while(KbHit)
    GetKey(0);
  LCD_ClrScr();
}


//----------------------------------------------------------------------
// Reads the frequency Counter
// Returns: Frequency in kHz
//----------------------------------------------------------------------
unsigned int FrequencyCount(void)
{
   unsigned int retval;
   DelayMs(5);  // wait for update
   retval   = FREQ1_PORT;
   retval <<= 8;
   retval  |= FREQ0_PORT;
   return retval;
}

#define MAX_FREQ_DEVIATION 50   // maximum allowable tolerance value for frequency measurement [kHz]
//---------------------------------------------------------------------------
// Measures the frequency of the Nanoboard on-board Variable Clock Generator
// Then reprograms the Clock frequency and compares the two frequencies
// We do this because there is no way to obtain a status from the ICS307
// Returns : 0 = Success
//           1 = No frequency change detected
//---------------------------------------------------------------------------
unsigned char TestICS307(void)
{  register unsigned int Freq1, Freq2;
   register unsigned char retval = 0;
   ICS307_ProgramW(ICS307_30MHZ);
//   ICS307_Program(ICS307_Default_Config, ICS307_Default_VCO_DIV, ICS307_Default_REF_DIV);  // set to 20MHZ
   Freq1 = FrequencyCount();
   if(((30000 + MAX_FREQ_DEVIATION) - Freq1) > ( 2*MAX_FREQ_DEVIATION)) retval = 1;  // make sure frequency counter reads close to 20.000Hz
//   ICS307_Program(ICS307_Default_Config,ICS307_Default_VCO_DIV +16 ,ICS307_Default_REF_DIV);  // set to 40MHZ
   ICS307_ProgramW(ICS307_25MHZ);
   Freq2 = FrequencyCount();
   if(((25000 + MAX_FREQ_DEVIATION) - Freq2) > ( 2*MAX_FREQ_DEVIATION)) retval = 1;  // make sure frequency counter reads close to 40.000Hz
//   ICS307_Program(ICS307_Default_Config, ICS307_Default_VCO_DIV, ICS307_Default_REF_DIV);  // set back to 20MHZ
   ICS307_ProgramW(ICS307_30MHZ);
   return retval;
}

//------------------------------------------------------------------
// Reads Electronic Signature of both SPI Flash Memories
// Returns :  0 both signatures received
//            1 Missing Configuration Flash Signature
//            2 Missing Embedded Flash Signature
//            3 Both Signatures Missing
//------------------------------------------------------------------
unsigned char TestM25P40Signature(void)
{
  register unsigned char retval = 0;   // return value
  SPI_open(DEVICE_FLASH_CONFIGURATION);  // check for response from configuration flash first
  if(
//       (M25P40_RES_RESPONSE != Flash_readElectronicSignature())    // for 4MBit device
//     &&
       (M25P80_RES_RESPONSE != Flash_readElectronicSignature())    // for 8MBit device
    )
  {
    retval |= DEVICE_FLASH_CONFIGURATION;
  }
  SPI_close();
  SPI_open(DEVICE_FLASH_EMBEDDED);   // check for response from embedded flash next
  if(
//       (M25P40_RES_RESPONSE != Flash_readElectronicSignature())    // for 4MBit device
//     &&
       (M25P80_RES_RESPONSE != Flash_readElectronicSignature())    // for 8MBit device
    )
  {
    retval |= DEVICE_FLASH_EMBEDDED;
  }
  SPI_close();
  return(retval);
}

//-------------------------------------------------------------------------------
// Test data is exchanged with the Nanoboard controller by sending and receiving
// a 32 bit (4 byte) command to SPI address 3.
// Currently only the first and last bytes are used to exchange data.
// The last byte sent contains the bits to set the LEDs and the last byte received
// contains the state of the jumpers. ('1' = open '0' = inserted)
// The first byte sent (0xc0) places the Nanoboard controller into test mode.
// If the first byte is not 0xC0 (eg. 0x00) the Nanoboard controller reverts back
// to non-test mode
//-------------------------------------------------------------------------------
unsigned char Nanoboard_TestModeOn(unsigned char leds)
{
  unsigned char byte1,byte2,byte3,byte4;
  SPI_open(DEVICE_TESTER);
  SPI_startCommand();
  byte1 = SPI_sendReceiveByte(0xC0);  // Bit 31 = LED BANK enable,  BIT30 = disable normal jumper function
  byte2 = SPI_sendReceiveByte(0x00);  // reserved for future expansion
  byte3 = SPI_sendReceiveByte(0x00);  // reserved for future expansion
  byte4 = SPI_sendReceiveByte(leds);   //Set LEDSs and return status of jumpers.
  SPI_endCommand();
  SPI_close();
  return byte4;
}

//-----------------------------------------------------------------------------------
// turns NanoBoard Test Mode off, LEDs and Jumpers revert to their normal functions
//-----------------------------------------------------------------------------------
void Nanoboard_TestModeOff(void)
{
  SPI_open(DEVICE_TESTER);
  SPI_startCommand();
  SPI_sendReceiveByte(0x00);  // Bit 31 = LED BANK enable,  BIT30 = disable normal jumper function
  SPI_sendReceiveByte(0x00);  // reserved for future expansion
  SPI_sendReceiveByte(0x00);  // reserved for future expansion
  SPI_sendReceiveByte(0x00);  // don't care
  SPI_endCommand();
  SPI_close();
}









