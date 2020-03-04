//-------------------------------------------------------------
//    _   _  __  __  _____     ____   ____  ___
//   | \ | ||  \/  ||_   _|   / ___| |  _ \|_ _|
//   |  \| || |\/| |  | |     \___ \ | |_) || |
//   | |\  || |  | |  | |      ___) ||  __/ | |
//   |_| \_||_|  |_|  |_|_____|____/ |_|   |___|
//                      |_____|
// (c) 2003 Altium
// Started: 04.12.2003 Ch.Weimann
// SPI driver Routines for NanoBoard Tester
// Adapted from Michael Rodway's Code
//-------------------------------------------------------------

#include "nmt_spi.h"
#include "hware.h"

#include "nmt_lcd.h"
#include "nmt_kbd.h"

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


void SPI_startCommand(void);

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
//  Send a 24 bit address to memory
//----------------------------------------------------------------------------------//
void Flash_sendAddress24(unsigned long flash_address)
{
    SPI_sendReceiveByte((unsigned char)(flash_address>>16));   // Send bits 23..16
    SPI_sendReceiveByte((unsigned char)(flash_address>>8));    // Send bits 15..8
    SPI_sendReceiveByte((unsigned char)flash_address);       // Send bits 7..0
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









