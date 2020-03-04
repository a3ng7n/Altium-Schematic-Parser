//-------------------------------------------------------------
//       _____  _____ ____ _______ _____ __
//      |  __ \|_   _/ __ \__   __| ____/_ |
//      | |__) | | || |  | | | |  | |__  | |
//      |  _  /  | || |  | | | |  |___ \ | |
//      | | \ \ _| || |__| | | |   ___) || |
//      |_|  \_\_____\____/  |_|  |____/ |_|
//
// Virtex II pro Rocket I/O tester routines
//-------------------------------------------------------------

#define REVISION 0x0091
#include "hware.h"
#include "lcd.h"
#include "stdio.h"
#include "spi.h"
#include "strio.h"

#define OK_SHOWTIME 400                                    // display time for -OK- message in ms

#define TEST_START_ADR  0x0000                             // start address of test
#define TEST_END_ADR    0x4000                             // end address of test

#define BAUDRATE 9600

#define UART_DEBUG_ACTIVE                                  // if this line is commented out, the no uart output takes place. this speeds up the test
#define DUMP_CHUNK_SIZE 0x80                               // size of memory chunk (in 16 bit words) that gets hex dumped to serial port if UART_DEBUG_ACTIVE is defined

#define OP_LCD 0                                           // output device for STRIO is LCD
#define OP_SER 1                                           // Serial Port
#define OP_ALL 3                                           // both LCD and serial port

static unsigned char OutputDevice = OP_ALL;                // output device for STRIO

//--------------------------------------------
// Prints Hello Message and revision number
//--------------------------------------------
void Hello(void)
{
  LCD_ClrScr();
  LCD_BACKLIGHT = 1;
  OutStr("\r\n",0);
  LCD_ClrScr();
  OutStr(__DATE__" " ,0);
  LCD_GotoXY(0,1);
  OutStr(__TIME__" \r\n" ,0);
  DelayMs(500);
  LCD_ClrScr();
  OutStr(" VIRTEX II PRO  ",0);
  LCD_GotoXY(0,1);
  OutStr(" Rocket I/O PORT ",0);
  DelayMs(1000);
  LCD_ClrScr();
  OutStr(" Tester ",0);
  LCD_GotoXY(0,1);
  OutStr(" REVISION %X.",REVISION >>8);
  OutStr("%X",REVISION & 0xFF );
  DelayMs(1000);
  LCD_ClrScr();
}

//--------------------------------------------
// Initialises ports to default values
//--------------------------------------------
void Port_Init(void )
{
    RAM_LCD    = SELECT_LCD;                               // xdata mapped to LCD
    RAM_MODE16 = MODE_8BIT;                                // map XDATA to CPU
    TX_START = 0;                                          // stop transmitter
    RX_START = 0;                                          // stop receiver
    LCD_BACKLIGHT = 1;                                     // turn on LCD backlight
    LED_PORT = 0;                                          // turn off LEDs
    RIO_RESET = 1;                                         // reset RIO Transceivers
    RIO_RESET = 0;
}

//--------------------------------------------------------------------------------------------------
// UART is used for debugging only
// Memory contents before and after rocket I/O transfer can be dumped to the serial port
//--------------------------------------------------------------------------------------------------
void UART_Init(void)
{
    __idata static unsigned char th_one;
    TR1 = 0;                                               // stop timer 1
    TMOD &=0x0F;
    TMOD |= 0x20;                                          // timer 1 is in mode 2: 8 bit auto reload
    PCON |= 0x80;                                          // Baud rate is 1/32th FOSC
    SCON  =  0x50;                                         // 8N1, REN
    th_one   = (unsigned char) (256-(((FOSC/192.0)/(float)BAUDRATE))); // TH1 holds reload value
    TH1  = th_one;
    TR1 = 1;                                               // enable timer 1
}

// Takes character 'c and sticks it into the transmit buffer
// this assumes that Timer1 is programmed
// to run at the correct baudrate
void UART_TxChar_Poll(unsigned char c)
{
#ifndef UART_DEBUG_ACTIVE                                  // do nothing if debug is not active
  return;
#else
    SBUF = c;                                              // stick character in transmit buffer
    while (TI == 0)                                        // wait for TI flag to come true
    {
    }
    TI = 0;                                                // clear flag again
#endif                                                     // UART_DEBUG_ACTIVE
}

// ---------------------------------
// all STRIO goes via this function
// ---------------------------------
void __Out_Char(unsigned char c)
{
   switch(OutputDevice)
   {
     case OP_SER:
       UART_TxChar_Poll(c);
     break;
     case OP_ALL:
       UART_TxChar_Poll(c);
     case OP_LCD:
     default:
       switch(c)
       {
          case '\r':                                       // ignore for LCD
          case '\n':
          break;
          default:
            LCD_WriteChar(c);
       }
     break;
   }
}
//------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------
// Writes a hex dump of SRAM contents from 'StartAddr' to 'EndAddr' to the serial port
//------------------------------------------------------------------------------------------
void Dump_RAM(unsigned int StartAddr, unsigned int EndAddr)
{
    volatile unsigned int address = StartAddr;
    OutputDevice = OP_SER;
    RAM_MODE16 = MODE_8BIT;                                // map XDATA back to CPU
    do
    {
       volatile unsigned char inbyte;
       inbyte =  (*(__xdata volatile unsigned char *) (address) );
       if((address % 16) == 0)
       {
           OutStr("\r\n%04X:",address);
       }
       OutStr("%02X ",inbyte);
    }  while (address++ != EndAddr);
    OutputDevice = OP_ALL;
}

// -----------------------------------------------------------------
// Fills Ram from 'StartAddress' to 'EndAddress' with byte 'pattern'
// Hex dumps the contents following Startaddress to serial port
// -----------------------------------------------------------------
void Fill_RAM(unsigned int StartAddress, unsigned int EndAddress, unsigned char c)
{
    unsigned int address = StartAddress;
    RAM_MODE16 = MODE_8BIT;                                // map XDATA back to CPU
    while(address <= EndAddress)
    {
       (*(__xdata volatile unsigned char *) (address++) ) = c;
    }
    Dump_RAM(StartAddress, StartAddress+ DUMP_CHUNK_SIZE - 1);
}

// ----------------------------------------------------------------------------
// Tests Rocket I/O channel from Startaddress to EndAddress
// channel : 0 = channel 0
//           1 = channel 1
// StartAddress : 0..0xFFFF
// EndAddress   : 0..0xFFFF
// No range checks are performed
// returns: Number of Errors
// ----------------------------------------------------------------------------
unsigned int TestChannel(unsigned char channel, unsigned int StartAddress, unsigned int EndAddress)
{
    unsigned int timeout = 0xFFFF;
    volatile unsigned int address = 0;
    unsigned int retval = 0;                               // return value
    if (channel != 0)
      RIO_SELECT = 1;
    else
      RIO_SELECT = 0;
    RAM_MODE16 = MODE_8BIT;                                // map XDATA to CPU
    LCD_ClrScr();
    OutStr("\r\n\r\nChannel %u",channel);
    OutputDevice = OP_SER;
    OutStr("\r\nbefore transmission:\r\n",0);
    OutputDevice = OP_ALL;
    LCD_GotoXY(0,1);
    OutStr("Clearing RAM...",0);
    Fill_RAM(TEST_START_ADR,TEST_END_ADR,(channel == 0) ? 0XAA : 0xBB);
    LCD_GotoXY(0,1);
    OutStr("Transmitting...    ",0);
    RAM_MODE16 = MODE_16BIT;                               // map XDATA to Receiver
    while (TX_DONE == 0);                                  // wait for transmitter to finish  todo: timeout
    RX_START  = 1;                                         // start receiver
    RX_START  = 0;
    DelayMs(2);
    TX_START = 1;                                          // start transmitter
    TX_START = 0;
    while ((TX_DONE == 0) && (--timeout)) ;                // wait for transmitter to finish todo: timeout
    if(!timeout)
      return -1;
    timeout = 0xFFFF;
    while ((RX0_DONE == 0)&& (--timeout)) ;                // wait for receiver to finish   todo: timeout
    if(!timeout)
      return -1;
    RAM_MODE16 = MODE_8BIT;                                // map XDATA back to CPU
    OutputDevice = OP_SER;
    OutStr("\r\nafter transmission:\r\n",0);
    Dump_RAM(0,DUMP_CHUNK_SIZE - 1);                       // dump RAM contents onto serial port
    LCD_GotoXY(0,1);
    OutStr("Verifying...    ",0);
    for(address = StartAddress; address <=EndAddress; address++) // verify contents
    {
       volatile unsigned int int_in;
       int_in  = (*(__xdata volatile unsigned char *)  (address << 1)     );
       int_in |= (*(__xdata volatile unsigned char *) ((address << 1) +1) ) << 8;
       if(int_in != address)
         retval ++;                                        // increment error counter
    }
    return retval;
}

//-------------------------------------------------------------------------
// Beeps the internal squawker
// Pitch: smaller number = higher pitch
// Duration: number of cycles
//-------------------------------------------------------------------------
void Beep(unsigned char Pitch, unsigned int Duration)
{
  __bit OnOff = 0;
  while(Duration--)
  {
    volatile unsigned char d;
    SPKR_OUT = OnOff ? 1 : 0;
    OnOff = OnOff ? 0 : 1;
    for(d=0;d<Pitch;d++);
    {
    }
  }
}

//-------------------------------------------------------------------------
// Beeps and asks for keypress if 'code' is non-0
// prints OK for 1s and exits if 'code is 0
//-------------------------------------------------------------------------
void ErrorBeep(unsigned int code)
{
  LCD_GotoXY(0,1);
  if(0 == code)
  {
    OutStr("\r\n - OK -         \r\n",0);
    DelayMs(OK_SHOWTIME);
  }
  else
  {
    OutStr("\r\n - FAILED -     \r\n",0);
    while(TEST_BUTTON != BUTTON_DOWN)
    {
      Beep(100,40);
      DelayMs(200);
      LCD_BACKLIGHT = LCD_BACKLIGHT ? 0 : 1;
      LED_PORT = LCD_BACKLIGHT ? 0x33 : 0xCC;
    }
    while(TEST_BUTTON == BUTTON_DOWN) DelayMs(10);
    RIO_RESET = 1;                                         // reset RIO Transceivers
    Delay10Us(50);                                         // wait a short while
    RIO_RESET = 0;                                         // reset inactive  --> transmit sync Characters
  }
  LCD_BACKLIGHT = 1;
  LED_PORT = 0x00;
  LCD_ClrScr();
}

//--------------------------------------------------------------
// checks if any loopback dipswitches are enabled and prints
// warning if that is tha case
//--------------------------------------------------------------
void CheckLoopBack(void)
{
  while(LOOP_0_PAR || LOOP_0_SER || LOOP_1_PAR || LOOP_1_SER)
  {
    unsigned int i;
    LCD_ClrScr();
    OutStr("\n\rWARNING:",0);
    LCD_GotoXY(0,1);
    OutStr("LOOPBACK ACTIVE ",0);
    Beep(100,40);
    for(i=0; i< 80; i++)
    {
      if(TEST_BUTTON == BUTTON_DOWN) goto LEAVE;
      DelayMs(10);
    }
    LCD_ClrScr();
    OutStr("Check DIP ",0);
    LCD_GotoXY(0,1);
    OutStr("Switches !\r\n",0);
    OutputDevice = OP_LCD;                                 // print warning only once on serial port
    for(i=0; i< 80; i++)
    {
      if(TEST_BUTTON == BUTTON_DOWN) goto LEAVE;
      DelayMs(10);
    }
  } // while
  LEAVE:
  OutputDevice = OP_ALL;
}

void main(void)
{
    Port_Init();
    LCD_Init();
    UART_Init();
    ICS307_ProgramW(ICS307_80MHZ);
    Hello();
    ICS307_ProgramW(ICS307_60MHZ);                         // Run Test at 60MHz RIO clock, corresponding to 1.2GHz bit rate
    OutStr("\r\nRocket I/O Tester\r\n\r\n",0);
    for(;;)
    {  static unsigned int channel_select = 0;
       LCD_BACKLIGHT = 1;
       for(channel_select = 0; channel_select <= 1; channel_select ++)
       {
          unsigned int ErrorCount;
          CheckLoopBack();
          ErrorCount= TestChannel(channel_select,TEST_START_ADR,TEST_END_ADR);
          OutputDevice = OP_SER;
          OutStr("\r\n\n%6u Bytes checked \r\n",TEST_END_ADR-TEST_START_ADR);
          OutStr("\r\n%6u Errors detected!\r\n",ErrorCount);
          OutStr("Failure Rate: %u%%\r\n",(100 * (unsigned long)ErrorCount) / TEST_END_ADR-TEST_START_ADR);
          OutputDevice = OP_ALL;
          LCD_GotoXY(0,1);
          ErrorBeep(ErrorCount);
       }
    }
}
