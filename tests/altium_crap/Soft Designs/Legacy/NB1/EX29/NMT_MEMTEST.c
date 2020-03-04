//
//   _   _  __  __  _____     __  __  _____  __  __  _____  _____  ____  _____
//  | \ | ||  \/  ||_   _|   |  \/  || ____||  \/  ||_   _|| ____|/ ___||_   _|
//  |  \| || |\/| |  | |     | |\/| ||  _|  | |\/| |  | |  |  _|  \___ \  | |
//  | |\  || |  | |  | |     | |  | || |___ | |  | |  | |  | |___  ___) | | |
//  |_| \_||_|  |_|  |_|_____|_|  |_||_____||_|  |_|  |_|  |_____||____/  |_|
//                     |_____|
//
// (c) 2003, 2004 Altium
// Started: 06.11.2003 Ch.Weimann
// V0.93 : 03.02.04 Ch.W. Fixed bug in LCD_GotoXY
// V0.94 : 19.04.04 Ch.W. Fixed bug in xdata_mux hardware, improved test vectors, added
//                        Bank switching hardware test
// V0.95 : 12.07.04 Ch.W. Fixed missing loopback hardware introduced in V0.94
// V0.96 : 16.08.04 Ch.W. added typecasts to suppress implicit int --> unsigned char warnings
//
// Tests Nanoboard On-board SRAM Memory and serves as CAN bus echo station for main tester
// Runs on 4k FPGA internal Block Ram, therefore many modules have been stripped down to a
// bare minimum to conserve code space


#define REVISION 0x0096

#include <regtsk51a.sfr>

#include "hware.h"
#include "uart.h"
#include "nmt_kbd.h"
#include "nmt_timer.h"
#include "nmt_lcd.h"
#include "nmt_xram.h"
#include "nmt_spi.h"

#define OK_SHOWTIME 400  // display time for -OK- message in ms

#define print _rom_LCD_WriteString

//#define TEST_MODE_ON     // define this to artificially create an error on banks 2 & 3

void Hello(void)
{
  LCD_ClrScr();
  LCD_BACKLIGHT = 1;
  LCD_ClrScr();
  print(__DATE__);
  LCD_GotoXY(0,1);
  print(__TIME__);
  DelayMs(500);
  LCD_ClrScr();
  print(" MEMORY TESTER");
  LCD_GotoXY(0,1);
  print(" (c)2004 ALTIUM   ");
  DelayMs(800);
  LCD_GotoXY(0,1);
  print(" REVISION ");
  LCD_HexByte_Write(REVISION >>8 );
  LCD_WriteChar('.');
  LCD_HexByte_Write(REVISION & 0xFF );
  DelayMs(800);
  LCD_ClrScr();
}

//-------------------------------------------------------------------------
// Beeps the internal squawker
// Volume [0..255], 0 = off, 255 = loudest
// Pitch: smaller number = higher pitch
// Duration: number of cycles
//-------------------------------------------------------------------------
void Beep(unsigned char Volume, unsigned char Pitch, unsigned int Duration)
{
  __bit OnOff = 0;
  while(Duration--)
  {
    volatile unsigned char d;
    SPK_PORT = OnOff ? Volume : 0;
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
    print(" - OK -         ");
    DelayMs(OK_SHOWTIME);
  }
  else
  {
    print(" - FAILED -     ");
    while(KbHit) GetKey(KEY_FORMAT_ASCII);
    while(!(KbHit))
    {
      Beep(200,100,40);
      DelayMs(200);
    }
    while(KbHit) GetKey(KEY_FORMAT_ASCII);
  }
  LCD_ClrScr();
}

inline void KeyBeep(void)
{
  Beep(128, 0xFF - (150 + LastKey * 4), 100);
}

/*------------------------------------------------
MAIN C function
------------------------------------------------*/
void main (void)
{
  register unsigned char c;
  register unsigned int temp=0;
  register unsigned int Count=0;
  unsigned long l=1;
  unsigned char jumpers = 0;

RESTART:
  Timer0Init();
  EA = 1;                             // Global Interrupt Enable
  SPEAKER_ENABLE = 1;
  ICS307_ProgramW(ICS307_30MHZ);      // run at known clock frequency
  UART_Init();                        // for CAN bus echo
  LCD_Init();
  Hello();
  print("   STATIC RAM");
  LCD_GotoXY(0,1);
  print("  MEMORY TEST");
  DelayMs(1000);
  LCD_ClrScr();
  print("Bank Switch:");
  ErrorBeep(memTestBankSwitching(42, 4));   // Test bank switching logic
  LCD_GotoXY(0,1);
  DelayMs(200);
  LCD_ClrScr();
  for(c=0;c<4;c++)
  {
    print("Testing Bank ");
    LCD_WriteChar(c+'0');
    LCD_GotoXY(0,1);
#ifdef TEST_MODE_ON
    if(c<2)
      ErrorBeep(TestXRAM(c,0xFFFB));     // leave out the four top bytes mapped to the LCD
    else
      ErrorBeep(TestXRAM(c,0xFFFC));     // leave out the four top bytes mapped to the LCD
#else
      ErrorBeep(TestXRAM(c,0xFFFB));     // leave out the four top bytes mapped to the LCD
#endif
  }

  UART_Select(SER_SEL_CAN);  // select CAN bus
  LCD_ClrScr();
  print(" Ready for CAN");
  LCD_GotoXY(0,1);
  print(" Bus Echo Test");
  Count = 0;
  for (;;)   // main loop never returns, there is no operating system to return to
  {
    if(Count++ >1000)                  // display 'life pulse' signal
    {
      Count = 0;
      c = (c==0xA5) ? '*' : 0xA5;
      LCD_GotoXY(15,1);
      LCD_WriteChar(c);
    }
    LED_PORT = ~DIP_PORT;                        // mirror DIP-Switches on LEDs
    jumpers = ~Nanoboard_TestModeOn(jumpers);    // mirror jumpers on Nanoboard LEDs
    if(RI)                                       // Character Received on CAN-bus  ?
    {
      unsigned char c;
      RI=0;
      c=SBUF;
      c^=0xAA;                          // invert every second bit
      DelayMs(5);                       // wait for 5 ms
      UART_TxChar_Poll(c);              // send it back
      LED_PORT = 0xFF;                  // flash LEDs on
      RI = 0;                           // ignore own echo
      DelayMs(100);
      RI = 0;
    }
    if(KbHit)                           // Restart on any key
    {
      GetKey(KEY_FORMAT_ASCII);
      KeyBeep();
      goto RESTART;
    }
  } // for(;;)
}


