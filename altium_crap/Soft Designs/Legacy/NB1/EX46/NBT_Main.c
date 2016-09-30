//--------------------------------------------------------------------------------------------------
//     _   _  ____  _____     __  __     _     ___  _   _
//    | \ | || __ )|_   _|   |  \/  |   / \   |_ _|| \ | |
//    |  \| ||  _ \  | |     | |\/| |  / _ \   | | |  \| |
//    | |\  || |_) | | |     | |  | | / ___ \  | | | |\  |
//    |_| \_||____/  |_|_____|_|  |_|/_/   \_\|___||_| \_|
//                     |_____|
//
// (c) 2003, 2004 Altium
// Started: 06.11.2003 Ch.W.
// Production Functional Tester for the Altium Nanoboard
// It assumes that all cables and the RTC adapter PCB are present and a second Nanoboard
// is connected running the Memory Tester application for CAN bus echo testing
// 06.01.2004: Ch.W. V0.90 sped up Ok messages and Knight rider display
// 13.01.2004: Ch.W. V0.91 added #defines for various timings
// 16.01.2004: Ch.W. V0.92 added PS2 keyboard read interrupt service routine
// 27.01.2004: Ch.W. V0.93 fixed bug in LCD_GotoXY, enhanced LCD functionality, added bargraph code
// 16.08.2005: Ch.W. V0.94 dropped printf and used OutStr instead. Changed beep timing loops for new compiler
//--------------------------------------------------------------------------------------------------

#include <regtsk51a.sfr>

#include "hware.h"
#include "uart.h"
#include "nbt_kbd.h"
#include "nbt_timer.h"
#include "nbt_lcd.h"
#include "nbt_i2c.h"
#include "nbt_adda.h"
#include "nbt_spi.h"
#include "nbt_ps2.h"
#include "nbt_rtc.h"
#include "nbt_bargraph.h"
#include "strio.h"

#define REVISION 0x0094

#define VOLUME 100           // internal buzzer volume for operational sounds
#define STATUS_VOLUME 250    // internal buzzer volume for success/failure sounds

#define ERRORBEEP_ACTIVE     // activates that annoying error beep

#define OK_SHOWTIME 400      // display time for -OK- message in ms

const unsigned int TestVoltages[] = {250,500,750,1000,0};   // voltages DA/AD for loopback test [2mV/bit]

static unsigned int ErrorCount;   // tallies the number of errors

static volatile unsigned char c = 0x55;  // for debugging

// ----------------------------------------------------------
// Does the 'Knightrider Thing' with the LEDs on the NanoBoard
// ----------------------------------------------------------
void KnightRider(unsigned int OnTimeMs, unsigned char count)
{
  __bit direction = 0;
  register unsigned char pattern = 1;
  Nanoboard_TestModeOn(pattern);     // select spartan 100 mode for spi access to LEDs
  while(count--)
  {
    LED_PORT = pattern;              // set LED[0..7] to pattern
    Nanoboard_TestModeOn(pattern);   // set SL[1..8] to pattern
    DelayMs(OnTimeMs);
    if(0 == direction)  // move right
    {
       pattern <<= 1;
       if(0==pattern)
       {
          direction = 1;
          pattern = 0x40;
       }
    }
    else                // move left
    {
       pattern >>= 1;
       if(0==pattern)
       {
          direction = 0;
          pattern = 0x02;
       }
    }
  }
  LED_PORT = 0;               // turn all LEDs off when exiting
  Nanoboard_TestModeOn(0);    // same for status leds
}

//-----------------------------------------------------------
// Prints status message and plays with LEDs
//-----------------------------------------------------------
void Hello(void)
{
  register unsigned char i;
  __bit line = 0;
  LCD_ClrScr();
  LCD_BACKLIGHT = 1;
  LCD_ClrScr();
  OutStr(__DATE__,0);

  OutStr(__TIME__,0);
  DelayMs(500);
  LCD_ClrScr();
  OutStr("NANOBOARD TESTER",0);
  LCD_GotoXY(0,1);
  OutStr(" (c)2004 ALTIUM ",0);
  LCD_BACKLIGHT = 1;
  DelayMs(300);
  LCD_GotoXY(0,1);
  OutStr("VERSION %X.",REVISION >> 8);
  OutStr("%X    ",REVISION&0xFF);
  DelayMs(500);
  for(i=0; i<255; i++)
  {
    if(0==(i & 0x0F))
    {
      LCD_GotoXY(0,line);
      line ^=1;
      if(line)
      {
        KnightRider(30,15);
      }
    }
    LCD_WriteChar(i);
  }
  LCD_ClrScr();
  DelayMs(200);
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
  __bit OldEA = EA;
  EA = 0;
  Duration *= 2;
  while(Duration--)
  {
    volatile unsigned char d;
    SPK_PORT = OnOff ? Volume : 0;
    OnOff = OnOff ? 0 : 1;
    for(d=0;d<Pitch;d++);
    {
       unsigned char i;
       for(i=0;i<70;i++)
         __asm("NOP\n\t");
    }
  }
  EA = OldEA;
}

//------------------------------------------------------------------------------------
// Outputs walking pattern of ones on user Header A and reads it back on user Header B
// returns     0 : OK
//         non-0 : bit at which error occurred
//------------------------------------------------------------------------------------
unsigned char TestUserIO(void)
{
  unsigned long OutValue = 0x01L;
  unsigned long InValue  = 0;
  unsigned char bit = 1;
  do
  {
    OutValue &= 0x0003FFFFL;  // only 18 significant bits
    U1H0_PORT = OutValue  & 0xFF;
    U1H1_PORT = (OutValue >> 8 ) & 0xFF;
    U1H2_PORT = (OutValue >> 16) & 0xFF;
    DelayMs(1);
    InValue   = U1H2_PORT & 0x03;
    InValue <<= 8;
    InValue  |= U1H1_PORT;
    InValue <<= 8;
    InValue  |= U1H0_PORT;
    if(InValue != OutValue)
      return(bit);
    OutValue<<=1;
    bit++;
    DelayMs(10);
  }
  while (0 != OutValue);
  return 0;
}

//--------------------------------------------------------------------------------
// Plays with LEDs on Keyboards attached to PS2 ports
// returns: 0 = OK
//          Bit 0 set: error on PORT0  (KB)
//          Bit 1 set: error on PORT1  (MOUSE)
//--------------------------------------------------------------------------------
unsigned char TestPS2(void)
{
  unsigned char retval = 0;
  static unsigned char pattern = 1;
  pattern >>= 1;
  if(pattern == 0) pattern = 4;
  if(PS2_SetLEDs(0,pattern))
    retval |=1;
  if(PS2_SetLEDs(1,pattern ^ 0x07))
    retval |=2;
  return (retval);
}


//-----------------------------------------------------------------
// Tests Master-Slave IO pins
// Assumes that Master I/O and Slave I/O ports are connected via
// 10-way ribbon cable
// Outputs walking pattern of 1 and verifies input of same pattern
// Leaves port with all bits set to 0
// returns: 0     : success
//          non-0 : bit pattern at error point: High Nibble: Output Pattern
//                                              Low Nibble:  Input Pattern
//-----------------------------------------------------------------
unsigned char TestMasterSlaveIO(void)
{
  register unsigned char TestPattern = 1;
  register unsigned char c;
  do
  {
    MASTERIO_PORT = TestPattern;   // output test pattern
    __asm( "NOP\n\t");             // give it some time to stabilise
    c =  MASTERIO_PORT & 0x0F;
    if(TestPattern !=  c)
    {
      MASTERIO_PORT = 0;
      return TestPattern | (c<<4);
    }
    TestPattern <<= 1;            // next test pattern
    TestPattern &= 0x0F;
  } while(0 != TestPattern);
  MASTERIO_PORT = 0;
  return 0;
}

//-------------------------------------------------------------------------
// Beeps and asks for keypress if 'code' is non-0
// prints OK for 1s and exits if 'code is 0
// increases global variable 'ErrorCount' if 'code' is non-0
//-------------------------------------------------------------------------
void ErrorBeep(unsigned int code)
{
  LCD_GotoXY(0,1);
  if(0 == code)
  {
    OutStr(" - OK -         ",0);
//    print(" - OK -    %d   ",ErrorCount);      // DEBUG only
    DelayMs(OK_SHOWTIME);
  }
  else
  {
    ErrorCount++;
    OutStr("FAILED: %04X    ",code);
    while(KbHit) GetKey(KEY_FORMAT_ASCII);
    while(!(KbHit))
    {
#ifdef ERRORBEEP_ACTIVE
      Beep(STATUS_VOLUME,100,40);
      DelayMs(200);
#endif
    }
    while(KbHit) GetKey(KEY_FORMAT_ASCII);
  }
  LCD_ClrScr();
}

//---------------------------------------------------------------------------
// Outputs a characteristic beep for each key
//---------------------------------------------------------------------------
inline void KeyBeep(void)
{
  Beep(VOLUME, 0xFF - (150 + LastKey * 4), 100);
}


//-------------------------------------------------------------------------
// makes sure every key on keypad was pressed individually
// Test/Reset key aborts test, exept when prompted
// returns 0: success 1: aborted
//----------------------------------------------------------
unsigned char TestKeypad(void)
{
  register unsigned char c,i;
  __bit abort;
  unsigned char Keys[]="123C456D789EA0BF";
  LCD_ClrScr();
  OutStr("Press All Keys",0);
  while(KbHit)GetKey(0); // Clear all pending keypresses
  do
  {
    abort = 1;
    LCD_GotoXY(0,1);
    for(i=0;i<sizeof(Keys)-1;i++)
    {
      c=Keys[i];
      if(c != ' ') abort = 0;  // still more to go?
      LCD_WriteChar(c);
    }
    if(KbHit)
    {
      c  = GetKey(KEY_FORMAT_ASCII);
      KeyBeep();
      if('T' == c)       // abort if Test/Reset Key was pressed prematurely
      { LCD_ClrScr();
        OutStr("Keyboard Test",0);
        return 1;
      }
      for(i=0;i<sizeof(Keys);i++)
      {
        if (Keys[i] == c)
          Keys[i] = ' ';
      }
    }
  }
  while(!abort);
  LCD_ClrScr();
  OutStr("Press TEST/RESET",0);
  LCD_GotoXY(0,1);
  OutStr("Key to continue",0);
  c = 0;
  do
  {
    if(KbHit)
    {
      c  = GetKey(KEY_FORMAT_ASCII);
      KeyBeep();
    }
  } while(c != 'T');
  return 0;
}


//----------------------------------------------------------
// Makes sure every dip switch is activated individually
// Test/Reset key aborts test
// returns 0: success 1: aborted
//----------------------------------------------------------
unsigned char TestDipSwitches(void)
{
  __bit finished;
  register unsigned char c,i, jumpers = 0;
  unsigned char patterns[]={0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};
  LCD_ClrScr();
  OutStr("  DIP-Switches",0);
  while(KbHit) GetKey(0);      // clear Keyboard buffer
  do
  {
    if(KbHit)                  // abort if Test/Reset key was pressed
    {
      if('T'==GetKey(KEY_FORMAT_ASCII))
      {
        return 1;
      };
    }
    LCD_GotoXY(0,1);
    OutStr("    ",0);
    finished = 1;
    for(i=0; i<8; i++)
    {
      if(0 == patterns[i])
      {
        LCD_WriteChar(0xFF);
      }
      else
      {
        LCD_WriteChar(0x30+i+1);
        finished = 0;
      }
    }
    c = ~DIP_PORT;
    LED_PORT = c;                                  // mirror DIP-switches on LEDs
    jumpers =  Nanoboard_TestModeOn(~jumpers);     // mirror Jumpers on SL LEDs
    for(i=0; i<8; i++)
    {
      if (c == patterns[i])
      {
        if(patterns[i] != 0)  // new valid pattern?
        {
          patterns[i] = 0;    // mark in pattern array as done
          Beep(VOLUME,100,20);
        }
      }
    }
    DelayMs(10);
  } while (finished == 0);
  return 0;
}

//-----------------------------------------------------------------
// Makes sure every configuration jumper is activated individually
// Test/Reset key aborts test
// returns 0: success 1: aborted
//-----------------------------------------------------------------
unsigned char TestJumpers(void)
{
  __bit finished;
  register unsigned char i, jumpers = 0;
  unsigned char patterns[]={0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};
  LCD_ClrScr();
  OutStr(" Config Jumpers ",0);
  while(KbHit) GetKey(0);      // clear Keyboard buffer
  do
  {
    if(KbHit)                  // abort if Test/Reset key was pressed
    {
      if('T'==GetKey(KEY_FORMAT_ASCII))
      {
        return 1;
      };
    }
    LCD_GotoXY(0,1);
    OutStr("    ",0);
    finished = 1;
    for(i=0; i<8; i++)
    {
      if(0 == patterns[i])
      {
        LCD_WriteChar(0xFF);
      }
      else
      {
        LCD_WriteChar(0x30+i+1);
        finished = 0;
      }
    }
    LED_PORT = ~DIP_PORT;                          // mirror DIP-switches on LEDs
    jumpers =  ~Nanoboard_TestModeOn(jumpers);     // mirror Jumpers on SL LEDs
    for(i=0; i<8; i++)
    {
      if (jumpers == patterns[i])
      {
        if(patterns[i] != 0)  // new valid pattern?
        {
          patterns[i] = 0;    // mark in pattern array as done
          Beep(VOLUME,100,20);
        }
      }
    }
    DelayMs(10);
  } while (finished == 0);
  return 0;
}

//--------------------------------------------------------------
// Tests CAN bus
// sends out 'TestCharacter'
// monitors own echo
// TestCharacter is XORed with 'MagicMask (0xAA) and sent back
// by a second Nanoboard
//
// returns: 0 : success
//          1 : no local echo received
//          2 : local echo scrambled
//          3 : response timeout
//          4 : response scrambled
//--------------------------------------------------------------
unsigned char TestCAN(unsigned char TestCharacter)
{
  unsigned char MagicMask = 0xAA;
  unsigned char InChar;
  UART_Select(SER_SEL_CAN);    // select multiplexer to CAN bus
  RI = 0;                      // clear RI flag to empty any pending characters
  UART_TxChar_Poll(TestCharacter);  // send out Test Character
  DelayMs(1);                  // wait for local echo to be clocked in
  if(0==RI)
    return 1;          // make sure echo is received
  if(SBUF != TestCharacter)
    return 2;          // make sure the right character is echoed
  RI = 0;
  DelayMs(100);                // wait for echo
  if(0== RI)
    return 3;                  // nothing at all came back
  InChar = SBUF;               // read character
  RI = 0;                      // clear flag
  InChar ^=MagicMask;          // unscramble
  if(InChar != TestCharacter)  // wrong character?
    return 4;
  return 0;                    // if we get to here everything has worked as expected
}

//---------------------------------------------------------------
// Uses RTC to time 1s to measure Variable system clock [kHz]
// assumes that Philips PCF8583 is connected to external I2C bus
// programs RTC to all 0s, then polls for 1 s elapsed at 100th second resolution
// This is not terribly accurate, but ensures we can detect if
// the wrong crystal is fitted
// Verifies that the NanoBoard on-board oscillator crystal frequency is
// 'TargetFrequency' +- 'Tolerance' [KHz]
// returns     0: success
//         non-0: absolute of deviation from TargetFrequency
//---------------------------------------------------------------
unsigned int MeasureClockFrequency(unsigned int TargetFrequency, unsigned int Tolerance)
{
  unsigned int retval;
  FREQMODE_PORT = FREQ_MODE_RESET;
  FREQMODE_PORT = FREQ_MODE_COUNT;
  if( ACK != RTC_SetTimeHundredths(0L))
    return -1;  // Error: No RTC found
  while (100L > RTC_GetTimeHundredths());  // wait for 1s to elapse
  FREQMODE_PORT = FREQ_MODE_STOP;
  retval = FREQ1_PORT;      // read high byte
  retval <<=8;              // shift into position
  retval += FREQ0_PORT;     // add low byte
  FREQMODE_PORT = FREQ_MODE_AUTO;  // frequency counter to measure variable clock output again
  if(retval > TargetFrequency)     // check tolerance
    retval -= TargetFrequency;
  else
    retval = TargetFrequency - retval;
  if(retval <= Tolerance)
  {
    return 0;
  }
  return retval;
}

/*------------------------------------------------
MAIN C function
------------------------------------------------*/
void main (void)
{
  register unsigned char jumpers = 0;            // stores jumper configuration of NanoBoard
  register signed int temp=0;
  register unsigned int Count=0;
  unsigned long l=1;
  FREQMODE_PORT = FREQ_MODE_AUTO;  // frequency counter to measure variable clock frequency
RESTART:
  ErrorCount = 0;
  Timer0Init();
  EA = 1;                       // Global Interrupt Enable
  IT0 = 1;                      // Configure interrupt 0 for falling edge on /INT0 (P3.2)
                                // used for PS2 port clock
  EX0 = 1;                      // External Interrupt 0 Enable
  PX0 = 1;                      // assign high priority to External Interrupt 0

  Nanoboard_TestModeOn(0);      // switch Status LEDs to normal function
  SPEAKER_ENABLE = 1;
  ICS307_ProgramW(ICS307_30MHZ);  // run with a known frequency: 30MHz
  Beep(VOLUME,80,200);            // Sound 'I am awake' beep just in case everything else is dead
  Beep(VOLUME,70,200);
  UART_Init();
  PS2_Init();
  LCD_Init();          // initialise LCD
  LCD_SetCursor(0,0);  // turn cursor off
  GenerateBarGraphCustomCharacters();  // Load Custom Characters into CG ram
  c = DAC_Init();
  if(NACK==c)
  {
    OutStr("NO DAC COMMS",0);
    DelayMs(1000);
  };
  ErrorCount = 0;
  Hello();
  OutStr("ADC Init",0);
  ErrorBeep(ADC_Init());
  OutStr("ADC Config",0);
  ErrorBeep(ADC_Config(0));
  OutStr("On-Board FLASH",0);
  ErrorBeep(TestM25P40Signature());
  OutStr("Clock Generator",0);
  ErrorBeep(TestICS307());
  ErrorBeep(TestKeypad());
  ErrorBeep(TestDipSwitches());
  ErrorBeep(TestJumpers());
  OutStr("PS2 Ports",0);
  ErrorBeep(TestPS2());
  OutStr("RS-232 TXD->RXD",0);
  UART_Select(SER_SEL_RXDTXD);
  ErrorBeep(TestSerial(0x5A, 0x5F));
  OutStr("RS-232 RTS->CTS",0);
  UART_Select(SER_SEL_RTSCTS);
  ErrorBeep(TestSerial(0x5A, 0x5F));
  OutStr("User IO",0);
  ErrorBeep(TestUserIO());
  OutStr("Master-Slave I/O",0);
  ErrorBeep(TestMasterSlaveIO());
  OutStr("Audio Codec Adj.",0);
  MAX1104_Adjust();
  OutStr("CAN-BUS",0);
  ErrorBeep(TestCAN('*'));
  OutStr("Crystal Osc Freq",0);
  ErrorBeep(MeasureClockFrequency(20000,100));
  OutStr("ADC/DAC Test",0);
  ErrorBeep(DAC_ADC_Test(16,TestVoltages));
  if(ErrorCount)
  {
    OutStr("Total Errors: %d",ErrorCount);
    Beep(STATUS_VOLUME,250,500);  // play failure sound
  }
  else
  {
    OutStr("SUCCESS",0);
    Beep(STATUS_VOLUME,200,100);  // play success sound
    Beep(STATUS_VOLUME,150,100);
  }
  for (;;)  // main loop, and embedded program never ends
  {
    jumpers =  Nanoboard_TestModeOn(~jumpers);     // mirror Jumpers on SL LEDs
    LED_PORT = ~DIP_PORT;                          // mirror DIP-Switches on LEDs
    if(!Timer[TIMER_1])
    {
      __xdata unsigned int i;
      Timer[TIMER_1] = TIMER_SECONDS(0.05);
      if(KbHit)   // has anyone pressed a key?
      {
        i  = GetKey(KEY_FORMAT_ASCII);     // read it and convert to ASCII equivalent
        LCD_ClrScr();
        LCD_BACKLIGHT = 1;
        switch(i)
        {
          case 'T':         // start whole test series again
            goto RESTART;
          case '1':         // all these keys allow repeating an individual test item
            Hello();
          break;
          case '2':
            OutStr("On-Board FLASH",0);
            ErrorBeep(TestM25P40Signature());
          break;
          case '3':
            OutStr("Clock Generator",0);
            ErrorBeep(TestICS307());
          break;
          case 'C':
            ErrorBeep(TestKeypad());
          break;
          case '4':
            ErrorBeep(TestDipSwitches());
          break;
          case '5':
            ErrorBeep(TestJumpers());
          break;
          case '6':
            OutStr("PS2 Ports",0);
            ErrorBeep(TestPS2());
          break;
          case 'D':
            OutStr("RS-232 TXD->RXD",0);
            UART_Select(SER_SEL_RXDTXD);
            ErrorBeep(TestSerial(0x5A, 0x5F));
          break;
          case '7':
            OutStr("RS-232 RTS->CTS",0);
            UART_Select(SER_SEL_RTSCTS);
            ErrorBeep(TestSerial(0x5A, 0x5F));
          break;
          case '8':
            OutStr("User IO",0);
            ErrorBeep(TestUserIO());
          break;
          case '9':
            OutStr("Master-Slave I/O",0);
            ErrorBeep(TestMasterSlaveIO());
          break;
          case 'E':
            OutStr("Audio Codec Adj.",0);
            MAX1104_Adjust();
          break;
          case 'A':
            OutStr("CAN-BUS",0);
            ErrorBeep(TestCAN('*'));
          break;
          case '0':
            OutStr("Crystal Osc Freq",0);
            ErrorBeep(MeasureClockFrequency(20000,100));
          break;
          case 'B':
            OutStr("ADC/DAC Test",0);
            ErrorBeep(DAC_ADC_Test(16,TestVoltages));
          break;
          case 'F':
            Hello();
          break;
          default:
            OutStr("Key='%c' ",i);
            OutStr("No=%02X",LastKey);
//            print("Key='%c' No=%02X",i,LastKey);
            KeyBeep();
          break;
        }
        OutStr("T/R to Restart",0);
        LCD_GotoXY(0,1);
        OutStr("Key->Indiv.Test",0);
      }
      c++;
      {
        static unsigned char Cnt = 5;
        if(0==Cnt--)   // play with status LEDs on attached PS2 Keyboards
        {              // every 5th time round the main loop
          Cnt = 5;
          TestPS2();                               // play with Keyboard LEDs
          LCD_BACKLIGHT = LCD_BACKLIGHT ? 0 : 1;   // play with LCD backlight
        }
      }
    }  // if
  } // for(;;)
}


