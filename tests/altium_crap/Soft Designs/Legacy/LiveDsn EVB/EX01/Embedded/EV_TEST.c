//
// (c) 2003, 2004 Altium
// Started: 19.08.2004 Ch.W.
// V0.90  : 1st cut
//
// Tests Nexar Evaluation Board functionality for production testing
// Runs on 16k FPGA internal Block Ram, therefore many modules have been stripped down to a
// bare minimum to conserve code space
// Tests 16 banks of 64k*8 onboard SRAM

#define REVISION 0x0090

#include <regtsk51a.sfr>

#include "hware.h"
#include "uart.h"
#include "timer.h"
#include "vlcd.h"
#include "ps2.h"
#include "xram.h"
#include "strio.h"
#include "led7.h"
#include "tunes.h"

#define OK_SHOWTIME 400  // display time for -OK- message in ms

#define TOP_BANK 0x00F    // top 64k bank

#define PRINT_MODE_LCD 1
#define PRINT_MODE_LED 2
#define PRINT_MODE_SER 4

#define PLAY_MAGNETIC  // if this is defined background music will be played at startup
#define PLAY_GRIEG     // if this is defined the melody during Memory testing is played

#define VOL_BACKGROUND 0x20  // Volume setting for background music
#define VOL_OK         0xFF   // Volume setting for OK jingle
#define VOL_ERROR      0xFF   // Volume setting for Error beep
#define VOL_AUDIOTEST  0x80   // Volume setting for audio test

static unsigned char PrintMode = PRINT_MODE_LCD; // each bit that is set will cause output on that device

unsigned char ErrorCount = 0;  // adds up Errors

//---------------------------------------------------
// all character I/O is handled by this primitive
//---------------------------------------------------
void __Out_Char(unsigned char x)
{
    if(PrintMode & PRINT_MODE_SER)  UART_TxChar_Poll(x);
    if(PrintMode & PRINT_MODE_LED)  Seg7_WriteDigitN(x);
    if(PrintMode & PRINT_MODE_LCD)  LCD_WriteChar(x);
}

void Test7Seg(void)
{
   char Pattern[6]={0};
   unsigned char Segment,Digit;
   for(Segment = 0;Segment < 5;Segment++)  // Knight rider on LD Leds
   {
     for(Digit=0;Digit<8;Digit++)
     {
       LED_PORT = 1<<Digit;
       DelayMs(40);
     }
     for(Digit=0;Digit<6;Digit++)
     {
       LED_PORT = 0x40>>Digit;
       DelayMs(40);
     }
   }
   LED_PORT = 0;
   Seg7_SetAll(0xFF);  // All LEDs on
   DelayMs(300);
   Seg7_SetAll(0);  // clear display
   for(Digit = 0; Digit <6; Digit++)   // every segment one at a time
   {
      Pattern[Digit] = 1;
      for(Segment = 0;Segment <=7;Segment++)
      {
         Seg7_Set(Pattern);
         DelayMs(150);
         Pattern[Digit]<<=1;
      }
   }
   Seg7_SetAll(0);  // clear display
}



void TestSound(void)
{
    unsigned char left, right;
    right = 96;
    EA = 0;
    VOLUME_PORT = VOL_AUDIOTEST;
    for(left=0;left<96;left++)
    {
       NOTEL_PORT = left;
       NOTER_PORT = right--;
       DelayMs(20);
    }
    EA = 1;
}

//-------------------------------------------------------------------------
// Beeps and asks for keypress if 'code' is non-0
// prints OK for 1s and exits if 'code is 0
// increases global variable 'ErrorCount' if 'code' is non-0
//-------------------------------------------------------------------------
void ErrorBeep(unsigned int code)
{
  if(0 == code)
  {
    OutStr(" - OK -",0);
//    print(" - OK -    %d   ",ErrorCount);      // DEBUG only
    DelayMs(OK_SHOWTIME);
  }
  else
  {
    ErrorCount++;
    OutStr("FAILED: %04X    ",code);
    KbHit = 0;  // clear key input
    VOLUME_PORT = VOL_ERROR;
    SetMelody(ErrorL,ErrorR);
    while(!KbHit);
    SetMelody(Silence,Silence);
  }
  LCD_ClrScr();
}


void Hello(void)
{
  LCD_ClrScr();
  LCD_SetBacklight(1);
  LCD_ClrScr();
  OutStr(__DATE__,0);
  LCD_GotoXY(0,1);
  OutStr(__TIME__,0);
  DelayMs(500);
  LCD_ClrScr();
  OutStr("Evaluation Board",0);
  LCD_GotoXY(0,1);
  OutStr("Tester          ",0);
  DelayMs(400);
  LCD_GotoXY(0,1);
  OutStr(" REVISION %X.",REVISION >>8 );
  OutStr("%02X",REVISION & 0xFF );
  DelayMs(400);
  LCD_ClrScr();
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
  while(KbHit) GetKey();      // clear Keyboard buffer
  do
  {
    if(KbHit)                  // abort if Test/Reset key was pressed
    {
      if(0==(GetKey() & M_KEY_TEST))
      {
        LCD_GotoXY(0,1);   // restore line 2 again
        OutStr("                ",0);
        LCD_GotoXY(0,1);
        LED_PORT = 0;   // clear LEDs again
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
        LCD_WriteChar('*');
      }
      else
      {
        LCD_WriteChar(0x30+i+1);
        finished = 0;
      }
    }
    c = ~DIP_PORT;
    LED_PORT = c;                                  // mirror DIP-switches on LEDs
//    jumpers =  Nanoboard_TestModeOn(~jumpers);     // mirror Jumpers on SL LEDs
    for(i=0; i<8; i++)
    {
      if (c == patterns[i])
      {
        if(patterns[i] != 0)  // new valid pattern?
        {
          patterns[i] = 0;    // mark in pattern array as done
//          Beep(VOLUME,100,20);
        }
      }
    }
    DelayMs(10);
  } while (finished == 0);
  LCD_GotoXY(0,1);   // restore line 2 again
  OutStr("                ",0);
  LCD_GotoXY(0,1);
  LED_PORT = 0;   // clear LEDs again
  return 0;
}


//---------------------------------------------------
// Test Keys
// Prompts the user tp press all keys
//---------------------------------------------------
unsigned char TestKeys(void)
{
   unsigned char KeyList = 0x3F;
   unsigned char i;
   unsigned char mask;
   unsigned volatile char c;
   do
   {
      Seg7_SetCurrentPosition(0);
      for(i=0;i<6;i++)
      {
         mask = 1<<i;
         c = ~GetKey();
         if(c==mask)
         {
           KeyList&=~mask;
         }
         Seg7_WriteDigitP((KeyList & mask) ? 0x80 : 0x00);
//       Seg7_WriteDigitP(mask);
      }
     KbHit = 0;
     while(!KbHit);
   } while (KeyList);
   Seg7_SetAll(0x00);
   return 0;
}


//--------------------------------------
// Tests Onboard Static Ram
// Returns 0 if success
// Non-0 if failure
//--------------------------------------
unsigned char TestSRAM(void)
{
   unsigned char Error = 0;
    unsigned int i = 0;
    XR_SelectBank(0);
    if(memTestDataBus(42L))
      Error |= 1;      // quick check data bus
    if(memTestBankSwitching(42, TOP_BANK + 1))   // check bank switching logic
      Error |= 2;
    LCD_ClrScr();
    OutStr("OnBoard SRAM",0);
    if(!Error)
    {
      for(i=0;i<=TOP_BANK;i++)                // test 16 banks of 64 k
      {
         unsigned char c;
         LED_PORT = 1<<(unsigned char)(i/2);
         TestPS2();                           // just to keep blinking the kbd-LEDs
         LCD_GotoXY(0,1);
         OutStr("                ",0);
         LCD_GotoXY(0,1);
         Seg7_SetAll(0);
         Seg7_SetCurrentPosition(0);
         PrintMode |= PRINT_MODE_LED;
         OutStr("%4u",64*(1+ (unsigned int)i));
         PrintMode &= ~PRINT_MODE_LED;
         OutStr("kB",0);
         c=memTestBank((unsigned char)i);
         if(c != 0)
         {
            Error |= 4;
            break;
         }
         else
         {
//            OutStr(" --OK-- ",0);
         }
      }
    }
    LCD_GotoXY(0,1);   // restore line 2 again
    OutStr("                ",0);
    LED_PORT = 0;
    LCD_GotoXY(0,1);
    return(Error);
}


/*------------------------------------------------
MAIN C function
------------------------------------------------*/
void main (void)
{
  register unsigned int temp=0;
  register unsigned int Count=0;
  unsigned long l=1;
  unsigned char jumpers = 0;
  ROMSIZE = ROM_SIZE >> 8;

  ObRamInit();              // initialise On-Board SRAM control lines
  RAM_LCD = SELECT_RAM;     // default to RAM select instead of LCD
  Timer0Init();
  EA = 1;                             // Global Interrupt Enable
  SPEAKER_ENABLE = 1;
  UART_Init();                        // for RS-232 Test
  LCD_Init();
  PS2_Init();                         // for keyboard port test
AGAIN:
  ErrorCount = 0;  // initialise Error counter
  SetMelody(Silence,Silence);  // turn background melody off
#ifdef PLAY_MAGNETIC
  VOLUME_PORT = VOL_BACKGROUND;
  SetMelody(MagneticL, MagneticR);
#endif
  LED_PORT = 0xFF;   // turn all LEDs off
//  LED_PORT = 0;   // turn all LEDs off
  LCD_ClrScr();   // Clear LCD
  Seg7_SetAll(0xFF);  // clear LED display
//  Seg7_SetAll(0x00);  // clear LED display
//  NOTEL_PORT = NOTER_PORT = 22;  // 100Hz

  Hello();
  OutStr("Press TEST/RESET",0);
  OutStr("To Start Test",0);
  KbHit = 0;
  do
  {
    while(!KbHit)
    {
      unsigned char i;
      temp = temp ? 0 : 1;
      TestPS2();                           // just to keep blinking the kbd-LEDs
      UART_Select(temp ? SER_SEL_RTSCTS : SER_SEL_RXDTXD);        // select RXD-->TXD
      for(i=0;i<200;i++)
        UART_TxChar_Poll(0x00);
    }
  } while (GetKey()!= 127) ;
  SetMelody(Silence,Silence);  // turn background melody off
  LCD_ClrScr();
  OutStr("Press All Keys",0);
  LCD_GotoXY(0,1);
  ErrorBeep(TestKeys());
  LCD_ClrScr();
  OutStr("DIP SWITCH TEST ",0);
  LCD_GotoXY(0,1);
  ErrorBeep(TestDipSwitches());
  LCD_ClrScr();
  OutStr("7 Segment",0);
  LCD_GotoXY(0,1);
  OutStr("Display Test",0);
  Test7Seg();                         // play with LED display
  LCD_ClrScr();
  UART_Select(SER_SEL_RXDTXD);        // select RXD-->TXD
  OutStr("RS-232 TEST ",0);
  LCD_GotoXY(0,1);
  OutStr("TXD->RCD ",0);
  ErrorBeep(TestSerial(0,255));
  LCD_ClrScr();
  UART_Select(SER_SEL_RTSCTS);        // select RTS-->CTS
  OutStr("RS-232 TEST ",0);
  LCD_GotoXY(0,1);
  OutStr("RTS->CTS ",0);
  ErrorBeep(TestSerial(0,255));
  LCD_ClrScr();
  OutStr("User I/O TEST ",0);
  LCD_GotoXY(0,1);
  ErrorBeep(TestUserIO());
  LCD_ClrScr();
  OutStr("PS2 Port ",0);
  LCD_GotoXY(0,1);
  ErrorBeep(TestPS2());
  LCD_ClrScr();
  OutStr("Sound Test",0);
  LCD_GotoXY(0,1);
  TestSound();
  LCD_ClrScr();
  VOLUME_PORT = VOL_BACKGROUND;
#ifdef PLAY_GRIEG
  SetMelody(GriegL,GriegR);        // play tune during Memory Test
#endif
  OutStr("SRAM Test",0);
  LCD_GotoXY(0,1);
  ErrorBeep(TestSRAM());
  SetMelody(Silence,Silence);
  LCD_ClrScr();
  if(!ErrorCount)
  {
    VOLUME_PORT = VOL_OK;
    SetMelody(OkSoundL, OkSoundR);
    OutStr("SUCCESS !!!",0);
  }
  else
  {
    VOLUME_PORT = VOL_ERROR;
    SetMelody(ErrorL,ErrorR);
    OutStr("%2d Errors!",ErrorCount);
  }
  KbHit = 0;        // wait for key press
  while (!KbHit)
  {
    if(ErrorCount)
    {
      LCD_SetBacklight(0);
      Seg7_SetAll(0xFF);
      LED_PORT = 0x00;
      DelayMs(200);
      LCD_SetBacklight(1);
      Seg7_SetAll(0x00);
      LED_PORT = 0xFF;
      DelayMs(200);
    }
  };
  goto AGAIN;
}


