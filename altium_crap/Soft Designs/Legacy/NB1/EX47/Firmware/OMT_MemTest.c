#define REVISION 0x0091

#include <regtsk51a.sfr>

#include "hware.h"
#include "Uart.h"
#include "OMT_Kbt.h"
#include "OMT_Timer.h"
#include "OMT_Lcd.h"
#include "OMT_xRam.h"
#include "OMT_SPI.h"
#include "VIIMT_I2c.h"
#include "MAX1617A.h"

#define OK_SHOWTIME 400                                    // display time for -OK- message in ms

#define print _rom_LCD_WriteString

#define TOP_BANK 0x00F                                     // top 64k bank

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
  print(" Daughterboard  ");
  LCD_GotoXY(0,1);
  print(" Memory Tester  ");
  DelayMs(400);
  LCD_GotoXY(0,1);
  print(" REVISION ");
  LCD_HexByte_Write(REVISION >>8 );
  LCD_WriteChar('.');
  LCD_HexByte_Write(REVISION & 0xFF );
  DelayMs(400);
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
  static unsigned char c;
  register unsigned int temp=0;
  register unsigned int Count=0;
  unsigned long l=1;
  unsigned char jumpers = 0;

RESTART:
  ObRamInit();                                             // initialise On-Board SRAM control lines
  Timer0Init();
  EA = 1;                                                  // Global Interrupt Enable
  SPEAKER_ENABLE = 1;
  ICS307_ProgramW(ICS307_80MHZ);                           // run at known clock frequency: 80MHz so this does not take forever

  UART_Init();                                             // for CAN bus echo
  LCD_Init();
  Hello();
  LCD_ClrScr();
  print(" Daughterboard ");
  LCD_GotoXY(0,1);
  print("  Memory Test  ");
  Count = 0;
  for (;;)                                                 // main loop never returns, there is no operating system to return to
  {
    unsigned char Error = 0;
    unsigned int i = 0;
    XR_SelectBank(0);
    if(memTestDataBus(42L))
      Error |= 1;                                          // quick check data bus
    if(memTestBankSwitching(42, TOP_BANK + 1))             // check bank switching logic
      Error |= 2;
    LCD_ClrScr();
    print("Daughter SRAM");
    if(!Error)
    {
      for(unsigned char i=0;i<=TOP_BANK;i++)                             // test 16 banks of 64 k
      {
         LCD_GotoXY(0,1);
         print("BANK ");
         LCD_HexByte_Write(i);
         c=memTestBank(i);
         if(c != 0)
         {
            Error |= 4;
            break;
         }
      }
    }
    ErrorBeep(Error);
    DelayMs(300);
    // test Max1617 temperature sensor
    Error = 0;
    LCD_ClrScr();
    print("MAX1617 Test");
    if(ACK != Max1617_SendByte(SPOR))                      // issue software power on reset
    {
      LCD_ClrScr();
      print("MAX1617 NO RESP.");
      ErrorBeep(1);
    }
    Max1617_WriteRegister(WCRW, 0x07);                     // set conversion rate to maximum
    LCD_ClrScr();
    print("Temp Alarm Test");
    if(0==P3&0x01)
      Error |= 1;                                          // alarm active after reset?
    Max1617_WriteRegister(WLHO, -25);                      // set local Thigh Limit to -25dC to trigger Alarm
    while(i++ < 20)                                        // wait for a maximum of 200 ms
    {
       if(0 == P3&0x01) break;                             // poll alarm flag
       DelayMs(10);
    }
    if(!(i<20))
      Error |= 2;
    ErrorBeep(Error);
    // display temperatures on LCD.
    for(;;)
    {
      LED_PORT = P3;
      if(Count++ >500)
      {
        unsigned char TString[5];
        Count = 0;
        LCD_GotoXY(0,0);
        print("MAX1617 : ");
        c=Max1617_GetTemperature(SENSOR_MAX1617);
        Max1617_Temperature2String(c, TString);
        LCD_WriteString(TString); LCD_WriteChar(0xDF); LCD_WriteChar('C');
        print("   ");
        LCD_GotoXY(0,1);
        print("FPGA    : ");
        c=Max1617_GetTemperature(SENSOR_FPGA);
        Max1617_Temperature2String(c, TString);
        LCD_WriteString(TString); LCD_WriteChar(0xDF); LCD_WriteChar('C');
        print("   ");
        if(KbHit)                                          // Restart on any key
        {
          c=GetKey(KEY_FORMAT_ASCII);
          switch(c)
          {  case 'A':                                     // 'A' key triggers alarm flag
               Max1617_WriteRegister(WLHO, -25);           // set local Thigh Limit to -25dC to trigger Alarm
               break;
             case '7':                                     // '7' key resets alarm flag
               Max1617_SendByte(SPOR);                     // issue software power on reset
               Max1617_WriteRegister(WCRW, 0x07);          // set conversion rate to maximum
               break;
             default:                                      // any other key restarts test
               KeyBeep();
               goto RESTART;
          }
        }
      }
    }
  } // for(;;)
}


