#include "hware.h"
#include "OMT_Timer.h"
#include "OMT_Lcd.h"

#include "Uart.h"

// sets timer1 up for Baudrate generation
void UART_Init(void)
{
    __idata static unsigned char th_one;
    TR1 = 0;                                               // stop timer 1
    TMOD &=0x0F;
    TMOD |= 0x20;                                          // timer 1 is in mode 2: 8 bit auto reload
    PCON |= 0x80;                                          // Baud rate is 1/32th FOSC
    SCON  =  0x50;                                         // 8N1, REN
    th_one   = (unsigned char) (256-(((FOSC/192.0)/(float)BAUDRATE)));    // TH1 holds reload value
    TH1  = th_one;
    TR1 = 1;                                               // enable timer 1
}

// Takes character 'c and sticks it into the transmit buffer
// this assumes that Timer1 is programmed 
// to run at the correct baudrate
void UART_TxChar_Poll(unsigned char c)
{
    SBUF = c;                                              // stick character in transmit buffer
    while (TI == 0)                                        // wait for TI flag to come true
    {

    }
    TI = 0;                                                // clear flag again
}

//--------------------------------------------------------
// Test if character is echoed correctly
// returns SERTEST_SUCCESS, if success
//         SERTEST_TIMEOUT, if timeout
//         SERTEST_BITERROR if bit error
// Parameters: FirstChar is the first character that gets sent
//             LastChar is the final Character that gets tested
//             We actually send Firstchar - LastChar + 1 characters
//--------------------------------------------------------
unsigned char TestSerial(unsigned char FirstChar, unsigned char LastChar)
{
  register unsigned char c;
  register unsigned char i;  
  c = SBUF;                                                // make sure receiver is empty
  RI = 0;
  c = ~FirstChar;
  for(i = FirstChar; i < LastChar; i++)
  {
    UART_TxChar_Poll(i);
    Timer[TIMER_0] = 1 + TIMER_SECONDS((1.0/BAUDRATE));    // wait for Eleven Bit Times (to allow for tolerances)
    while(Timer[TIMER_0]);

    if(RI)                                                 // character received?
       {
          c = SBUF;
          RI = 0;
          if (c != i) return(SERTEST_BITERROR);
       }
       else                                                // no character has come back
       {
         return(SERTEST_TIMEOUT);
       }
  }
  return(SERTEST_SUCCESS);
}

//-------------------------------------------------------
// Selects Uart Multiplexer
// port is the mask for SER_SEL[0..2] (eg SER_SEL_RXDTXD)
//-------------------------------------------------------
void UART_Select(unsigned char port)
{
  register unsigned char i;
  port &= 0xC0;                                            // only top two bits are relevant
  i = P0;
  i &= 0x3F;
  i |= port;
  P0 = i;
}

