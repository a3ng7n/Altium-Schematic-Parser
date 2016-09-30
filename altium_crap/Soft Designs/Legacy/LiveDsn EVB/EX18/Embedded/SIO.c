#include "hware.h"
#include "sio.h"
#include "AudioMixer.h"


//------------------------------------------------
//  Send a Character
//------------------------------------------------
void TXSerial(unsigned char c)
{
    unsigned int sc;
    unsigned int timeout;
    timeout = 0x100000;
    do
    {
      sc = SER_REG(SCON_OFS);
      sc &= 0x02;
      timeout--;
    }
    while ((0 == sc) && timeout);  // wait for TI to come true
    sc = SER_REG(SCON_OFS);
    sc &= ~0x02;               // clear TI
    SER_REG(SCON_OFS) = (unsigned char) sc;
    SER_REG(SBUF_OFS) = c;       // stick character into transmit buffer
}


//---------------------------------------------------
// returns 0 if characters are in the receive buffer
// returns 1 if receive buffer is empty
//---------------------------------------------------
unsigned int RxBufferEmpty(void)
{
  return ! SIO_RI_Set();
}

//----------------------------------------------------
// Receive Serial Character
//----------------------------------------------------
unsigned char RXSerial(void)
{
  return SIO_Get_SBUF();
}



//------------------------------------------------------
// initialises SRL0
//------------------------------------------------------
void SIO_Init(unsigned int BaudRate)
{
    unsigned long divider;
    SER_REG(ADCON_OFS) = 0x80;  // set to internal baud rate generator
    SER_REG(TCON_OFS)  = 0x00;  // stop timer
    SER_REG(SCON_OFS)  = 0x70;  // Mode 1, enable receiver
    SER_REG(PCON_OFS)  = 0x80;  // SCON = 1
    divider = (((unsigned long)FCLK) << 4)/(((unsigned long)BaudRate) * 32);  // use integer arithmetic
    if((divider &0xF) >= 0x8)   // rounding
      divider += 0x10;
    divider >>= 4;
    divider = 1024 - divider;
    SER_REG(SRELL_OFS) = 0xFF & divider;
    SER_REG(SRELH_OFS) = 0x03 & (divider >> 8);
}


// Serial Interrupt
void __interrupt(1) SERIAL_ISR(void)
{
    // Serial Interrupt Processing here if implemented
}


