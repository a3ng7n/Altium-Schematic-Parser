#ifndef __SIO_H__
#define __SIO_H__


//------------------------------------------------
//  Send a Character
//------------------------------------------------
void TXSerial(unsigned char c);

//---------------------------------------------------
// returns 0 if characters are in the receive buffer
// returns 1 if receive buffer is empty
//---------------------------------------------------
unsigned int RxBufferEmpty(void);

//----------------------------------------------------
// Receive Serial Character
//----------------------------------------------------
unsigned char RXSerial(void);


void __interrupt(1) SERIAL_ISR(void) ;


//------------------------------------------------------
// initialises SRL0
//------------------------------------------------------
void SIO_Init(unsigned int BaudRate);


//----------------------------------------------------
// returns 1 if RI is set
//----------------------------------------------------
inline unsigned char  SIO_RI_Set(void)
{
   unsigned int sc;
   sc = SER_REG(SCON_OFS);
   return (sc & 0x01);  // character received?
}


//----------------------------------------------------
// returns SBUF
// resets RI flag
//----------------------------------------------------
inline unsigned char SIO_Get_SBUF(void)
{
   unsigned int sc,sb;
   sc = SER_REG(SCON_OFS);             // get SCON
   sb = SER_REG(SBUF_OFS);             // read character
   SER_REG(SCON_OFS) = sc & ~0x01;     // clear RI flag
   return (unsigned char) (sb &0xFF);
}

#endif // __SIO_H__
