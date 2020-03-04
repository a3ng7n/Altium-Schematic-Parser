
#include "hware.h"
#include "uart.h"


// -----------------------------------------------------------
// initialise UART for  BAUDRATE
// UART runs in mode 1
// -----------------------------------------------------------
void UartInit(void)
{
    __idata static unsigned char th_one;
    TR1 = 0;            // stop timer 1
    PCON  = 0x80;       // Set SMOD
    SCON  =  0x50;      // 8N1, REN

    PCON_bit(7) = 1;    // SMOD = 1 -> high speed baud rate

    TL1 = 0;            // initialise to zero
    th_one   = (unsigned char) (256-(((FOSC/192.0)/(float)BAUDRATE))+0.5);    // TH1 holds reload value
    TH1  = th_one;

    TMOD_bit(7) = 0;    // disable GATE
    TMOD_bit(6) = 0;    // operate as timer
    TMOD_bit(5) = 1;
    TMOD_bit(4) = 0;    // 8-bit timer auto-reload mode

    TR1 = 1;            // enable timer 1
    ES  = 1;            // enable serial interrupt
    PS  = 1;            // set serial interrupt high priority
}

// -----------------------------------------------------------
// Takes character 'c and sticks it into the transmit buffer
// this assumes that Timer1 is programmed
// to run at the correct baudrate
// -----------------------------------------------------------
void UART_TxChar_Poll(unsigned char c)
{
    SBUF = c;          // stick character in transmit buffer
    while (TI == 0)    // wait for TI flag to come true
    {

    }
    TI = 0;            // clear flag again
}


typedef struct
{
   unsigned char Ring[RXBUFSIZE];
   unsigned char tail;  // index to last read entry
   unsigned char head;  // index to first valid entry
} RxBufType;

static volatile RxBufType RxBuf={{0}};

//----------------------------------------------------------
// returns 0 if characters are present in ringbuffer
// returns 1 if buffer is empty
// ---------------------------------------------------------
unsigned char RxBufEmpty(void)
{
    unsigned char retval;
    ES = 0;
    retval = (RxBuf.head == RxBuf.tail) ? 1 : 0;
    ES = 1;
    return(retval);
}

//----------------------------------------------------------
// adds next character to receive ringbuffer
// if buffer is full, the character is lost
//----------------------------------------------------------
static void RxBufAddChar(unsigned char c)
{
    unsigned char pos = RxBuf.head;
    if (++RxBuf.head >= RXBUFSIZE) RxBuf.head = 0;
    if (RxBuf.head == RxBuf.tail)   // full?
    {
       RxBuf.head = pos;
    }
    else
    {
       RxBuf.Ring[RxBuf.head]=c;
    }
}

// ----------------------------------------------------------------
// returns next character from  Receive Ringbuffer
// if buffer is empty, the last received character is returned
// ----------------------------------------------------------------
unsigned char RxBufGetChar(void)
{
   unsigned volatile char c;
   ES = 0;
   c = 0x55;
   if(RxBuf.head == RxBuf.tail) return c; // empty  --> don't change indices, just return last character
   if (++RxBuf.tail >= RXBUFSIZE) RxBuf.tail = 0;
   c = RxBuf.Ring[RxBuf.tail];
   ES = 1;
   return c;
}


/*------------------------------------------------
UART Interrupt Service Routine.

Set a breakpoint on 'overflow_count++' and run the
program in the debugger.  You will see this line
executes every 65536 clock cycles.
------------------------------------------------*/
__interrupt(INTVEC_SIO) void UART_ISR (void)
{
    register unsigned char c;
    if(RI)
    {
      RI = 0;
      c = SBUF;
      RxBufAddChar(c);
    }
}


