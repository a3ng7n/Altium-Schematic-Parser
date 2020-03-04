#ifndef __UART_H__
#define __UART_H__


#define BAUDRATE  57600.0     // baudrate for UART, used to program TIMER1 reload value
//#define BAUDRATE  38400.0     // baudrate for UART, used to program TIMER1 reload value
#define RXBUFSIZE 32          // size of interrupt driven RX ringbuffer


// -----------------------------------------------------------
// initialise UART for  BAUDRATE
// UART runs in mode 1
// -----------------------------------------------------------
void UartInit(void);

// -----------------------------------------------------------
// Takes character 'c and sticks it into the transmit buffer
// this assumes that Timer1 is programmed
// to run at the correct baudrate
// -----------------------------------------------------------
void UART_TxChar_Poll(unsigned char c);

//----------------------------------------------------------
// returns 0 if characters are present in ringbuffer
// returns 1 if buffer is empty
// ---------------------------------------------------------
unsigned char RxBufEmpty(void);

// ----------------------------------------------------------------
// returns next character from  Receive Ringbuffer
// if buffer is empty, the last received character is returned
// ----------------------------------------------------------------
unsigned char RxBufGetChar(void);


#endif

