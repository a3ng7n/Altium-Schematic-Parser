#ifndef __UART_H__
#define __UART_H__

#define BAUDRATE 9600

#ifndef BAUDRATE
  #error "BAUDRATE undefined"
#endif

// Takes character 'c and sticks it into the transmit buffer
// this assumes that Timer1 is programmed
// to run at the correct baudrate
void UART_TxChar_Poll(unsigned char c);

// sets timer1 up for Baudrate generation
void UART_Init(void);

// this are the return codes for TestSerial
enum {SERTEST_SUCCESS, SERTEST_TIMEOUT, SERTEST_BITERROR};

//--------------------------------------------------------
// Test if character is echoed correctly
// returns SERTEST_SUCCESS, if success
//         SERTEST_TIMEOUT, if timeout
//         SERTEST_BITERROR if bit error
// Parameters: FirstChar is the first character that gets sent
//             LastChar is the final Character that gets tested
//             We actually send Firstchar - LastChar + 1 characters
//--------------------------------------------------------
unsigned char TestSerial(unsigned char FirstChar, unsigned char LastChar);

//-------------------------------------------------------
// Selects Uart Multiplexer
// port is the mask for SER_SEL[0..2] (eg SER_SEL_RXDTXD)
//-------------------------------------------------------
void UART_Select(unsigned char port);


#endif
