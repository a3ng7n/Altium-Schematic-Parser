//-----------------------------------------------------------
//       _   _  ____  _____     ___  ____    ____     _   _
//      | \ | || __ )|_   _|   |_ _||___ \  / ___|   | | | |
//      |  \| ||  _ \  | |      | |   __) || |       | |_| |
//      | |\  || |_) | | |      | |  / __/ | |___  _ |  _  |
//      |_| \_||____/  |_|_____|___||_____| \____|(_)|_| |_|
//                       |_____|
//  I2C.H
//  I2C Routines
//
//  (c) 2001 Ch.Weimann
//-----------------------------------------------------------

#ifndef I2C_H
#define I2C_H 1

#define ACK  (0)
#define NACK (1)

#define SCL_L  SCL=0
#define SCL_H  SCL_CTRL=CTRL_HIZ
#define SDA_L  SDA=0
#define SDA_H  SDA_CTRL=CTRL_HIZ

#define I2C_W  (0)  // bit for write
#define I2C_R  (1)  // bit for read

#define I2C_DELAY  {__asm( "NOP\n\t");__asm( "NOP\n\t");__asm( "NOP\n\t");__asm( "NOP\n\t");}

void I2C_Init(void);
//-----------------------------------------------------------
// initialises the lines to the idle state:
// SDA and SCL High impedance

void I2C_Start(void);
//-----------------------------------------------------------
// creates start condition and leave SCL Low on exit

void I2C_Stop(void);
//---------------------------------------------------------------
// assumes SCL is low and SDA is HIZ on entry
// creates stop condition and will leave SDA AND SCL HIgh on exit


unsigned char I2C_TxByte(unsigned char c);
//-----------------------------------------------------------
// Assumes SCL=L on entry
// SCL will be L on exit
// returns Acknowledge


unsigned char I2C_RxByte(unsigned int SendAck);
//------------------------------------------------------------
// Assumes SCL=L on entry
// SCL will be L on exit
// if 'SendAck' is non-0 Master will generate Ack
// returns Byte that was read


unsigned char I2C_WriteByte(unsigned char DevAddr, unsigned char TwoByteAddress, unsigned int Addr, unsigned char Data);
//----------------------------------------------------------------------------------------------------------------------
// Addresses Device 'DevAddr', and writes 'Data' to device internal address 'Addr'
// if 'TwoByteAddress' !=0 two adress bytes are sent
// Returns 'ACK' if success, 'NACK' if not

unsigned char I2C_ReadByte(unsigned char DevAddr, unsigned char TwoByteAddress, unsigned int Addr);
//------------------------------------------------------------------------------------------------
// returns byte at offset 'Addr' in device 'DevAddr'
// if 'TwoByteAddress' !=0 two adress bytes are sent

#endif //def I2C_H
