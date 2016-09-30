
#include <stdio.h>
#include "GeneralDefines.h"
#include "CAN_Driver.h"

#define MODE_REGISTER             0x00
#define COMMAND_REGISTER          0x01
#define STATUS_REGISTER           0x02
#define INTERRUPT_REGISTER        0x03
#define INTERRUPT_ENABLE_REGISTER 0x04
#define BUS_TIMING_REGISTER_0     0x06
#define BUS_TIMING_REGISTER_1     0x07
#define OUTPUT_CONTROL_REGISTER   0x08

#define ACCEPTANCE_CODE0_REGISTER   0x10
#define ACCEPTANCE_CODE1_REGISTER   0x11
#define ACCEPTANCE_CODE2_REGISTER   0x12
#define ACCEPTANCE_CODE3_REGISTER   0x13

#define ACCEPTANCE_MASK0_REGISTER   0x14
#define ACCEPTANCE_MASK1_REGISTER   0x15
#define ACCEPTANCE_MASK2_REGISTER   0x16
#define ACCEPTANCE_MASK3_REGISTER   0x17
#define CLOCK_DIVIDER_REGISTER      0x1F
#define TRANSMIT_BUFFER_REGISTER_0  0x10
#define TRANSMIT_BUFFER_REGISTER_1  0x11
#define TRANSMIT_BUFFER_REGISTER_2  0x12
#define TRANSMIT_BUFFER_REGISTER_3  0x13
#define TRANSMIT_BUFFER_REGISTER_4  0x14
#define TRANSMIT_BUFFER_REGISTER_5  0x15
#define TRANSMIT_BUFFER_REGISTER_6  0x16
#define TRANSMIT_BUFFER_REGISTER_7  0x17
#define TRANSMIT_BUFFER_REGISTER_8  0x18
#define TRANSMIT_BUFFER_REGISTER_9  0x19
#define TRANSMIT_BUFFER_REGISTER_10 0x1A

#define RECEIVE_BUFFER_REGISTER_0   0x10
#define RECEIVE_BUFFER_REGISTER_1   0x11
#define RECEIVE_BUFFER_REGISTER_2   0x12
#define RECEIVE_BUFFER_REGISTER_3   0x13
#define RECEIVE_BUFFER_REGISTER_4   0x14
#define RECEIVE_BUFFER_REGISTER_5   0x15
#define RECEIVE_BUFFER_REGISTER_6   0x16
#define RECEIVE_BUFFER_REGISTER_7   0x17
#define RECEIVE_BUFFER_REGISTER_8   0x18
#define RECEIVE_BUFFER_REGISTER_9   0x19
#define RECEIVE_BUFFER_REGISTER_10  0x1A


#define ACCEPT_NODE_ID_FILTER 0xE0
#define ACCEPT_ALL_FILTER 0xFF

// private functions
void setCANRegister(byte registerAddress, byte newValue);
byte getCANRegister(byte registerAddress);

// private variables
ReceivedMessageCallback receivedMessageCallback = NULL;
SNPData rxData;


void CANInit(byte nodeID)
{
    do
    {
        setCANRegister(MODE_REGISTER, 0x01);
    } while(!CHECK_BIT(getCANRegister(MODE_REGISTER), Bit0));

    setCANRegister(CLOCK_DIVIDER_REGISTER, 0xC0);
    setCANRegister(BUS_TIMING_REGISTER_0, 0x09); // 40 MHz 125kb/s
    setCANRegister(BUS_TIMING_REGISTER_1, 0x1C); // 40 MHz 125kb/s
    setCANRegister(OUTPUT_CONTROL_REGISTER, 0x1A);
    setCANRegister(CLOCK_DIVIDER_REGISTER, 0x8F);

    // set first filter to accept messages for this NodeID only
    byte safeNodeID = nodeID & NODE_ID_MASK;
    setCANRegister(ACCEPTANCE_CODE0_REGISTER, safeNodeID);
    setCANRegister(ACCEPTANCE_MASK0_REGISTER, ACCEPT_NODE_ID_FILTER);
    setCANRegister(ACCEPTANCE_MASK1_REGISTER, ACCEPT_ALL_FILTER);

    // Set second filter to accept Broadcast Messages
    setCANRegister(ACCEPTANCE_CODE2_REGISTER, 0x00);
    setCANRegister(ACCEPTANCE_MASK2_REGISTER, ACCEPT_NODE_ID_FILTER);
    setCANRegister(ACCEPTANCE_MASK3_REGISTER, ACCEPT_ALL_FILTER);

    setCANRegister(INTERRUPT_ENABLE_REGISTER, 0x01);

    do
    {
        setCANRegister(MODE_REGISTER, 0x00);
    } while(CHECK_BIT(getCANRegister(MODE_REGISTER), Bit0));
}

void transmitData(SNPData* txData)
{
    while(!CHECK_BIT(getCANRegister(STATUS_REGISTER), Bit2)); // loop until the Transmit Buffer status bit is set
    if(txData->dataLength > SNP_MAX_DATA_BYTES)
       txData->dataLength = SNP_MAX_DATA_BYTES;

    setCANRegister(TRANSMIT_BUFFER_REGISTER_0, txData->dataLength);
    setCANRegister(TRANSMIT_BUFFER_REGISTER_1, txData->messageID);
    setCANRegister(TRANSMIT_BUFFER_REGISTER_2, 0x00);

    for(byte i=0, txBufReg = TRANSMIT_BUFFER_REGISTER_3; i < txData->dataLength; i++, txBufReg++)
    {
       setCANRegister(txBufReg, txData->data[i]);
    }

    do
    {
       // Setting Bit 0 in the Command register tells the CAN to
       // transmit the contents of its transmit buffer
       setCANRegister(COMMAND_REGISTER, 0x01);
    } while(CHECK_BIT(getCANRegister(STATUS_REGISTER), Bit3)); // check Transmission Complete bit
}

_Bool receiveData(void)
{
    _Bool dataReceived = FALSE;

    while(CHECK_BIT(getCANRegister(STATUS_REGISTER), Bit0)) // (bit 0 == 1) -> unread data in Rx FIFO
    {
       dataReceived = TRUE;

       rxData.messageID = getCANRegister(RECEIVE_BUFFER_REGISTER_1);
       rxData.dataLength = getCANRegister(RECEIVE_BUFFER_REGISTER_0);
       if(rxData.dataLength > SNP_MAX_DATA_BYTES)
          rxData.dataLength = SNP_MAX_DATA_BYTES;

       for(byte i=0, rxBufReg = RECEIVE_BUFFER_REGISTER_3; i < rxData.dataLength; i++, rxBufReg++)
       {
          rxData.data[i] = getCANRegister(rxBufReg);
       }

       // release receive buffer
       setCANRegister(COMMAND_REGISTER, 0x04);

       if(receivedMessageCallback != NULL)
           receivedMessageCallback(&rxData);
    }
    return(dataReceived);
}

void setReceivedMessageCallback(ReceivedMessageCallback newReceivedMessageCallback)
{
    receivedMessageCallback = newReceivedMessageCallback;
}

void setCANRegister(byte registerAddress, byte newValue)
{
    __xdata byte* CANRegister = registerAddress;
    *CANRegister = newValue;
}

byte getCANRegister(byte registerAddress)
{
    __xdata byte* CANRegister = registerAddress;
    return(*CANRegister);
}

