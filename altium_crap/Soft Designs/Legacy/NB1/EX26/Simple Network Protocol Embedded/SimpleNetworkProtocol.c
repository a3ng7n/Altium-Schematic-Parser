//...............................................................................
// Simple Network Protocol code.
//
// NOTE: This unit implements the CAN Driver callback 'ReceivedMessageCallback'.
//
// Callbacks: To include this unit in your own project, you also need
// to implement a number of callback functions in your application layer.
// The client of this API needs to provide implementations and to set the
// callbacks before the system will run properly. They are called in various
// circumstances such as when a network variable has just been set, or when
// a request for a network variable value has been received since it is up
// to the application layer to interpret and supply these values.
//
// SetNetworkVariableCallback:
//     Called whenever a "Set Network Variable" message is received over the network.
//     The networkVariableAddress & networkVariableValue parameters will contain the
//     correct values before the call is made. The application uses these values in
//     its implementation of this callback.
//
// GetNetworkVariableCallback:
//     Called whenever a "Get Network Variable" message is received over the network.
//     The networkVariableAddress parameter will contain the address the sender is
//     requesting the value for. The application level should return the value for
//     the requested address. If this parameter value is not supported, it would
//     be best for the implementation of this function to set the value to '0'.
//
// NetworkVariableValueCallback:
//     Called whenever a "Network Variable Value" message is received over the network.
//     The networkVariableAddress and networkVariableValue will contain the data just
//     received. These messages are sent in response to this node previously sending
//     a "Get Network Variable" message.
//...............................................................................


#include <stdio.h>
#include "GeneralDefines.h"
#include "SimpleNetworkProtocol.h"
#include "CAN_Driver.h"

#define SET_NW_VARIABLE_MSG_LENGTH (byte)2
#define GET_NW_VARIABLE_MSG_LENGTH (byte)2
#define NW_VARIABLE_VALUE_MSG_LENGTH (byte)2

#define NETWORK_VARIABLE_ADDRESS_OFFSET (byte)0
#define NETWORK_VARIABLE_VALUE_OFFSET (byte)1
#define TARGET_NODE_OFFSET (byte)1

#define FCT_CODE_SET_VARIABLE (byte)0
#define FCT_CODE_GET_VARIABLE (byte)1
#define FCT_CODE_VARIABLE_VALUE (byte)3

#define MESSAGE_CODE_MASK 0x07

// Private functions
byte createMessageID(byte messageCode, byte targetNodeID);
byte getMessageCode(byte messageID);
void processReceivedMessage(SNPData* rxData);

// Private variables
byte nodeID = (byte)0x00;
SNPData txData;
SetNetworkVariableCallback setNetworkVariable = NULL;
GetNetworkVariableCallback getNetworkVariable = NULL;
NetworkVariableValueCallback networkVariableValue = NULL;


void networkInit(void)
{
    setReceivedMessageCallback(processReceivedMessage);
    CANInit(nodeID);
}

void setNetworkRegister(byte targetNodeID, byte networkVariableAddress, byte networkVariableValue)
{
    txData.dataLength = SET_NW_VARIABLE_MSG_LENGTH;
    txData.messageID = createMessageID(FCT_CODE_SET_VARIABLE, targetNodeID);
    txData.data[NETWORK_VARIABLE_ADDRESS_OFFSET] = networkVariableAddress;
    txData.data[NETWORK_VARIABLE_VALUE_OFFSET] = networkVariableValue;
    transmitData(&txData);
}

void getNetworkRegister(byte targetNodeID, byte networkVariableAddress)
{
    txData.dataLength = GET_NW_VARIABLE_MSG_LENGTH;
    txData.messageID = createMessageID(FCT_CODE_GET_VARIABLE, targetNodeID);
    txData.data[NETWORK_VARIABLE_ADDRESS_OFFSET] = networkVariableAddress;
    txData.data[TARGET_NODE_OFFSET] = nodeID;
    transmitData(&txData);
}

void sendNetworkVariableValue(byte targetNodeID, byte networkVariableAddress, byte networkVariableValue)
{
    txData.dataLength = NW_VARIABLE_VALUE_MSG_LENGTH;
    txData.messageID = createMessageID(FCT_CODE_VARIABLE_VALUE, targetNodeID);
    txData.data[NETWORK_VARIABLE_ADDRESS_OFFSET] = networkVariableAddress;
    txData.data[NETWORK_VARIABLE_VALUE_OFFSET] = networkVariableValue;
    transmitData(&txData);
}

void processReceivedMessage(SNPData* rxData)
{
    switch(getMessageCode(rxData->messageID))
    {
       case FCT_CODE_SET_VARIABLE:
           if(setNetworkVariable != NULL)
              setNetworkVariable(rxData->data[NETWORK_VARIABLE_ADDRESS_OFFSET], rxData->data[NETWORK_VARIABLE_VALUE_OFFSET]);
       break;
       case FCT_CODE_GET_VARIABLE:
       {
           byte nvAddress = rxData->data[NETWORK_VARIABLE_ADDRESS_OFFSET];
           byte nvValue = 0;
           if(getNetworkVariable != NULL)
              nvValue = getNetworkVariable(nvAddress);
           sendNetworkVariableValue(rxData->data[TARGET_NODE_OFFSET], nvAddress, nvValue);
       }
       break;
       case FCT_CODE_VARIABLE_VALUE:
           if(networkVariableValue != NULL)
              networkVariableValue(rxData->data[NETWORK_VARIABLE_ADDRESS_OFFSET], rxData->data[NETWORK_VARIABLE_VALUE_OFFSET]);
       break;
    }
}

void setNodeID(byte newNodeID)
{
    nodeID = newNodeID & NODE_ID_MASK; // make sure we only store valid values
}

byte getNodeID(void)
{
    return(nodeID);
}

void setSetNetworkVariableCallback(SetNetworkVariableCallback newSetNetworkVariableCallback)
{
    setNetworkVariable = newSetNetworkVariableCallback;
}

void setGetNetworkVariableCallback(GetNetworkVariableCallback newGetNetworkVariableCallback)
{
    getNetworkVariable = newGetNetworkVariableCallback;
}

void setNetworkVariableValueCallback(NetworkVariableValueCallback newNetworkVariableValueCallback)
{
    networkVariableValue = newNetworkVariableValueCallback;
}

byte createMessageID(byte messageCode, byte targetNodeID)
{
    byte messageID = messageCode & MESSAGE_CODE_MASK;
    messageID <<= NODE_ID_BIT_LENGTH; // make the message code the 3 MS bits
    messageID |= (targetNodeID & NODE_ID_MASK); // and make the target node ID the bottom 5 bits
    return(messageID);
}

byte getMessageCode(byte messageID)
{
    byte messageCode = messageID >> NODE_ID_BIT_LENGTH;
    messageCode &= MESSAGE_CODE_MASK;
    return(messageCode);
}
