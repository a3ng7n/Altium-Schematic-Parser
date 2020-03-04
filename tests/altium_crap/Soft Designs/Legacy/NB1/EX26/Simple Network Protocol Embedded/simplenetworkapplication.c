

#include <string.h>
#include "GeneralDefines.h"
#include "LCD.h"
#include "Keypad.h"
#include "SimpleNetworkProtocol.h"
#include "CAN_Driver.h"

#define NODE_ID_PORT P0
#define NV01_LEDS_PORT P1
#define NV02_SENSOR_PORT P1

#define LEDS_NVA 0x01
#define SENSOR_NVA 0x02


#pragma romstring
__rom char nodeIDMessage[] = "Node ID: ";
__rom char getTargetMessage[] = "GET Target: ";
__rom char setTargetMessage[] = "SET Target: ";
__rom char receiveMessage[] = "Received: ";
__rom char getAttemptMessage[] = "Getting...";
#pragma ramstring


_Bool getSetMode = TRUE; // SET mode = TRUE, GET mode = FALSE
byte transmitTarget = (byte)0x00;

// Private function prototypes
void processKeypress(byte keypress);

// Callback prototypes
void setNetworkVariableCallback(byte networkVariableAddress, byte networkVariableValue);
byte getNetworkVariableCallback(byte networkVariableAddress);
void networkVariableValueCallback(byte networkVariableAddress, byte networkVariableValue);
void displayGetSetMessage(void);


void init(void)
{
    NV01_LEDS_PORT = 0x00; // initialise LEDS

    LCDInit();
    keypadInit();

    setNodeID(NODE_ID_PORT);
    LCDDisplayMessageWithHexDigit(nodeIDMessage, getNodeID(), TRUE);

    setSetNetworkVariableCallback(setNetworkVariableCallback);
    setGetNetworkVariableCallback(getNetworkVariableCallback);
    setNetworkVariableValueCallback(networkVariableValueCallback);
    networkInit();
}

#define DELAY_LOOP_ITERATIONS 0xFF00
void delay(void)
{
    for(int i=0; i < 2; i++)
    {
       for(int j=0; j < DELAY_LOOP_ITERATIONS; j++)
       {
          __asm("NOP");
       }
    }
}

void main(void)
{
    init();
    while(TRUE)
    {
       delay();
       receiveData();
       byte keypress = keyScan();
       if(keypress != Key_INVALID)
       {
          processKeypress(keypress);
       }
    }
}

void processKeypress(byte keypress)
{
    if(keypress != Key_INVALID)
    {
       if(keypress <= Key_9)
       {
          transmitTarget = keypress;
          displayGetSetMessage();
       } else if(keypress == Key_A)
       {
          // toggle get/set mode
          getSetMode = !getSetMode;
          displayGetSetMessage();
       } else
       {
          if(getSetMode)
          {
             setNetworkRegister(transmitTarget, LEDS_NVA, keypress);
          } else
          {
             getNetworkRegister(transmitTarget, SENSOR_NVA);
             LCDDisplayMessage(getAttemptMessage, FALSE);
          }
       }
    }
}

void displayGetSetMessage(void)
{
   if(getSetMode)
      LCDDisplayMessageWithHexDigit(setTargetMessage, transmitTarget, TRUE);
   else
      LCDDisplayMessageWithHexDigit(getTargetMessage, transmitTarget, TRUE);
}

void setNetworkVariableCallback(byte networkVariableAddress, byte networkVariableValue)
{
    switch(networkVariableAddress)
    {
       case LEDS_NVA:
           NV01_LEDS_PORT = networkVariableValue;
       break;
    }
}

byte getNetworkVariableCallback(byte networkVariableAddress)
{
    byte returnValue = (byte)0;
    switch(networkVariableAddress)
    {
       case SENSOR_NVA:
          returnValue = NV02_SENSOR_PORT;
       break;
    }
    return(returnValue);
}

void networkVariableValueCallback(byte networkVariableAddress, byte networkVariableValue)
{
    switch(networkVariableAddress)
    {
       case SENSOR_NVA:
          LCDDisplayMessageWithHexDigit(receiveMessage, networkVariableValue, FALSE);
       break;
    }
}

