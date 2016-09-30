
#include "GeneralDefines.h"
#include "Keypad.h"

#define KEYPAD_DATA_PORT P2
#define KEYPAD_CTRL_PORT P2
#define KEYPAD_RESET_ON 0x01
#define KEYPAD_RESET_OFF 0x00

__rom byte keypadMapping[] = {0x01, 0x02, 0x03, 0x0C,
                              0x04, 0x05, 0x06, 0x0D,
                              0x07, 0x08, 0x09, 0x0E,
                              0x0A, 0x00, 0x0B, 0x0F};

void keypadReset(void)
{
    KEYPAD_CTRL_PORT = KEYPAD_RESET_OFF;
    KEYPAD_CTRL_PORT = KEYPAD_RESET_ON;
    KEYPAD_CTRL_PORT = KEYPAD_RESET_OFF;
}

void keypadInit(void)
{
    keypadReset();
}

byte keyScan(void)
{
    byte returnVal = Key_INVALID;
    byte keypadData = KEYPAD_DATA_PORT;
    if(CHECK_BIT(keypadData, Bit4))
    {
       byte mappingIndex = keypadData & 0x0F;
       returnVal = keypadMapping[mappingIndex];
    } else
    {
       returnVal = Key_INVALID;
    }
    keypadReset();
    return(returnVal);
}

