
#include "GeneralDefines.h"
#include "LCD.h"
#include <stdio.h>

#define LCD_CTRL_PORT P3
#define LCD_DATA_PORT P0
#define LCD_DEFAULT 0xFF
#define LCD_FIRST_LINE 0xEF
#define LCD_SECOND_LINE 0x10
#define LCD_STROBE_ON 0xDF
#define LCD_STROBE_OFF 0x20

#define MAX_LINE_LENGTH 16

#pragma romstring
__rom char hexToASCIIMap[] = "0123456789ABCDEF";
#pragma ramstring


// Prototypes for private functions
void LCDWaitTillReady(void);
char mapHexNibbleToASCII(byte hexNibble);


void LCDInit(void)
{
    LCDClearScreen();
}

void LCDClearScreen(void)
{
    LCDClearLine(TRUE);
    LCDClearLine(FALSE);
}

int LCDDisplayMessage(__rom char* message, _Bool firstLine)
{
    LCDClearLine(firstLine);
    byte linePosition;
    for(linePosition=0;
        (linePosition < MAX_LINE_LENGTH) && (message[linePosition] != '\0');
        linePosition++)
    {
       LCDDisplayCharacter(message[linePosition], linePosition, firstLine);
    }
    return(linePosition);
}

char mapHexNibbleToASCII(byte hexNibble)
{
    return(hexToASCIIMap[hexNibble & 0x0F]);
}

int LCDDisplayMessageWithHexDigit(__rom char* message, byte digitToDisplay, _Bool firstLine)
{
    int linePosition = LCDDisplayMessage(message, firstLine);
    LCDDisplayCharacter('0', linePosition, firstLine);
    linePosition++;
    LCDDisplayCharacter('x', linePosition, firstLine);
    linePosition++;
    LCDDisplayCharacter(mapHexNibbleToASCII(digitToDisplay >> 4), linePosition, firstLine); // upper nibble
    linePosition++;
    LCDDisplayCharacter(mapHexNibbleToASCII(digitToDisplay), linePosition, firstLine); // lower nibble
    linePosition++;
    return(linePosition);
}

void LCDClearLine(_Bool firstLine)
{
    for(byte linePosition=0; linePosition < MAX_LINE_LENGTH; linePosition++)
    {
       LCDDisplayCharacter(' ', linePosition, firstLine);
    }
}

void LCDDisplayCharacter(char characterToDisplay, byte linePosition, _Bool firstLine)
{
    LCDWaitTillReady();
    LCD_DATA_PORT = characterToDisplay;

    byte command = 0x00;
    command |= linePosition; // set the line position
    if(firstLine)
    {
       command &= LCD_FIRST_LINE;
    } else
    {
       command |= LCD_SECOND_LINE;
    }
    command |= LCD_STROBE_OFF;
    LCD_CTRL_PORT = command;

    command &= LCD_STROBE_ON;
    LCD_CTRL_PORT = command;

    LCD_CTRL_PORT = LCD_DEFAULT;
}

//...............................................................................
// LCDWaitTillReady
// Won't return until the LCD busy signal indicates the LCD
// is ready for more data.
//...............................................................................
void LCDWaitTillReady(void)
{
    byte state = LCD_CTRL_PORT;
    while (!CHECK_BIT(state, Bit0))
    {
       state = LCD_CTRL_PORT;
    }
}
