
// Globally accessible LCD Functions
void LCDInit(void);
void LCDClearScreen(void);
void LCDClearLine(_Bool firstLine);
void LCDDisplayCharacter(char characterToDisplay, byte linePosition, _Bool firstLine);
int LCDDisplayMessage(__rom char* message, _Bool firstLine);
int LCDDisplayMessageWithHexDigit(__rom char* message, byte digitToDisplay, _Bool firstLine);

