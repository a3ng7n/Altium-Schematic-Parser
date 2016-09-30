#define SWITCH       P2

#define KEYPAD_RST   P2_2
#define KEYPAD_VALID P1_1
#define KEYPAD_CHAR  P0

#define LCD_STROBE P2_0
#define LCD_LINE   P2_1
#define LCD_ENABLE P2_4
#define LCD_BUSY   P1_0
#define LCD_ADDR   P1
#define LCD_CHAR   P0


static const char lcdchar[16] =  {'1', '2', '3', 'C', '4', '5', '6', 'D', '7', '8', '9', 'E', 'A', '0', 'B', 'F'};
static const char banner[16] =  {'A', 'l', 't', 'i', 'u', 'm', ' ', 'N', 'a', 'n', 'o', 'B', 'o', 'a', 'r', 'd'};     

void delay ()
{
    int multi = 0x0008;
    int sw = SWITCH;
    for (int i = 0; i < sw*multi; i++) {}     //read dipswitches and calculate delay
}

void ResetKeypadScanner ()
{
    KEYPAD_RST = 0;
    KEYPAD_RST = 1;                                 // assert the keypad scanner reset line
    KEYPAD_RST = 0;                                 // un-assert the keypad scanner reset line
}

void WriteChar (int line, int addr, int character)
{
    LCD_STROBE = 0;                                 // disable the Strobe line on the LCD controller
    LCD_LINE = line;                              // set the line on the LCD
    LCD_ADDR = addr;                                // set the address
    LCD_CHAR = character;                           // write the character
    while (LCD_BUSY == 1) {}                      // wait till the LCD is not busy...
    LCD_STROBE = 1;                                 // enable Strobe to load the character
}

void WriteLine(int line, char* str, int len)
{
    for (int i = 0; i < len; i++)
    {
       WriteChar(line, i, str[i]);
    }
}

void BlankLine (int line)
{
    int addr = 0;
    int blank = ' ';
    for (int i = 0; i<16; i++)
    {
       addr = i;
       WriteChar (line, addr, blank);
    }
}

void WriteBanner (int line)
{
    for (int i = 0; i<16; i++)
    {
        WriteChar (line, i, banner[i]);
    }
}

void ScrollBanner (int line)
{
    int ind = 0;
    int addr = 0;
    int character = 0;
    
    for (int i = 0; i<16; i++)
    {
       for (int j = 0; j<16; j++)
       {
          ind = j+i-15;
          if (ind >= 0)
          {
             addr = j;                        // set the character address
             character = banner[ind];
             WriteChar (line, addr, character);
          }
       delay();                               // mark time before scolling and writing the next char
       }
    }
}

void main ()
{
    int keyvalue = 0;
    int key = 0;
    int line = 0;
    int addr = 15;

    LCD_STROBE = 0;                           // ensure that the LCD strobe is low
    LCD_ENABLE = 0;                           // enable speaking to the LCD

    ResetKeypadScanner ();
    BlankLine(0);
    BlankLine(1);

    ScrollBanner (line = 0);                  // scroll the banner message on the LCD top line
    WriteLine(1, "Key Pressed:", 12);

    while (1)
    {
       while (KEYPAD_VALID == 0) {}                   // wait till VALIDKEY indicates a key has been pressed
       keyvalue = lcdchar[KEYPAD_CHAR];                // read the key and determine the ASCII char set value
       ResetKeypadScanner ();
       WriteChar (line = 1, addr, keyvalue);       // display the key value on the bottom line
    }
}




