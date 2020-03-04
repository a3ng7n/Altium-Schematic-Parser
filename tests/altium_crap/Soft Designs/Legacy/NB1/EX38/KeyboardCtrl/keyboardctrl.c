#include <stdio.h>
#include <stdlib.h>

#include "ntype.h"
#include "Interrupt0.h"

#define KEYBOARD_OUTPUT P2
#define KEYBOARD_INPUT  P2
#define KEYBOARD_STROBE P3_6
#define KEYBOARD_BUSY   P0_1

#define LCD_BUSY   P0_0
#define LCD_ENABLE P3_4
#define LCD_LINE   P3_7
#define LCD_STROBE P3_5
#define LCD_ADDR   P1
#define LCD_DATA   P0

void shortdelay (void)
{
    for (int i = 0; i<0x0FF; i++) {}
}

void longdelay (void)
{
    for (int i = 0; i<0x00FF; i++) {}
}


void LCDDisplayChar(int p, int l, int c)
{
    LCD_ADDR = p;
    LCD_LINE = l;
    LCD_DATA = c;
    while (LCD_BUSY == 1) {
    }
    LCD_STROBE = 1;
    LCD_STROBE = 0;
}

void LCDDisplayLine(char * str, int start, int len, int line)
{
    for (int i = start; i < (start + len); i++)
    {
       LCDDisplayChar(i, line, str[i-start]);
    }
}

void LCDClear(byte l)
{
    LCDDisplayLine("                ", 0, 16, l);
    LCDDisplayLine("                ", 0, 16, l);
}

void InitLCD()
{
    LCD_STROBE = 0;
    LCD_ENABLE = 0;
}

char KeyboardMapKey(byte key)
{
    switch (key)
    {
       case 0x45 : return '0';
       case 0x69 : return '1';
       case 0x72 : return '2';
       case 0x7A : return '3';
       case 0x6B : return '4';
       case 0x73 : return '5';
       case 0x74 : return '6';
       case 0x6C : return '7';
       case 0x75 : return '8';
       case 0x7D : return '9';


       case 0x15 : return 'Q';
       case 0x1D : return 'W';
       case 0x24 : return 'E';
       case 0x2D : return 'R';
       case 0x2C : return 'T';
       case 0x35 : return 'Y';
       case 0x3C : return 'U';
       case 0x43 : return 'I';
       case 0x44 : return 'O';
       case 0x4D : return 'P';

       case 0x1C : return 'A';
       case 0x1B : return 'S';
       case 0x23 : return 'D';
       case 0x2B : return 'F';
       case 0x34 : return 'G';
       case 0x33 : return 'H';
       case 0x3B : return 'J';
       case 0x42 : return 'K';
       case 0x4B : return 'L';

       case 0x1A : return 'Z';
       case 0x22 : return 'X';
       case 0x21 : return 'C';
       case 0x2A : return 'V';
       case 0x32 : return 'B';
       case 0x31 : return 'N';
       case 0x3A : return 'M';
       case 0x29 : return ' ';
    }
    return '-';
}

byte keyboard_sent_init_sequence;
byte keyboard_ignore_next_key;
byte keyboard_caps_on;
byte keyboard_caps_sent;
byte char_written    = 0;

byte key      = 0;
byte key_old1 = 0;
byte key_old2 = 0;

char digits[16] = {'0',
                   '1',
                   '2',
                   '3',
                   '4',
                   '5',
                   '6',
                   '7',
                   '8',
                   '9',
                   'A',
                   'B',
                   'C',
                   'D',
                   'E',
                   'F'};

void int_to_hex(byte val, char* res)
{
    byte top;
    byte bottom;

    top    = val & 0xF0;
    top    = top >> 4;
    bottom = val & 0x0F;
    res[0] = '0';
    res[1] = 'x';
    res[2] = digits[top];
    res[3] = digits[bottom];
    res[4] = '\0';
}

char lower_case(char c)
{
    byte byte_char;

    byte_char = c;
    if ((byte_char >= 65) && (byte_char <= 90))
    {
       return (byte_char + 32);
    }
    return c;
}

void KeyboardInit()
{
    keyboard_sent_init_sequence  = 0;
    keyboard_ignore_next_key     = 0;
    keyboard_caps_on             = 0;
    keyboard_caps_sent           = 0;

    KEYBOARD_INPUT  = 0xFF;
    KEYBOARD_STROBE = 0;
    KEYBOARD_STROBE = 1;
    KEYBOARD_STROBE = 0;
}

void KeyboardToggleCAPS()
{
    KEYBOARD_INPUT = 0xED;
    KEYBOARD_STROBE = 0;
    KEYBOARD_STROBE = 1;
    KEYBOARD_STROBE = 0;
    if (keyboard_caps_on == 1)
    {
       KEYBOARD_INPUT = 0x00;
       keyboard_caps_on = 0;
    }
    else
    {
       KEYBOARD_INPUT = 0x04;
       keyboard_caps_on = 1;
    }
    KEYBOARD_STROBE = 0;
    KEYBOARD_STROBE = 1;
    KEYBOARD_STROBE = 0;
    keyboard_caps_sent = 0;
}

byte lcd_current_pos;

void Interrupt0(void)
{
    DISABLE_INTERRUPTS();
    key = KEYBOARD_OUTPUT;
    if (key == 0xAA)
       keyboard_sent_init_sequence = 1;
    if ((key == 0x58) && (keyboard_ignore_next_key == 0))
       keyboard_caps_sent = 1;
    else if (key == 0xF0)
       keyboard_ignore_next_key = 1;
    else
    {
       if (keyboard_ignore_next_key == 0)
           char_written = 0;
       keyboard_ignore_next_key = 0;
    }

    ENABLE_INTERRUPTS();
}

void main(void)
{
    byte lcd_current_pos = 0;
    byte lcd_cleared     = 1;

    char keystr[5];
    char displaychar;



    InitInterrupt0();
    InitLCD();
    LCDClear(0);
    LCDClear(1);
    LCDDisplayLine("Init KBRD...", 0, 12, 0);
    ENABLE_INTERRUPTS();
    KeyboardInit();

    while (keyboard_sent_init_sequence == 0)
    {
    }

    DISABLE_INTERRUPTS();
    LCDDisplayLine("KBRD Initialized",0, 16, 0);
    ENABLE_INTERRUPTS();

    while (1)
    {
       if (keyboard_caps_sent == 1)
       {
          KeyboardToggleCAPS();
          keyboard_caps_sent = 0;
       }
       else
       {
          if (char_written == 0)
          {
             DISABLE_INTERRUPTS();
             if ((lcd_current_pos == 0) && (lcd_cleared != 0))
             {
                LCDClear(0);
                lcd_cleared = 1;
             }
             displaychar = KeyboardMapKey(key);
             if (displaychar != '-')
             {
                 if (keyboard_caps_on == 0)
                 {
                     displaychar = lower_case(displaychar);
                 }
                 LCDDisplayChar(lcd_current_pos, 0, displaychar);
                 lcd_current_pos++;
                 lcd_current_pos= lcd_current_pos%16;
             }

             int_to_hex(key, keystr);
             LCDDisplayLine(keystr, 12, 4, 1);

             int_to_hex(key_old1, keystr);
             LCDDisplayLine(keystr, 6, 4, 1);

             int_to_hex(key_old2, keystr);
             LCDDisplayLine(keystr, 0, 4, 1);

             key_old2 = key_old1;
             key_old1 = key;

             char_written = 1;
             ENABLE_INTERRUPTS();
          }
       }
    }
}
