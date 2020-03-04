#define LCD_BUSY   P3_7
#define LCD_ENABLE P3_4
#define LCD_LINE   P3_1
#define LCD_STROBE P3_0
#define LCD_ADDR   P2
#define LCD_DATA   P0

void delay (int loops)
{
    for (int i = 0; i<loops; i++) {}
}

void display_char_at_line (int addr, int line, int c)
{
    LCD_ADDR = addr;
    LCD_LINE = line;
    LCD_DATA = c;
    while (LCD_BUSY == 1) {
    }
    LCD_STROBE = 1;
    delay(10);
    LCD_STROBE = 0;
}

void display_bar (int length, int line, int c)
{
    for (int i = 0; i < length; i++)
    {
       display_char_at_line(i, line, c);
    }
}

void main (void)
{
    int key;
    int barlength;
    int keydown;
    int on;
    keydown = P1;
    LCD_STROBE = 0;
    LCD_ENABLE = 0;
    while (1)
    {
        P2 = P1 = P0 = 0;
        on = 0;

        while (keydown == 0) {
            keydown = P1;
        }
        key = P0;
        if (key > 15) {
            key = 0;
        }
        on = 1;

        barlength = (16 - key);
        display_bar(barlength, 0, 0xFF);

        while (keydown != 0) {
            P1 = on;
            delay(100 + key*10);
            keydown = P1;
            on = (++on)%2;
        }

        display_bar(16, 0, 32);
        display_bar(16, 1, 32);

        display_bar(barlength, 1, 0xFF);
    }
}
