/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Simple routines to drive the LCD
|*
\*****************************************************************************/

void inline Write_LCD               (unsigned char Data);
void        Write_DR_LCD            (unsigned char Data);
void        InitLCD                 (void);
void        LCD_WriteString         (const char *str);
void        LCD_ClearDisplay        (void);
void        LCD_Set_IconPattern     (int Icon, int Pattern);
void        LCD_GotoXY              (unsigned char X, unsigned char Y);

void        Wait_100us              (int Time);
void        Wait_1ms                (int Time);



