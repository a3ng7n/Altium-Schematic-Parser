/*****************************************************************************
 *
 *  VERSION:    %W% %E%
 *
 *      IN PACKAGE: TASKING Peripheral Library for 80515
 *
 *      AUTHORS:        SAPA
 *
 *      COPYRIGHT:  Copyright (c) 2003, Altium BV
 *
 *      DESCRIPTION:    Include file for alphanumeric lcd module
 *
 ****************************************************************************/

#ifndef _LCD_H
#define _LCD_H

/* Undefine all */
#undef     ALPHALCD_WIDTH
#undef     ALPHALCD_HEIGHT
#undef     ALPHALCD_CONFIG_1X16_AS_2_LINE
#undef     ALPHALCD_VFD_MODULE
#undef     ALPHALCD_HANDLE_NEWLINE
#undef     ALPHALCD_GOTOXY_CLIP
#undef     ALPHALCD_4BITBUS
#undef     ALPHALCD_BASEADDR
#undef     ALPHALCD_BITBANG
#undef     ALPHALCD_BITBANG_DB
#undef     ALPHALCD_BITBANG_E
#undef     ALPHALCD_BITBANG_RS
#undef     ALPHALCD_BITBANG_RW
#undef     ALPHALCD_USERCHAR_0
#undef     ALPHALCD_USERCHAR_1
#undef     ALPHALCD_USERCHAR_2
#undef     ALPHALCD_USERCHAR_3
#undef     ALPHALCD_USERCHAR_4
#undef     ALPHALCD_USERCHAR_5
#undef     ALPHALCD_USERCHAR_6
#undef     ALPHALCD_USERCHAR_7


/* Configuration */
#define    ALPHALCD_WIDTH       16
#define    ALPHALCD_HEIGHT      2
//#define  ALPHALCD_CONFIG_1X16_AS_2_LINE
//#define  ALPHALCD_VFD_MODULE
#define    ALPHALCD_HANDLE_NEWLINE
//#define  ALPHALCD_GOTOXY_CLIP
//#define  ALPHALCD_4BITBUS
#define    ALPHALCD_BASEADDR        0xfffc
//#define    ALPHALCD_BITBANG
//#define    ALPHALCD_BITBANG_DB      P0
//#define    ALPHALCD_BITBANG_E       P1_3
//#define    ALPHALCD_BITBANG_RS      P1_4
//#define    ALPHALCD_BITBANG_RW      P1_5
#define    ALPHALCD_USERCHAR_0      0x18, 0x18, 0x03, 0x04, 0x04, 0x04, 0x03, 0x00
#define    ALPHALCD_USERCHAR_1      0x18, 0x18, 0x07, 0x04, 0x06, 0x04, 0x04, 0x00
#define    ALPHALCD_USERCHAR_2      0x06, 0x04, 0x06, 0x04, 0x06, 0x04, 0x0E, 0x0E
#define    ALPHALCD_USERCHAR_3      0x01, 0x03, 0x1F, 0x08, 0x08, 0x08, 0x08, 0x08
//#define  ALPHALCD_USERCHAR_4
//#define  ALPHALCD_USERCHAR_5
//#define  ALPHALCD_USERCHAR_6
//#define  ALPHALCD_USERCHAR_7

#define BOC_BOARD   // i.v.m rare mapping van de LCD module

/*
 * Perform some checking first
 */
#if (!defined ALPHALCD_WIDTH) || (ALPHALCD_WIDTH == 0)
#error > Specify valid display width
#endif

#if (!defined ALPHALCD_HEIGHT) || (ALPHALCD_HEIGHT == 0) || (ALPHALCD_HEIGHT == 3)
#error > Specify valid display height
#endif

#if (ALPHALCD_HEIGHT == 4) && (ALPHALCD_WIDTH > 20)
#error > Sorry, displays that are 4 line high are supported up to 20 characters wide.
#endif


#ifdef ALPHALCD_BITBANG
#ifndef ALPHALCD_BITBANG_DB
#error > Specify port for LCD databus.
#endif
#ifndef ALPHALCD_BITBANG_RS
#error > Specify portpin for LCD RS pin.
#endif
#ifndef ALPHALCD_BITBANG_RW
#error > Specify portpin for LCD RW pin.
#endif
#ifndef ALPHALCD_BITBANG_E
#error > Specify portpin for LCD E pin.
#endif
#else
#ifndef ALPHALCD_BASEADDR
#error > Specify baseaddress
#endif
#endif

#define DISPLAY_POWER_ON    0x04
#define DISPLAY_POWER_OFF   ~DISPLAY_POWER_ON

#define CURSOR_TYPE_OFF     0x00
#define CURSOR_TYPE_BLOCK   0x01
#define CURSOR_TYPE_UNDERSCORE  0x02
#define CURSOR_TYPE_BOTH    (CURSOR_TYPE_BLOCK | CURSOR_TYPE_UNDERSCORE)


typedef struct
{
    unsigned char x;
    unsigned char y;
} lcd_cursorpos_t;



/*
 * Backwards compatibility with pre-Viper Technology compilers
 */
#ifndef __C51__
#define __at    _at
#define __rom   _rom
#define __xdata _xdat
#endif

#ifdef __CZ80__
#define __rom
#define __xdata
#endif

/* Prototypes public functions */

void lcd_init ( void );

void lcd_showcursor ( unsigned char type );
void lcd_hidecursor ( void );
void lcd_display_on ( void );
void lcd_display_off ( void );
void lcd_clear_screen ( void );

void lcd_putc ( unsigned char character );

void lcd_gotoxy ( unsigned char x, unsigned char y );
lcd_cursorpos_t lcd_getxy ( void );

void lcd_create_char ( unsigned char character, __rom unsigned char * pattern );

#endif
