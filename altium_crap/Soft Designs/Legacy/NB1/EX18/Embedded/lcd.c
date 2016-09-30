/*****************************************************************************
 *
 *  VERSION:    1.1
 *
 *  COPYRIGHT:  Copyright (c) 2003, Altium BV
 *
 *  DESCRIPTION:    Lcd module. Suitable for dot matrix controllers:
 *          - Samsung KS0066U
 *          - Hyundai HD44780A00
 *          - Noritake-Itron VFD controller
 *          - other compatible controllers
 *
 ****************************************************************************/

#include "lcd.h"

/*
 * Fixed mapped 'data' in external data space. These 'variables' are in fact
 * registers of the LCD module, that are accessed through the external bus.
 */
#ifndef ALPHALCD_BITBANG
#  define ALPHALCD_WRITE_COMMAND          (*(__xdata volatile unsigned char *) (ALPHALCD_BASEADDR + 0))
#  define ALPHALCD_WRITE_DATA             (*(__xdata volatile unsigned char *) (ALPHALCD_BASEADDR + 1))
#  ifdef BOC_BOARD
#    define ALPHALCD_READ_STATUS            (*(__xdata volatile unsigned char *) (ALPHALCD_BASEADDR + 2))
#    define ALPHALCD_READ_DATA              (*(__xdata volatile unsigned char *) (ALPHALCD_BASEADDR + 3))
#  else
#    define ALPHALCD_READ_STATUS        ALPHALCD_WRITE_COMMAND
#    define ALPHALCD_READ_DATA      ALPHALCD_WRITE_DATA
#  endif
#endif


/* Declare user defined characters.
 * They will be loaded into the LCD module during lcd_init.
 */
#ifdef ALPHALCD_USERCHAR_0
__rom unsigned char char0_pattern[] = { ALPHALCD_USERCHAR_0 };
#endif
#ifdef ALPHALCD_USERCHAR_1
__rom unsigned char char1_pattern[] = { ALPHALCD_USERCHAR_1 };
#endif
#ifdef ALPHALCD_USERCHAR_2
__rom unsigned char char2_pattern[] = { ALPHALCD_USERCHAR_2 };
#endif
#ifdef ALPHALCD_USERCHAR_3
__rom unsigned char char3_pattern[] = { ALPHALCD_USERCHAR_3 };
#endif
#ifdef ALPHALCD_USERCHAR_4
__rom unsigned char char4_pattern[] = { ALPHALCD_USERCHAR_4 };
#endif
#ifdef ALPHALCD_USERCHAR_5
__rom unsigned char char5_pattern[] = { ALPHALCD_USERCHAR_5 };
#endif
#ifdef ALPHALCD_USERCHAR_6
__rom unsigned char char6_pattern[] = { ALPHALCD_USERCHAR_6 };
#endif
#ifdef ALPHALCD_USERCHAR_7
__rom unsigned char char7_pattern[] = { ALPHALCD_USERCHAR_7 };
#endif



/*
 * Static data.
 */
static unsigned char        displaypower_cursortype_mode    = CURSOR_TYPE_OFF;
#define CURSOR_TYPE_MASK    0x03



/*
 * Static functions (some may be omitted if not required)
 */
static void lcd_busydelay ( void );
static unsigned char lcd_read_status ( void );
static void lcd_write_data ( unsigned char byte );
static void lcd_write_exec ( unsigned char command );
#ifdef ALPHALCD_BITBANG
static void lcd_bitbang_write ( unsigned char is_data , unsigned char byte );
#endif
#if ((defined ALPHALCD_BITBANG) && (defined ALPHALCD_4BITBUS))
static void lcd_bitbang_write_nibble ( unsigned char nibble );
#endif



/*****************************************************************************
 *
 *  FUNCTION:   lcd_busydelay
 *
 *  AVAILABILITY:   LOCAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Waits until the busyflag of the LCD module is low.
 *          The busyflag shows the LCD module is still processing 
 *          the latest command.
 */
static void lcd_busydelay ( void )
{
    while (lcd_read_status() & 0x80)
        ;
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_read_status
 *
 *  AVAILABILITY:   LOCAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   unsigned char
 *
 *  DESCRIPTION:    Reads the status register from the LCD module.
 *          This register contains the busyflag and the current 
 *          cursor address in DD-RAM.
 *          Use lcd_getxy() to get the current position of the
 *          cursor in screencoordinates.
 */
static unsigned char lcd_read_status ( void )
{
    unsigned char byte;
#ifdef ALPHALCD_BITBANG
    ALPHALCD_BITBANG_RS = 0; // RS = exec command
    ALPHALCD_BITBANG_RW = 1; // RW = reading
    ALPHALCD_BITBANG_E = 1;                 /* Enable input high */

#  ifdef ALPHALCD_4BITBUS
    ALPHALCD_BITBANG_DB |= 0x0F;// release portpins!!
    byte = (ALPHALCD_BITBANG_DB << 4);          /* Read high nibble */
    ALPHALCD_BITBANG_E = 0;                 /* Enable input low */
    ALPHALCD_BITBANG_E = 1;                 /* Enable input high */
    byte |= (ALPHALCD_BITBANG_DB & 0x0F);           /* Read low nibble */
#  else
    ALPHALCD_BITBANG_DB = 0xFF;// release portpins!!
    byte = ALPHALCD_BITBANG_DB;
#  endif

    ALPHALCD_BITBANG_E = 0;                 /* Enable input low */
#else /* ALPHALCD_BITBANG */

#  ifdef ALPHALCD_4BITBUS
    byte = (ALPHALCD_READ_STATUS << 4);
    byte |= (ALPHALCD_READ_STATUS & 0x0F);
#  else
    byte = ALPHALCD_READ_STATUS;
#  endif

#endif /* ALPHALCD_BITBANG */
    return byte;
}



#ifdef ALPHALCD_BITBANG
/*****************************************************************************
 *
 *  FUNCTION:   lcd_bitbang_write
 *
 *  AVAILABILITY:   LOCAL   (only when ALPHALCD_BITBANG)
 *
 *  PARAMETERS: unsigned char   is_data
 *          unsigned char   byte
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Sends 'byte' to the LCD module. 'is_data' reflects the RS signal.
 *          When 'is_data' is 1, 'byte' is written to the command 
 *          register of the LCD module.
 *          When 'is_data' is 0, 'byte' is written as data.
 */
static void lcd_bitbang_write ( unsigned char is_data , unsigned char byte )
{
    ALPHALCD_BITBANG_RS = is_data;

#ifdef ALPHALCD_4BITBUS
    lcd_bitbang_write_nibble (byte >> 4);           /* high nibble first */
    lcd_bitbang_write_nibble (byte);
#else
    ALPHALCD_BITBANG_RW = 0; // RW = writing
    ALPHALCD_BITBANG_E = 1;                 /* Enable input high */
    ALPHALCD_BITBANG_DB = byte;
    ALPHALCD_BITBANG_E = 0;                 /* Enable input low */
#endif
}
#endif


#if ((defined ALPHALCD_BITBANG) && (defined ALPHALCD_4BITBUS))
/*****************************************************************************
 *
 *  FUNCTION:   lcd_bitbang_write_nibble
 *
 *  AVAILABILITY:   LOCAL   (only when ALPHALCD_BITBANG and ALPHALCD_4BITBUS)
 *
 *  PARAMETERS: unsigned char   nibble
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Sends 'nibble' to the LCD module. The 4 lower bits (0..3) 
 *          of the databus port (ALPHALCD_BITBANG_DB) act as the 4 bit 
 *          databus.
 *          The RS signal is not set, and thus must be preset by the caller!!
 */
static void lcd_bitbang_write_nibble ( unsigned char nibble )
{
    ALPHALCD_BITBANG_RW = 0; // RW = writing
    ALPHALCD_BITBANG_E = 1;                 /* Enable input high */
    ALPHALCD_BITBANG_DB |= 0x0F;
    ALPHALCD_BITBANG_DB &= (0xF0 | nibble);
    ALPHALCD_BITBANG_E = 0;                 /* Enable input low */
}
#endif



/*****************************************************************************
 *
 *  FUNCTION:   lcd_write_data
 *
 *  AVAILABILITY:   LOCAL
 *
 *  PARAMETERS: unsigned char   byte
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Sends 'byte' as DATA to the LCD module.
 */
static void lcd_write_data ( unsigned char byte )
{
#ifdef ALPHALCD_BITBANG
    lcd_bitbang_write ( 1, byte );
#else
#  ifdef ALPHALCD_4BITBUS
    ALPHALCD_WRITE_DATA = (byte >> 4);          /* high nibble first */
    ALPHALCD_WRITE_DATA = byte;
#  else
    ALPHALCD_WRITE_DATA = byte;
#  endif
#endif
    lcd_busydelay ();
}



/*****************************************************************************
 *
 *  FUNCTION:   lcd_write_exec
 *
 *  AVAILABILITY:   LOCAL
 *
 *  PARAMETERS: unsigned char   command
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Sends 'command' as COMMAND to the LCD module.
 */
static void lcd_write_exec ( unsigned char command )
{
#ifdef ALPHALCD_BITBANG
    lcd_bitbang_write ( 0, command );
#else
#  ifdef ALPHALCD_4BITBUS
    ALPHALCD_WRITE_COMMAND = (command >> 4);        /* high nibble first */
    ALPHALCD_WRITE_COMMAND = command;
#  else
    ALPHALCD_WRITE_COMMAND = command;
#  endif
#endif
    lcd_busydelay ();
}


/* Determine the value for the initialize command
 * This value depends on the databus that is used (4/8 bit)
 * and how the liquid cristal screen is actually wired to
 * the display controller (single/multi line).
 */
#undef ALPHALCD_INTERFACE_INIT_VAL
#ifdef ALPHALCD_4BITBUS
#if (ALPHALCD_HEIGHT == 1)
#if (ALPHALCD_WIDTH == 16) && (defined ALPHALCD_CONFIG_1X16_AS_2_LINE)
#define ALPHALCD_INTERFACE_INIT_VAL 0x28            /* Set DL to 4 bits, multiple lines (due to LCD hardware implementation) */
#else
#define ALPHALCD_INTERFACE_INIT_VAL 0x20            /* Set DL to 4 bits, single line */
#endif
#else
#define ALPHALCD_INTERFACE_INIT_VAL 0x28            /* Set DL to 4 bits, multiple lines */
#endif
#else
#if (ALPHALCD_HEIGHT == 1)
#if (ALPHALCD_WIDTH == 16) && (defined ALPHALCD_CONFIG_1X16_AS_2_LINE)
#define ALPHALCD_INTERFACE_INIT_VAL 0x38            /* Set DL to 8 bits, multiple lines (due to LCD hardware implementation) */
#else
#define ALPHALCD_INTERFACE_INIT_VAL 0x30            /* Set DL to 8 bits, single line */
#endif
#else
#define ALPHALCD_INTERFACE_INIT_VAL 0x38            /* Set DL to 8 bits, multiple lines */
#endif
#endif


/*****************************************************************************
 *
 *  FUNCTION:   lcd_init
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Initializes the interface to LCD module, clears the 
 *          screen and turns the display on. The cursor is initialy 
 *          turned off and placed at (0, 0).
 *          If 'user characters' are defined, they are uploadeed 
 *          to the LCD module.
 */
void lcd_init ( void )
{
/* A special startup sequence is required to ensure proper initialization
 * of the LCD module.
 * The sequence actually forces the module to enter the 8 bit mode. Then, the
 * module is configurable with just one 'enable' cycle.
 *
 * When 4 bit mode is desired (ALPHALCD_4BITBUS is defined), lcd_exec produces
 * two 'enable' cycles (one for each nibble). By passing 0x33, the module is
 * in fact configured to 8 bit mode twice. Then we pass 0x32 so the second nibble
 * sets the LCD in 4 bit mode.

 * Do the special startup sequence. */

#ifdef ALPHALCD_4BITBUS
    lcd_write_exec (0x33);                  /* Set DL to 8 bits, twice */
    lcd_write_exec (0x32);                  /* Set DL to 8 bits, then to 4 bit mode */
#else
    lcd_write_exec (0x30);                  /* Set DL to 8 bits */
    lcd_write_exec (0x30);                  /* Set DL to 8 bits, again */
    lcd_write_exec (0x30);                  /* Set DL to 8 bits, one more */
#endif

/* Special sequence done. Now initialize! */

    lcd_write_exec (ALPHALCD_INTERFACE_INIT_VAL);       /* Initialize the LCD interface */

/* The display is now properly initialized! */

    lcd_write_exec (0x06);                  /* Freeze display + increment cursor-pos on every write-action */

    lcd_clear_screen ();                    /* Clear display */


/* 'Upload' user defined characters */

#ifdef ALPHALCD_USERCHAR_0
    lcd_create_char(0, char0_pattern);
#endif
#ifdef ALPHALCD_USERCHAR_1
    lcd_create_char(1, char1_pattern);
#endif
#ifdef ALPHALCD_USERCHAR_2
    lcd_create_char(2, char2_pattern);
#endif
#ifdef ALPHALCD_USERCHAR_3
    lcd_create_char(3, char3_pattern);
#endif
#ifdef ALPHALCD_USERCHAR_4
    lcd_create_char(4, char4_pattern);
#endif
#ifdef ALPHALCD_USERCHAR_5
    lcd_create_char(5, char5_pattern);
#endif
#ifdef ALPHALCD_USERCHAR_6
    lcd_create_char(6, char6_pattern);
#endif
#ifdef ALPHALCD_USERCHAR_7
    lcd_create_char(7, char7_pattern);
#endif


#ifdef ALPHALCD_VFD_MODULE
    lcd_set_brightness ( 0 );               /* Set brightness to 100% */
#endif

    lcd_gotoxy(0, 0);
    lcd_hidecursor();
    lcd_display_on();
}




/*****************************************************************************
 *
 *  FUNCTION:   lcd_showcursor
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: unsigned char   type
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Sets the cursor type to be displayed.
 *          Two cursor types are available, which can be OR'd to 
 *          combine them:
 *          - CURSOR_TYPE_BLOCK
 *          - CURSOR_TYPE_UNDERSCORE
 */
void lcd_showcursor ( unsigned char type )
{
    displaypower_cursortype_mode = (displaypower_cursortype_mode & ~CURSOR_TYPE_MASK) | type;

    lcd_write_exec (0x08 | displaypower_cursortype_mode);
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_hidecursor
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Hide the cursor
 */
void lcd_hidecursor ( void )
{
    displaypower_cursortype_mode = (displaypower_cursortype_mode & ~CURSOR_TYPE_MASK);

    lcd_write_exec (0x08 | displaypower_cursortype_mode);
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_display_on
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Turns the screen on when 'on' is nonzero, off when zero.
 */
void lcd_display_on ( void )
{
    displaypower_cursortype_mode = (displaypower_cursortype_mode | DISPLAY_POWER_ON);

    lcd_write_exec (0x08 | displaypower_cursortype_mode);
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_display_off
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Turns the screen on when 'on' is nonzero, off when zero.
 */
void lcd_display_off ( void )
{
    displaypower_cursortype_mode = (displaypower_cursortype_mode & DISPLAY_POWER_OFF);

    lcd_write_exec (0x08 | displaypower_cursortype_mode);
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_clear_screen
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Clears the screen.
 */
void lcd_clear_screen ( void )
{
    lcd_write_exec (0x01);
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_putc
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: unsigned char   character
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Writes 'character' to the screen at the current cursor
 *          position. The cursor shifts automatically one position
 *          to the right, or, if needed jumps to the next line.
 *          if ()screen and turns the display on. The cursor is initialy
 *          turned off and placed at (0, 0).
 *
 *          If ALPHALCD_HANDLE_NEWLINE is defined, and 'character'
 *          happens to be a newline, the cursor will jump to the
 *          next line also. The 'character' itself is not printed then.
 *          The cursor automatically rolls over to home (0, 0) when
 *          writing at the most bottom-right position.
 *
 *          Special support is added for 1x16 modules that requires
 *          repositioning of the cursor when writing at postion 7.
 */
void lcd_putc ( unsigned char character )
{
#if !((ALPHALCD_HEIGHT == 2) && (ALPHALCD_WIDTH == 40))
/* For all displays, except the 2x40, we need to keep track of the cursor position */
    lcd_cursorpos_t cursorpos;
#endif

#ifdef ALPHALCD_HANDLE_NEWLINE
    if (character == '\n')
    {
        lcd_gotoxy(0, lcd_getxy().y + 1);       /* Jump to beginning of next line */
        return;
    }
#endif


#if !((ALPHALCD_HEIGHT == 2) && (ALPHALCD_WIDTH == 40))
/* For all displays, except the 2x40, we need to keep track of the cursor position */

    cursorpos = lcd_getxy();                /* Get the current cursor position */
#endif


    lcd_write_data ( character );


#if !((ALPHALCD_HEIGHT == 2) && (ALPHALCD_WIDTH == 40))
/* For all displays, except the 2x40, we need to keep track of the cursor position */
    if (cursorpos.x == ALPHALCD_WIDTH-1)            /* Did we just print the most right character position ? */
    {
#ifdef ALPHALCD_GOTOXY_CLIP
/* Special test is required, otherwise writing to the most bottom-rioght position would result
 * in jumpting to the bottom-left instead of going back home.
 */
        if (cursorpos.y == ALPHALCD_HEIGHT-1)
        {
            lcd_gotoxy(0, 0);           /* Jump home */
        }
        else
#endif
        {
            lcd_gotoxy(0, cursorpos.y + 1);     /* Jump to beginning of next line */
        }
    }

#if ((ALPHALCD_HEIGHT == 1) && (ALPHALCD_WIDTH == 16) && (defined ALPHALCD_CONFIG_1X16_AS_2_LINE))
// special treatment for an 1x16 display which needs a gotoxy when writing at position 7

    else if (cursorpos.x == 7)
    {
        lcd_gotoxy(8, 0);
    }
#endif
#endif
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_gotoxy
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: unsigned char   x
 *          unsigned char   y
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Places the cursor at screencoordinates specified 
 *          by 'x' and 'y'.
 *
 *          When 'x' and/or 'y' is outside the screen rectangle
 *          (specified by ALPHALCD_WIDTH and ALPHALCD_HEIGHT),
 *          'x' and/or 'y' will be adjusted. This adjustment 
 *          depends on ALPHALCD_GOTOXY_CLIP.
 *
 *          With ALPHALCD_GOTOXY_CLIP defined, the cursor clips
 *          on the screen's boundaries. So placing the cursor at (18, 0)
 *          on a 2x16 LCD will result in (15, 0).
 *
 *          When ALPHALCD_GOTOXY_CLIP is NOT defined, the cursor 
 *          will be placed at the beginning of the next line, 
 *          when 'x' is outside.
 *          When 'y' is outside, the cursor jumps home (0, 0). 
 *          Since 'x' is tested first, the cursor also jumps 
 *          home when 'x' is out of range while 'y' is at the 
 *          very bottom line.
 *
 *          All this fucntionality works well together with the 
 *          functions lcd_movex() and lcd_movey().
 *
 *          Special support is added for 1x16 modules that are 
 *          internally wired as a 2x8 display.
 */
void lcd_gotoxy ( unsigned char x, unsigned char y )
{
    unsigned char offset = 0;

    if (x >= ALPHALCD_WIDTH)
    {
#ifdef ALPHALCD_GOTOXY_CLIP
        x = ALPHALCD_WIDTH-1;               /* Clip to the right border */
#else
        x = 0;                      /* Jump to start of next line */
        y++;
#endif
    }

    if (y >= ALPHALCD_HEIGHT)
    {
#ifdef ALPHALCD_GOTOXY_CLIP
        y = ALPHALCD_HEIGHT-1;              /* Clip to the bottom border */
#else
        x = 0;                      /* Jump home */
        y = 0;
#endif
    }

#if (ALPHALCD_HEIGHT > 1)
    switch (y)
    {
        case 0 : { offset =  0; break; }
        case 1 : { offset = 64; break; }
#if (ALPHALCD_HEIGHT >= 2)
        case 2 : { offset = ALPHALCD_WIDTH; break; }
        case 3 : { offset = ALPHALCD_WIDTH + 64; break; }
#endif
        default: { offset =  0; }
    }

#else
#if (ALPHALCD_WIDTH == 16) && (defined ALPHALCD_CONFIG_1X16_AS_2_LINE)
    if (x >= 8)
    {
        offset = (64-8);
    }
#endif

#endif
    lcd_write_exec (0x80 + offset + x);
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_getxy
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: None
 *
 *  RETURN VALUE:   lcd_cursorpos_t
 *
 *  DESCRIPTION:    Reads the current cursor address back from LCD module
 *          and converts it to the actual screencoordinates.
 *          The result is returned in the two byte structure 
 *          lcd_cursorpos_t.
 *
 *          Special support is added for 1x16 modules that are 
 *          internally wired as a 2x8 display.
 */
lcd_cursorpos_t lcd_getxy ( void )
{
    unsigned char address;
    lcd_cursorpos_t cursorpos;

    address = lcd_read_status() & 0x7F;         /* Get cursor address from module (+ mask away busy flag) */
    cursorpos.y = 0;                    /* Initial value for y overwritten when different */

#if (ALPHALCD_HEIGHT == 1)
#if (ALPHALCD_WIDTH == 16) && (defined ALPHALCD_CONFIG_1X16_AS_2_LINE)
    if (address >= 64)
    {
        address -= (64 - 8);
    }
#endif


#elif (ALPHALCD_HEIGHT == 2)
    if (address >= 64)
    {
        cursorpos.y = 1;
        address -= 64;
    }
#else
#if (ALPHALCD_WIDTH > 20)
#error > This size of LCD is not supported! Should have been detected earlier in headerfile.
#endif
    if (address >= (ALPHALCD_WIDTH + 64))
    {
        cursorpos.y = 3;
        address -= (ALPHALCD_WIDTH + 64);
    }
    else if (address >= 64)
    {
        cursorpos.y = 1;
        address -= 64;
    }
    else if (address >= ALPHALCD_WIDTH)
    {
        cursorpos.y = 2;
        address -= ALPHALCD_WIDTH;
    }

#endif

    cursorpos.x = address;                  /* address now contains the horizontal (x) position */
    return cursorpos;
}


/*****************************************************************************
 *
 *  FUNCTION:   lcd_create_char
 *
 *  AVAILABILITY:   GLOBAL
 *
 *  PARAMETERS: unsigned char       character
 *          __rom unsigned char *   pattern
 *
 *  RETURN VALUE:   None
 *
 *  DESCRIPTION:    Uploads a userdefined 5x8 character/bitmap,
 *          described in 'pattern', as 'character' into the LCD module.
 *
 *          Up to 8 userdefined characters can be uploaded, and can be
 *          put on the screen using lcd_putc e.g..
 *          Often the 8 usercharacters are mapped twice in the ASCII table:
 *           from 0 .. 7 and 8 .. 15.
 *
 *          'pattern' must be an unsigned char array of 8 bytes, where the 5
 *          lower bits reflect the 5 pixels of a character:
 *          __rom unsigned char degress_celcius[] = { 0x06, 0x04, 0x06, 0x04, 0x06, 0x04, 0x0E, 0x0E };
 *
 *          Note that row 8 is also used by the cursor CURSOR_TYPE_UNDERSCORE.
 */
void lcd_create_char ( unsigned char character, __rom unsigned char * pattern )
{
    unsigned char row;

    lcd_write_exec (0x40 | ((character & 0x07) << 3));  /* Select character to be created */

    for (row = 0; row < 8; row++)
    {
        lcd_write_data (*pattern++ & 0x1F);
    }

    lcd_write_exec (0x00);                  /* End character-creation */
}
