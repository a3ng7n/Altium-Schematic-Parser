/*******************************************************************************
 * FILE:     @(#)lcd.c	1.4 04/09/08                                        
 * DESCRIPTION:                                             
 *  Source for the OSEK LCD device driver.                  
 ******************************************************************************/
#if defined(DEMO)
#include "demo.h"
#else
#include "tetris.h"
#endif

#if defined(NEXAR)

#include <osek/osek.h>

/* width of the LCD screen */
#define    LCD_WIDTH       16
/* height of the LCD screen */
#define    LCD_HEIGHT      2

/****************************************************
 * Fixed mapped 'data' in external data space.
 * Registers of the LCD module are accessed through
 * the external bus.
 ***************************************************/
#define LCD_BASEADDR        0x200

/* The Write Data Register sends commands to the LCD module */
#define LCD_WRITE_COMMAND  (*(__xdata volatile unsigned char *) (LCD_BASEADDR + 0))
/* The Write Data Register sends a byte as DATA to the LCD module */
#define LCD_WRITE_DATA     (*(__xdata volatile unsigned char *) (LCD_BASEADDR + 1))
/* The Read Status Register contains the busy flag and the
 * cursor coordinates */
#define LCD_READ_STATUS    (*(__xdata volatile unsigned char *) (LCD_BASEADDR + 2))
/* the Busy Flag shows that the system is still processing a command */
#define LCD_BUSY_FLAG       0x80

#define LCD_READ_DATA      (*(__xdata volatile unsigned char *) (LCD_BASEADDR + 3))

typedef struct
{
    unsigned char x;
    unsigned char y;
} LCD_CursorPos;


inline void
LCD_Busy(void)
{
    while (LCD_READ_STATUS & LCD_BUSY_FLAG) ;
}

static void LCD_ClearScreen(void);
static void LCD_PutC ( unsigned char c );
static void LCD_WriteData ( unsigned char byte );
static void LCD_WriteExec ( unsigned char command );
static void LCD_SetXY ( unsigned char x, unsigned char y );
static LCD_CursorPos LCD_GetXY ( void );
#define CURSOR_TYPE_OFF              0x00
#define CURSOR_TYPE_BLOCK            0x01
#define CURSOR_TYPE_UNDERSCORE       0x02
#define CURSOR_TYPE_BOTH             (CURSOR_TYPE_BLOCK | CURSOR_TYPE_UNDERSCORE)
#define CURSOR_TYPE_MASK             0x03      
#define DISPLAY_POWER_ON             0x04
#define DISPLAY_POWER_OFF            ~DISPLAY_POWER_ON
#define LCD_HideCursor()            LCD_SetPowerCursor(CURSOR_TYPE_OFF)
#define LCD_SetCursorBlock()        LCD_SetPowerCursor(CURSOR_TYPE_BLOCK)
#define LCD_SetCursorUnderscore()   LCD_SetPowerCursor(CURSOR_TYPE_UNDERSCORE)
#define LCD_SetCursorBoth()         LCD_SetPowerCursor(CURSOR_TYPE_BOTH)
#define LCD_DisplayOn()             LCD_SetPowerCursor(DISPLAY_POWER_ON) 
#define LCD_DisplayOff()            LCD_SetPowerCursor(DISPLAY_POWER_OFF)  
static  void LCD_SetPowerCursor(unsigned char);
static  unsigned char PowerCursorMode    = CURSOR_TYPE_OFF;      

/*******************************************************************
 *  Places the cursor at screencoordinates specified 
 *  by 'x' and 'y'.
 *
 *  When 'x' and/or 'y' is outside the screen rectangle
 *  (specified by ALPHALCD_WIDTH and ALPHALCD_HEIGHT),
 * 'x' and/or 'y' will be adjusted. 
 *
 *  With ALPHALCD_GOTOXY_CLIP defined, the cursor clips
 *  on the screen's boundaries. So placing the cursor at (18, 0)
 *  on a 2x16 LCD will result in (15, 0).
 ******************************************************************/
static void LCD_SetXY ( unsigned char x, unsigned char y );
 /************************************************************
 * Reads the current cursor address back from LCD module
 * and converts it to the actual screencoordinates.
 * The result is returned in the two byte structure 
 * LCD_CursorPos
 ************************************************************/
static LCD_CursorPos LCD_GetXY ( void );

/**************************************************************
 * Determine the value for the initialize command
 * This value depends on the databus that is used (4/8 bit)
 * and how the liquid cristal screen is actually wired to 
 * the display controller (single/multi line).
 * In our case: 8 bit bus, multiple lines
 **************************************************************/
#define LCD_INTERFACE_INIT_VAL 0x38

DeclareMessage(mrlcdtx);
DeclareMessage(mslcdtx);
DeclareEvent(elcdtx);


TASK (tlcdtx)
{
	ioreq* preqtx;

    	if (E_OK != ReceiveMessage(mrlcdtx,&preqtx))
    	{
            	/* an io request has been lost */
        	ShutdownOS(E_ERR_IO_OFLW);
    	}
	else
	{
		/* blocking io driver: sorry */
		while (preqtx->len > preqtx->pos)
		{
			LCD_PutC(preqtx->buf[preqtx->pos++]);
		}

		/* wake up write task */
		SetEvent(preqtx->task,preqtx->event);
	}

	TerminateTask();
}

/* statics */

static void
LCD_WriteData ( unsigned char byte )
{
     LCD_WRITE_DATA = byte;
     LCD_Busy();
}

static void
LCD_WriteExec ( unsigned char command )
{
     LCD_WRITE_COMMAND = command;
     LCD_Busy();   
}

static void LCD_SetPowerCursor(unsigned char type)
{
    PowerCursorMode = (PowerCursorMode & ~CURSOR_TYPE_MASK) | type;
    LCD_WriteExec(0x08 | PowerCursorMode);    
}


static void LCD_SetXY ( unsigned char x, unsigned char y )
{
    unsigned char offset = 0;

    if (x >= LCD_WIDTH)
    {
        x = LCD_WIDTH-1;
    }
    if (y >= LCD_HEIGHT)
    {
        y = LCD_HEIGHT-1;
    }

    /* LCD_HEIGHT == 2 */
    switch (y)
    {
        case 0 : { offset =  0; break; }
        case 1 : { offset = 64; break; }
        default: { offset =  0; }
    }

    LCD_WriteExec(0x80 + offset + x);
}

static LCD_CursorPos LCD_GetXY( void )
{
    /* get address from Read Status register */
    unsigned char address;     
    address = LCD_READ_STATUS& 0x7F;
    LCD_CursorPos cursorpos = {0,0};
    
    if (address >= 64)
    {
        cursorpos.y = 1;
        address -= 64;
    }

    cursorpos.x = address;           
           
    return cursorpos;
}


static void LCD_ClearScreen(void)
{      
    LCD_WriteExec(0x1);
}

static void LCD_PutC ( unsigned char c )
{
    /* track  cursor position */
    LCD_CursorPos pos;
    unsigned char endline=0;  

    pos = LCD_GetXY();

    if (c == '\n')
    {
        /* Go to next line */
        LCD_SetXY(0, pos.y + 1); 
        endline = 1;
    }
    else
    {
          /* write character */
          LCD_WriteData(c);
    }

    /* Have we just print in the rightest position
     * or and 'endline' -> go to next line
     */ 
    if (pos.x == LCD_WIDTH-1 || endline)  
    {
        if (pos.y == LCD_HEIGHT-1)
        {
            /* back to 0,0 */
            LCD_SetXY(0, 0);          
        }
        else
        {
            /* next line */
            LCD_SetXY(0, pos.y + 1);
        }
    }
}

#endif // nexar

/*****************************************************
 * no need to protect it against race conditions since
 * this routine must have run before any other task
 * attempts to access the device.
 *****************************************************/
void lcd_init ( void )
{

#if defined(NEXAR)

	/* Do the special startup sequence. */
    LCD_WriteExec (0x30);   /* Set DL to 8 bits */
    LCD_WriteExec (0x30);   /* Set DL to 8 bits, again */
    LCD_WriteExec (0x30);   /* Set DL to 8 bits, one more */

    /* Initialize the LCD interface */
    LCD_WriteExec (LCD_INTERFACE_INIT_VAL);

    /* Freeze display + increment cursor-pos on every write-action */
    LCD_WriteExec (0x06);

    /* Clear Display */
    LCD_ClearScreen ();

    /* Reset Cursor */
    LCD_SetXY(0, 0);
    LCD_HideCursor();
    LCD_DisplayOn();
#endif
    
    /* init the lcd device */
    devices[LCD].rxevent = 0;
    devices[LCD].rxmsg   = -1;
#if defined(NEXAR)
    devices[LCD].txevent = elcdtx;
    devices[LCD].txmsg   = mslcdtx;
#else
    devices[LCD].txevent = 0;
    devices[LCD].txmsg   = -1;
#endif
    
}

