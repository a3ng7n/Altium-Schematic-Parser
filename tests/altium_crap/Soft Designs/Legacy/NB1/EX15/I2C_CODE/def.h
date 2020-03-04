#define LCD_STROBE   0x80
#define LCD_LINE0    0x00
#define LCD_LINE1    0x10

#define DAC_CMD_PowerUp    0xF0
#define DAC_CMD_PowerDown  0x3C



/* ------------------------------------------------------------------------ */

// LCD
#define ADDR_LCD_DAT_REG      0xF7
#define LCD_DAT_REG            (*(__sfr unsigned char *)0xF7)
#define LCD_DAT_REG_bit(x)     ((*(__sfr __bitstruct_t *)0xF7).__b ## x )

#define ADDR_LCD_CTRL_REG     0xFF
#define LCD_CTRL_REG           (*(__sfr unsigned char *)0xFF)
#define LCD_CTRL_REG_bit(x) ((*(__sfr __bitstruct_t *)0xFF).__b ## x )

// KEYPAD
#define ADDR_KEYPAD_REG       0xB7
#define KEYPAD_REG          (*(__sfr unsigned char *)0xB7)
#define KEYPAD_REG_bit(x)      ((*(__sfr __bitstruct_t *)0xB7).__b ## x )


/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
__bit i2c_int = 0 ;
__bit key_int = 0 ;

unsigned char InputCharacter = 'x' ;

// Holds input string from Keypad
//struct KPAD_STRING {
//   unsigned char str[] ;   // format: "2.67" [V]
//   unsigned char cnt ;     // holds last character position in the string
//} string_var ;
unsigned char my_str[4] ;
unsigned char my_str_cnt = 0 ;
//struct KPAD_STRING string ;
//   KPAD_STRING *str = &string_var ;  // get pointer to string input from keypad
/* ------------------------------------------------------------------------ */
void I2CInterruptServiceRoutine(void) ;
__bit GetLcdBusyFlag(void) ;
void DisplayCharacter(  unsigned char c,
                        unsigned char line,
                        unsigned char position
                      ) ;
void DisplayLine( unsigned char* str,
                  unsigned char line,
                  unsigned char start_position,
                  unsigned char length
                 ) ;
void GetI2cClock( unsigned char freq,
                  unsigned char sclk
                ) ;
//void InitI2C( unsigned char freq,
//              unsigned char sclk
//            ) ;
void InitInterrupts(void) ;
void WaitLong(void) ;
unsigned char KeypadInterruptServiceRoutine(void) ;
void InitLCD(void) ;
void DisplayDacString(void) ;