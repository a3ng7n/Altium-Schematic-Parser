/* --------------------------------------------------------------------- */
// LCD
#define ADDR_LCD_DAT          0xF7
#define LCD_DAT		         (*(__sfr unsigned char *)0xF7)
#define LCD_DAT_bit(x)	      ((*(__sfr __bitstruct_t *)0xF7).__b ## x )

#define ADDR_LCD_CTRL         0xFF
#define LCD_CTRL		         (*(__sfr unsigned char *)0xFF)
#define LCD_CTRL_bit(x)	      ((*(__sfr __bitstruct_t *)0xFF).__b ## x )
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
// Keypad
#define ADDR_KPAD_DAT         0xEF
#define KPAD_DAT		         (*(__sfr unsigned char *)0xEF)
#define KPAD_DAT_bit(x)	      ((*(__sfr __bitstruct_t *)0xEF).__b ## x )
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
// Serial
#define ADDR_SERIAL_PCON      0xE7
#define SERIAL_PCON		      (*(__sfr unsigned char *)0xE7)
#define SERIAL_PCON_bit(x)	   ((*(__sfr __bitstruct_t *)0xE7).__b ## x )

#define ADDR_SERIAL_SCON      0xDF
#define SERIAL_SCON           (*(__sfr unsigned char *)0xDF)
#define SERIAL_SCON_bit(x)	   ((*(__sfr __bitstruct_t *)0xDF).__b ## x )

#define ADDR_SERIAL_SBUF      0xD7
#define SERIAL_SBUF		      (*(__sfr unsigned char *)0xD7)
#define SERIAL_SBUF_bit(x)	   ((*(__sfr __bitstruct_t *)0xD7).__b ## x )

#define ADDR_SERIAL_SRELL     0xCF
#define SERIAL_SRELL          (*(__sfr unsigned char *)0xCF)
#define SERIAL_SRELL_bit(x)   ((*(__sfr __bitstruct_t *)0xCF).__b ## x )

#define ADDR_SERIAL_SRELH     0xC7
#define SERIAL_SRELH		      (*(__sfr unsigned char *)0xC7)
#define SERIAL_SRELH_bit(x)   ((*(__sfr __bitstruct_t *)0xC7).__b ## x )

#define ADDR_SERIAL_TCON      0xBF
#define SERIAL_TCON		      (*(__sfr unsigned char *)0xBF)
#define SERIAL_TCON_bit(x)	   ((*(__sfr __bitstruct_t *)0xBF).__b ## x )

#define ADDR_SERIAL_TL        0xB7
#define SERIAL_TL		         (*(__sfr unsigned char *)0xB7)
#define SERIAL_TL_bit(x)	   ((*(__sfr __bitstruct_t *)0xB7).__b ## x )

#define ADDR_SERIAL_TH        0xAF
#define SERIAL_TH		         (*(__sfr unsigned char *)0xAF)
#define SERIAL_TH_bit(x)	   ((*(__sfr __bitstruct_t *)0xAF).__b ## x )

#define ADDR_SERIAL_ADCON     0xA7
#define SERIAL_ADCON		      (*(__sfr unsigned char *)0xA7)
#define SERIAL_ADCON_bit(x)   ((*(__sfr __bitstruct_t *)0xA7).__b ## x )

/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
#define KPAD      0x00
#define L_DAT     0x01
#define L_CTRL    0x02
#define S_PCON    0x03
#define S_SCON    0x04
#define S_SBUF    0x05
#define S_SRELL   0x06
#define S_SRELH   0x07
#define S_TCON    0x08
#define S_TL      0x09
#define S_TH      0x0A
#define S_ADCON   0x0B
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
#define SRELL_INIT   0x9E
#define SRELH_INIT   0x03
#define SCON_INIT    0x50
#define PCON_INIT    0x80
#define ADCON_INIT   0x80
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
#define LCD_LINE0    0x00
#define LCD_LINE1    0x10
#define LCD_STROBE   0x80
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
#define KEY_1 0x00
#define KEY_2 0x01
#define KEY_3 0x02
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
volatile unsigned char key_ON  = 0x00 ;
volatile unsigned char key_VAL = 0x00 ;
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
volatile unsigned char RecDatCount = 0 ;
volatile unsigned char UnblockRec = 0 ;
volatile unsigned char AllowTransmit = 0 ;
#define  TEXTSIZE 33
unsigned char Text[TEXTSIZE] ;
#define KCODE
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
#define _INT0 0x03
#define _INT1 0x13
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
__rom unsigned char E_Line    [] = "                " ;
__rom unsigned char S_Message [] = "Send mode" ;
__rom unsigned char R_Message [] = "Receive mode" ;
__rom unsigned char W_Message [] = "Welcome to      Serial" ;
/* --------------------------------------------------------------------- */


extern void KeyPadInterrupt(void) ;
extern void SerialInterrupt(void) ;
extern __bit GetLcdBusyFlag(void) ;
extern void DisplayCharOnLcd(unsigned char pos, unsigned char c) ;
extern void DisplayMessageOnLcd(  unsigned char line,
                                   __rom KCODE unsigned char * msg
                                ) ;
extern void InitializeLcd(void) ;
extern void ClearLcd(void) ;
extern void LoadWbReg(   unsigned char reg,
                  unsigned char val
              ) ;
extern unsigned char GetWbReg(unsigned char reg) ;

extern void TransmitData(void) ;
extern void InitDataMemW(void) ;
extern void ScanKeypadAndDisplayMessage(void) ;
extern void InitializeSerial(void) ;
extern void InitializeInterrupts(void) ;
extern void Initialize(void) ;


