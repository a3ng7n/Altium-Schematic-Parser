#define LCD_STROBE   0x80
#define LCD_LINE0    0x00
#define LCD_LINE1    0x10



#define ADDR_LCD_DAT_REG      0xF7
#define LCD_DAT_REG		      (*(__sfr unsigned char *)0xF7)
#define LCD_DAT_REG_bit(x)	   ((*(__sfr __bitstruct_t *)0xF7).__b ## x )

#define ADDR_LCD_CTRL_REG     0xFF
#define LCD_CTRL_REG	   	   (*(__sfr unsigned char *)0xFF)
#define LCD_CTRL_REG_bit(x)	((*(__sfr __bitstruct_t *)0xFF).__b ## x )

#define ADDR_CAN_ADR_REG      0xE7
#define CAN_ADR_REG		      (*(__sfr unsigned char *)0xE7)
#define CAN_ADR_REG_bit(x)	   ((*(__sfr __bitstruct_t *)0xE7).__b ## x )

#define ADDR_CAN_DAT_REG      0xEF
#define CAN_DAT_REG	   	   (*(__sfr unsigned char *)0xEF)
#define CAN_DAT_REG_bit(x)	   ((*(__sfr __bitstruct_t *)0xEF).__b ## x )


#define CAN_MOD   0x00
#define CAN_CMR   0x01
#define CAN_SR    0x02
#define CAN_IR    0x03
#define CAN_IER   0x04
#define CAN_BTR0  0x06
#define CAN_BTR1  0x07
#define CAN_OCR   0x08
#define CAN_ALC   0x0B
#define CAN_ECC   0x0C
#define CAN_EWL   0x0D
#define CAN_REC   0x0E
#define CAN_TEC   0x0F

#define CAN_ACR0  0x10
#define CAN_ACR1  0x11
#define CAN_ACR2  0x12
#define CAN_ACR3  0x13

#define CAN_AMR0  0x14
#define CAN_AMR1  0x15
#define CAN_AMR2  0x16
#define CAN_AMR3  0x17

#define CAN_RXB1  0x11
#define CAN_RXD1  0x14

#define CAN_TXID0 0x10
#define CAN_TXID1 0x11
#define CAN_TXID2 0x12
#define CAN_TXD0  0x13
#define CAN_TXD1  0x14
#define CAN_TXD2  0x15
#define CAN_TXD3  0x16
#define CAN_TXD4  0x17
#define CAN_TXD5  0x18
#define CAN_TXD6  0x19
#define CAN_TXD7  0x1A

#define CAN_MC    0x1D
#define CAN_RBSA  0x1E
#define CAN_CDR   0x1F


unsigned char CanD1R = 0 ;
unsigned char CanIDR = 0 ;
unsigned char P1FD1R = 0 ;
unsigned char MainCounter = 0 ;
unsigned char ErrorNumber = 0 ;

/* ------------------------------------------------------------------------ */
void           CanInterruptServiceRoutine (  void ) ;
/* ------------------------------------------------------------------------ */
unsigned char  GetCanRegister             (  unsigned char reg ) ;
/* ------------------------------------------------------------------------ */
__bit          GetLcdBusyFlag             (  void ) ;
/* ------------------------------------------------------------------------ */
void           DisplayCharacter           (  unsigned char c,
                                             unsigned char line,
                                             unsigned char position
                                          ) ;
/* ------------------------------------------------------------------------ */
void           DisplayLine                (  unsigned char* str,
                                             unsigned char line,
                                             unsigned char start_position,
                                             unsigned char length
                                          ) ;
/* ------------------------------------------------------------------------ */
void           InitLCD                    (  void ) ;
/* ------------------------------------------------------------------------ */
__bit          GetCanResetBit             (  void ) ;
/* ------------------------------------------------------------------------ */
void           LoadCanRegister            (  unsigned char reg,
                                             unsigned char data
                                          ) ;
/* ------------------------------------------------------------------------ */
void           InitCAN                    (  void ) ;
/* ------------------------------------------------------------------------ */
unsigned char  GetCharToDisplay           (  unsigned char c ) ;
/* ------------------------------------------------------------------------ */
void           DisplayMsg_TX              (  unsigned char tx ) ;
/* ------------------------------------------------------------------------ */
void           DisplayMsg_RX              (  unsigned char rx ) ;
/* ------------------------------------------------------------------------ */
void           SetFrameToTransmit         (  void ) ;
/* ------------------------------------------------------------------------ */
void           TransmitFrame              (  void ) ;
/* ------------------------------------------------------------------------ */
void           InitInterrupts             (  void ) ;
/* ------------------------------------------------------------------------ */
void           WaitLong                   (  void ) ;
/* ------------------------------------------------------------------------ */