#include "def.h"

#define DEBUG_INFO


/* ------------------------------------------------------------------------ */
__interrupt(0*8+3) void Interrupt0Handler( void ) {
   EAL = 0 ;
   CanInterruptServiceRoutine();
   EAL = 1 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void CanInterruptServiceRoutine(void) {
   CAN_ADR_REG = CAN_IR ;
   while (!CAN_DAT_REG_bit(0)) {
      return ;
   }

   CanD1R = GetCanRegister(CAN_RXD1) ;
   if (CanIDR ^ 0x55) {
      P1FD1R = CanD1R ;
   }
   // Release Receive Buffer
   LoadCanRegister(CAN_CMR, 0x04) ;

   return ;
}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
unsigned char GetCanRegister(unsigned char reg) {
   CAN_ADR_REG = reg ;
   return CAN_DAT_REG ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
__bit GetLcdBusyFlag(void) {
   return LCD_CTRL_REG_bit(6) ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void DisplayCharacter(  unsigned char c,
                        unsigned char line,
                        unsigned char position
                      ) {
   // TO DO
   unsigned char tmp ;

   if (line==LCD_LINE0) {
      tmp = LCD_STROBE | position ;
   } else {
      tmp = LCD_STROBE | position | LCD_LINE1 ;
   }
   while (GetLcdBusyFlag()==1) {
      ; // wait for the LCD not busy
   }
   LCD_DAT_REG = c ;
   LCD_CTRL_REG = tmp ;

   while (GetLcdBusyFlag()==1) {
      ; // wait for the LCD not busy
   }

}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void DisplayLine( unsigned char* str,
                  unsigned char line,
                  unsigned char start_position,
                  unsigned char length
                 ) {
   for (unsigned char i=start_position; i < (start_position + length); i++) {
      DisplayCharacter( str[i-start_position],
                        line,
                        i
                      ) ;
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void InitLCD(void) {
   DisplayLine("Transmit: ", LCD_LINE0, 0, 10) ;
   DisplayLine("Receive : ", LCD_LINE1, 0, 10) ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
__bit GetCanResetBit(void) {
   CAN_ADR_REG = CAN_MOD ;
   return CAN_DAT_REG_bit(0) ;
   }
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void LoadCanRegister(unsigned char reg, unsigned char data) {
   CAN_ADR_REG = reg ;
   CAN_DAT_REG = data ;

   if (CAN_DAT_REG!=data) {
      ;
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void InitCAN(void) {
   //Load 01 to the CR = MODE register
   LoadCanRegister(CAN_MOD, 0x01) ;
   // Check if reset bit is set
   while(GetCanResetBit()==0) {
      ;
   }
   // Load 0xFF to the ACCMASK0 register
   LoadCanRegister(CAN_AMR0, 0xFF) ;
   // Load 0xFF to the ACCMASK1 register
   LoadCanRegister(CAN_AMR1, 0xFF) ;
   // Load 0xFF to the ACCMASK2 register
   LoadCanRegister(CAN_AMR2, 0xFF) ;
   // Load 0xFF to the ACCMASK3 register
   LoadCanRegister(CAN_AMR3, 0xFF) ;
   // Load 0xC0 to the CDR register
   LoadCanRegister(CAN_CDR, 0xC0) ;
   // Load 0x03 to the BTR0 register
   LoadCanRegister(CAN_BTR0, 0x03) ;
   // Load 0x6E to the BTR1 register
   LoadCanRegister(CAN_BTR1, 0x6E) ;
   // Load 0x1A to the OCR register
   LoadCanRegister(CAN_OCR, 0x1A) ;
   // Load 0x00 to the CR = MODE register, clear reset bit
   LoadCanRegister(CAN_MOD, 0x00) ;
   // Enable Receive interrupt
   LoadCanRegister(CAN_IER, 0x01) ;
   // Check if reset bit is cleared
   while(GetCanResetBit()==1) {
      ;
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
unsigned char GetCharToDisplay(unsigned char c) {
    unsigned char tmp = 0x00 ;
    tmp = c & 0x0F ;
   switch (tmp) {
      case 0x00 : return '0' ;
      case 0x01 : return '1' ;
      case 0x02 : return '2' ;
      case 0x03 : return '3' ;
      case 0x04 : return '4' ;
      case 0x05 : return '5' ;
      case 0x06 : return '6' ;
      case 0x07 : return '7' ;
      case 0x08 : return '8' ;
      case 0x09 : return '9' ;
      case 0x0A : return 'A' ;
      case 0x0B : return 'B' ;
      case 0x0C : return 'C' ;
      case 0x0D : return 'D' ;
      case 0x0E : return 'E' ;
      case 0x0F : return 'F' ;
   }
   return ' ' ;
}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
void DisplayMsg_TX(unsigned char tx) {
   DisplayCharacter(GetCharToDisplay(tx), LCD_LINE0, 15) ;
}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
void DisplayMsg_RX(unsigned char rx) {
   DisplayCharacter(GetCharToDisplay(rx), LCD_LINE1, 15) ;
}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
void SetFrameToTransmit(void) {
   CAN_ADR_REG = CAN_SR ;

   if (CAN_DAT_REG_bit(6)) {
      InitCAN() ;
   }

   CAN_ADR_REG = CAN_SR ;
   while (!CAN_DAT_REG_bit(2)) {
      ;
   }
   // Load 0x03 to the TXID0 register
   LoadCanRegister(CAN_TXID0, 0x03) ;
   // Load 0x55 to the TXID1 register
   LoadCanRegister(CAN_TXID1, 0x55) ;
   // Load 0x00 to the TXID2 register
   LoadCanRegister(CAN_TXID2, 0x00) ;
   // Load 0x00 to the TXD0 register
   LoadCanRegister(CAN_TXD0, 0x00) ;
   // Load counter value to the TXD1 register
   LoadCanRegister(CAN_TXD1, MainCounter) ;
   // Load 0x00 to the TXD2 register
   LoadCanRegister(CAN_TXD2, 0x00) ;
   // Load 0x00 to the TXD3 register
   LoadCanRegister(CAN_TXD3, 0x00) ;
   // Load 0x00 to the TXD4 register
   LoadCanRegister(CAN_TXD4, 0x00) ;
   // Load 0x00 to the TXD5 register
   LoadCanRegister(CAN_TXD5, 0x00) ;
   // Load 0x00 to the TXD6 regisiter
   LoadCanRegister(CAN_TXD6, 0x00) ;
   // Load 0x00 to the TXD7 register
   LoadCanRegister(CAN_TXD7, 0x00) ;
   if (MainCounter==0x0F) {
      MainCounter = 0x00 ;
   } else {
      MainCounter++ ;
   }
}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
void TransmitFrame(void) {
   LoadCanRegister(CAN_CMR, 0x01) ;
}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
void InitInterrupts(void) {
   EX0 = 1 ;
   IT0 = 1 ;
   EAL = 1 ;
}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */
void WaitLong(void) {
   for (unsigned char i=0xFE; i> 0x00; i--) {
      for(unsigned char j=0xFF; j>0x00; j--) {
         for(unsigned char k=0xFF; k>0x00; k--) {
            __asm ("nop") ;
         }
      }
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void main (void) {
   // Initialize LCD
   InitLCD();
   // Initialize CAN
   InitCAN();
   // Enable interrupts
   InitInterrupts();

    while (1) {
      DisplayMsg_TX(MainCounter);
      DisplayMsg_RX(P1FD1R);
      SetFrameToTransmit();
      TransmitFrame();
      WaitLong();
   }
}
/* ------------------------------------------------------------------------ */
