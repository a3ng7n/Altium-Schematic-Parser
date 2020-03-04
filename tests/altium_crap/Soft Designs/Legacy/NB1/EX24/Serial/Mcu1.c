#include "def.h"


/* ------------------------------------------------------------------------ */
__interrupt (_INT0) void KeyPadInterruptHandler (void) {
   EAL = 0 ;
   KeyPadInterrupt() ;
   EAL = 1 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
__interrupt (_INT1) void SerialInterruptHandler (void) {
   EAL = 0 ;
   SerialInterrupt() ;
   EAL = 1 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void KeyPadInterrupt(void) {
   key_VAL = GetWbReg(KPAD) ;
   LoadWbReg(KPAD, 0);
   key_ON = 1 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void SerialInterrupt(void) {
   unsigned char scon ;
   unsigned char scon_0 ;
   unsigned char scon_1 ;

   scon = GetWbReg(S_SCON) ;
   scon_0 = scon & 0x01 ;
   scon_1 = scon & 0x02 ;

   if(scon_1) {
      AllowTransmit = 1 ;
      LoadWbReg(S_SCON, scon&0xFC) ;// clear bit 0 and 1
   } else {
      if(scon_0 && UnblockRec) {
         if (!RecDatCount) {
            ClearLcd() ;
         }
         Text[RecDatCount] = GetWbReg(S_SBUF) ;
         DisplayCharOnLcd(RecDatCount, Text[RecDatCount] ) ;
         RecDatCount++ ;
         LoadWbReg(S_SCON, scon&0xFC) ;
         if (RecDatCount==TEXTSIZE-1) {
            RecDatCount = 0 ;
         }
      }
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
__bit GetLcdBusyFlag(void) {
   return LCD_CTRL_bit(6) ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void DisplayCharOnLcd(  unsigned char pos,
                        unsigned char c
                      ) {

   while (GetLcdBusyFlag()==1) {
      ;
   }
   LCD_DAT = c ;
   LCD_CTRL = LCD_STROBE | pos ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void DisplayMessageOnLcd(  unsigned char line,
                           __rom KCODE unsigned char * msg
                         ) {

   unsigned char pos ;
   register unsigned char c ;

   pos = line*16 ;
   while( (c=*msg++) ) {
      DisplayCharOnLcd(pos, c) ;
      pos++ ;
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void InitializeLcd(void) {
   DisplayMessageOnLcd(0, W_Message) ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void ClearLcd(void) {
   DisplayMessageOnLcd(0, E_Line) ;
   for (unsigned char i = 0 ; i<32; i++) {
      while(GetLcdBusyFlag()==1) {
         ;
      }
      LCD_DAT = ' ' ;
      LCD_CTRL = LCD_STROBE | i ;
   }
}
/* ------------------------------------------------------------------------ */

/* --------------------------------------------------------------------- */
void LoadWbReg(   unsigned char reg,
                  unsigned char val
              ) {
   switch(reg) {
      case KPAD      :
                        KPAD_DAT     = val ;
                        break ;
      case L_DAT     :
                        LCD_DAT      = val ;
                        break ;
      case L_CTRL    :
                        LCD_CTRL     = val ;
                        break ;
      case S_PCON    :
                        SERIAL_PCON  = val ;
                        break ;
      case S_SCON    :
                        SERIAL_SCON  = val ;
                        break ;
      case S_SBUF    :
                        SERIAL_SBUF  = val ;
                        break ;
      case S_SRELL   :
                        SERIAL_SRELL = val ;
                        break ;
      case S_SRELH   :
                        SERIAL_SRELH = val ;
                        break ;
      case S_TCON    :
                        SERIAL_TCON  = val ;
                        break ;
      case S_TL      :
                        SERIAL_TL    = val ;
                        break ;
      case S_TH      :
                        SERIAL_TH    = val ;
                        break ;
      case S_ADCON   :
                        SERIAL_ADCON = val ;
                        break ;
      default        :
                        break ;
   }
}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
unsigned char GetWbReg(unsigned char reg) {
   switch(reg) {
      case KPAD      :
                        return KPAD_DAT ;
      case L_DAT     :
                        return LCD_DAT ;
      case L_CTRL    :
                        return LCD_CTRL ;
      case S_PCON    :
                        return SERIAL_PCON ;
      case S_SCON    :
                        return SERIAL_SCON ;
      case S_SBUF    :
                        return SERIAL_SBUF ;
      case S_SRELL   :
                        return SERIAL_SRELL ;
      case S_SRELH   :
                        return SERIAL_SRELH ;
      case S_TCON    :
                        return SERIAL_TCON ;
      case S_TL      :
                        return SERIAL_TL ;
      case S_TH      :
                        return SERIAL_TH ;
      case S_ADCON   :
                        return SERIAL_ADCON ;
      default        :
                        return 0x00 ;
   }

}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
void TransmitData(void) {
   for (unsigned char i = 0; i<RecDatCount; i++) {
      LoadWbReg(S_SBUF, Text[i] ) ;
      while (!AllowTransmit) {
         ;
      }
      AllowTransmit = 0 ;
   }
}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
void InitDataMemW(void) {
   for (unsigned char i = 0; i<TEXTSIZE; i++) {
      Text[i] = '-' ;
   }
}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
void ScanKeypadAndDisplayMessage(void) {
   if (key_ON) {
      switch (key_VAL) {
         case KEY_1 :
            ClearLcd() ;
            DisplayMessageOnLcd(0, S_Message) ;
            RecDatCount = 32 ;
            UnblockRec = 0 ;
            TransmitData() ;
            break ;

         case KEY_2 :
            if (!UnblockRec) {
               UnblockRec = 1 ;
               ClearLcd() ;
               DisplayMessageOnLcd(0, R_Message) ;
               RecDatCount = 0 ;
            }
            break ;

         case KEY_3 :
            ClearLcd() ;
            InitDataMemW() ;
            RecDatCount = 0 ;
            UnblockRec = 0 ;
            break ;
      }
   }
   key_ON = 0 ;
}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
void InitializeSerial(void) {

   LoadWbReg(S_SRELL, SRELL_INIT) ;
   LoadWbReg(S_SRELH, SRELH_INIT) ;
   LoadWbReg(S_SCON, SCON_INIT) ;
   LoadWbReg(S_PCON, PCON_INIT) ;
   LoadWbReg(S_ADCON, ADCON_INIT) ;

}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
void InitializeInterrupts(void) {
   EX0 = 1 ;
   IT0 = 1 ;
   EX1 = 1 ;
   IT1 = 1 ;
   EAL = 1 ;
}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
void Initialize(void) {
   InitializeInterrupts() ;
   InitializeLcd() ;
   InitializeSerial() ;
   InitDataMemW() ;
}
/* --------------------------------------------------------------------- */

/* --------------------------------------------------------------------- */
void main (void) {
   Initialize() ;
   while (1) {
      ScanKeypadAndDisplayMessage() ;
   }
}
/* --------------------------------------------------------------------- */
