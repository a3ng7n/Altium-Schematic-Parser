#include "string.h"
#include "def.h"
#include "i2c.h"
//#define DEBUG_INFO

unsigned char interrupt_counter = 0 ;
/* ------------------------------------------------------------------------ */
/*
   Interrupt 0 handler.
*/
__interrupt(0*8+3) void Interrupt0Handler( void ) {
   EAL = 0 ;
   I2CInterruptServiceRoutine();
   EAL = 1 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Interrupt 1 handler.
*/
__interrupt(2*8+3) void Interrupt1Handler( void ) {
   EAL = 0 ;
   //P0 = 0x99 ;
   KeypadInterruptServiceRoutine();
   EAL = 1 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Set i2c_int bit to let the software the i2c master has executed our
   command.
*/
void I2CInterruptServiceRoutine(void) {
   i2c_int = 1 ;
   return ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   TODO : clean it up.
*/
unsigned char KeypadInterruptServiceRoutine(void) {
   unsigned char key = '0' ;
   //P0 = 0x88 ;
   key = KEYPAD_REG ;
   interrupt_counter++ ;
   //P0 = interrupt_counter ;
//   switch (key) {
//      case 0x00 : return '1' ;
//      case 0x01 : return '2' ;
//      case 0x02 : return '3' ;
//      case 0x03 : return 'C' ;
//      case 0x04 : return '4' ;
//      case 0x05 : return '4' ;
//      case 0x06 : return '6' ;
//      case 0x07 : return 'D' ;
//      case 0x08 : return '7' ;
//      case 0x09 : return '8' ;
//      case 0x0A : return '9' ;
//      case 0x0B : return 'E' ;
//      case 0x0C : return 'A' ;
//      case 0x0D : return '0' ;
//      case 0x0E : return 'B' ;
//      case 0x0F : return 'F' ;
//   }
//   return '-' ;

   switch (key) {
      case 0x00 : { InputCharacter = '1' ;   break ;} ;
      case 0x01 : { InputCharacter = '2' ;   break ;} ;
      case 0x02 : { InputCharacter = '3' ;   break ;} ;
      case 0x03 : { InputCharacter = 'C' ;   break ;} ;
      case 0x04 : { InputCharacter = '4' ;   break ;} ;
      case 0x05 : { InputCharacter = '5' ;   break ;} ;
      case 0x06 : { InputCharacter = '6' ;   break ;} ;
      case 0x07 : { InputCharacter = 'D' ;   break ;} ;
      case 0x08 : { InputCharacter = '7' ;   break ;} ;
      case 0x09 : { InputCharacter = '8' ;   break ;} ;
      case 0x0A : { InputCharacter = '9' ;   break ;} ;
      case 0x0B : { InputCharacter = 'E' ;   break ;} ;
      case 0x0C : { InputCharacter = 'A' ;   break ;} ;
      case 0x0D : { InputCharacter = '0' ;   break ;} ;
      case 0x0E : { InputCharacter = 'B' ;   break ;} ;
      case 0x0F : { InputCharacter = 'F' ;   break ;} ;
   }

   if (my_str_cnt == 4) {
      my_str_cnt = 0 ;
   } else {
      my_str[my_str_cnt] = InputCharacter ;
      //P0 = InputCharacter ;
      my_str_cnt++ ;
   }

   DisplayDacString() ;

   return 0 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void DisplayFirstLine(void) {
   DisplayLine("Input 'V': ", LCD_LINE0, 0, 11) ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Returns LCD Busy flag.
   If the LCD indicates its busy then we cannot display anything.
*/
__bit GetLcdBusyFlag(void) {
   return LCD_CTRL_REG_bit(6) ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Puts a character on the LCD panel at given position. Takes three arguments
   c is an ASCII coded character, line selects which line, 0 for upper line and
   1 for bottom line and position gives coordinate within the line. 0 is for
   the most left position and goes up to 15 which is the most right character
   in line.
*/
void DisplayCharacter(  unsigned char c,
                        unsigned char line,
                        unsigned char position
                      ) {
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
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Displays string on the LCD panel. Takes four arguments. First is pointer
   to string, then line, starting position in line and the length
   of the string. Uses DisplayCharacter function to put a single character
   from the string on the LCD panel.
*/
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
/*
   Address slave device on I2C bus. Generates START condition followed by
   slave's address and direction bit. For to slave communication dir bit
   is cleared and set when addressing for reading from a slave device.
   Waits for an acknowledge from addressed device and returns 0 if successful.
   If no device responds with an acknowledge then there is no device with
   this address and the function returns 1.
*/
__bit AddressI2CSlave(unsigned char adr, unsigned char dir) {
   unsigned char cmd = 0 ;

   i2c_int = 0 ;
   cmd = adr & dir ;
   I2C_WRIT_REG = cmd ;
   cmd = I2C_CTRL_EN | I2C_CTRL_START | I2C_CTRL_IEN | I2C_CTRL_WR ;
   I2C_CTRL_REG = cmd ;
   while(!i2c_int){
      ;
   }
   //if (!I2C_STAT_REG_bit(I2C_STAT_RXACK)) {
   if (!I2C_STAT_REG_bit(1)) {
      return 0 ;
   } else {
      return 1 ;
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Generate classic STOP condition on I2C bus.
*/
void SendI2CStop(void) {
   unsigned char cmd = 0 ;
   i2c_int = 0 ;
   cmd = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_STOP ;
   I2C_CTRL_REG = cmd ;
   while (!i2c_int) {
      ;
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Send data byte to I2C slave. If last is set then I2C master will
   automatically generate STOP condition after sending the data.
*/
void SendI2CByte( unsigned char  data,
                  __bit          last
                )
{
   unsigned char cmd = 0 ;

   i2c_int = 0 ;
   if (last) {
      cmd = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_WR | I2C_CTRL_STOP ;
   } else {
      cmd = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_WR ;
   }
   I2C_WRIT_REG = data ;
   I2C_CTRL_REG = cmd ;
   while(!i2c_int) {
      ;
   }
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/*
   Get data byte from I2C slave. If last is set then I2C master will
   automatically generate two STOP conditions on I2C bus after completing
   data transfer from a slave device. First is not acknowledge and then
   classic STOP condition.
*/
unsigned char GetI2CByte( __bit last ) {
   unsigned char cmd = 0 ;

   i2c_int = 0 ;
   if (last) {
      cmd = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_RD | I2C_CTRL_ACK | I2C_CTRL_STOP ;
   } else {
      cmd = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_RD ;
   }
   I2C_CTRL_REG = cmd ;
   while (!i2c_int) {
      ;
   }
   return I2C_READ_REG ;
}
/* ------------------------------------------------------------------------ */




/* ------------------------------------------------------------------------ */
void InitI2C(void) {
   I2C_CLK0_REG = 0x0E ;   // 0x0E gives 400kHz sclk for 30 MHz
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void PowerUpDAC(void) {
   unsigned char cmd = 0 ;



   cmd = DAC_CMD_PowerUp ;
   I2C_WRIT_REG = cmd ;

   cmd = I2C_CTRL_EN | I2C_CTRL_IEN | I2C_CTRL_WR ;
   I2C_CTRL_REG = cmd ;

   while(!i2c_int) {
      ;
   }

   cmd = DAC_CMD_PowerDown ;
   //I2C_WRIT_REG =

}
/* ------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void InitInterrupts(void) {
   EAL = EX0 = EX1 = IT0 = IT1 = 1 ;
}
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
void DisplayDacString(void) {
   unsigned char pos = 15 ;
   DisplayFirstLine() ;
   for(unsigned char i = my_str_cnt ; i >= 0 ; i--) {
      P0 = 0x55 ;
      switch (i) {
         case 0 : pos = 15 ; break ;
         case 1 : pos = 14 ; break ;
         case 2 : pos = 13 ; break ;
         case 3 : pos = 12 ; break ;
         case 4 : pos = 11 ; break ;
      }
      DisplayCharacter(my_str[i], LCD_LINE0, pos) ;
   }
   P0 = 0x88 ;
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
   DisplayFirstLine();
   InitInterrupts() ;
   // Initialize CAN

   //
    while (1) {
//      P0 = InputCharacter ;
//      DisplayDacString() ;
      __asm ("nop") ;
      __asm ("nop") ;
      __asm ("nop") ;
      __asm ("nop") ;
      __asm ("nop") ;
//      P0 = InputCharacter ;
   }
}
/* ------------------------------------------------------------------------ */