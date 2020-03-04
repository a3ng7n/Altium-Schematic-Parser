#include "Interrupt0.h"



// INT 0 /////////////////////////////////////////////////

//__interrupt(0*8+3) void Interrupt0IntHandler( void )
//{
//    Interrupt0();
//}


void InitInterrupt0( void )
{
    IT0 = 1;
    EX0 = 1;    /* Enable Int0 interrupt */

}

void EnableInt0Interrupt( void )
{
    EX0 = 1;    /* Enable timer 0 interrupt */
}

void DisableInt0Interrupt( void )
{
    EX0 = 0;    /* Disable Int 0 interrupt */
}



// Timer 0 /////////////////////////////////////////////////

//__interrupt(2*8+3) void Timer0IntHandler( void )
//{
//    Timer0Interrupt();
//}

__interrupt(9*8+3) void Interrupt0IntHandler( void )
{
   //DISABLE_INTERRUPTS();
    Interrupt0();
}



__interrupt(10*8+3) void Timer0IntHandler( void )
{
   //DISABLE_INTERRUPTS();
    Timer0Interrupt();
}


//void InitTimer0(WORD hzFreq )
//{
//    BYTE temp;
//    TMOD               = 0x02;   // Timer0 Mode 2
////    TH0                = MODE_2_TIMER(hzFreq);
//    temp                = MODE_2_TIMER(22050);
//    TH0                = temp;
//    TR0                = 1;   //  Enable Timer0
//    ET0                = 1;   //  Enable Timer0 interrupt
//}

void InitTimer0(WORD hzFreq )
{
    BYTE temp;
      // enable int1

//    IE1 = 1 ;

//    IT1 = 1 ;
//    EX1 = 1 ;

   //EX3 = 1 ;
      // end enable int1

    SetTimerMode();
    temp                = MODE_2_TIMER(22050);
    //temp = 150 ;  // dla 30 MHz out freq is ~528 Hz
    //temp = 140 ;
    LoadTimer(temp) ;
    EnableTimer();

}


void SetTimerMode(void) {
   P1 = 0x20 ;
   P2 = 0x01 ;
   P2 = 0x81 ;
   P2 = 0x01 ;
}

void LoadTimer(BYTE ld) {
   P1 = ld ;
   P2 = 0x05 ;
   P2 = 0x85 ;
   P2 = 0x05 ;
}

void EnableTimer(void) {
   P1 = 0x40 ;
   P2 = 0x00 ;
   P2 = 0x80 ;
   P2 = 0x00 ;
}


void EnableTimer0Interrupt( void )
{
    IT1 = 1 ;    /* Enable timer 0 interrupt */
    EX1 = 1 ;
}

void DisableTimer0Interrupt( void )
{
    IT1 = 0 ;    /* Disable Int 0 interrupt */
    EX1 = 0 ;
}


