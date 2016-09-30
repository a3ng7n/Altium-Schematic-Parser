#include "Interrupt0.h"



// INT 0 /////////////////////////////////////////////////

__interrupt(0*8+3) void Interrupt0IntHandler( void )
{
    Interrupt0();
}


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


__interrupt(2*8+3) void Interrupt1IntHandler( void )
{
    Interrupt1();
}


void InitInterrupt1( void )
{
    IT1 = 1;
    EX1 = 1;    /* Enable Int0 interrupt */ 
}

void EnableInt1Interrupt( void )
{
    EX1 = 1;    /* Enable timer 0 interrupt */  
}

void DisableInt1Interrupt( void )
{
    EX1 = 0;    /* Disable Int 0 interrupt */   
}


// Timer 0 /////////////////////////////////////////////////

__interrupt(1*8+3) void Timer0IntHandler( void )
{
//   TH0        =        2;
    
    Timer0Interrupt();
}


void InitTimer0(WORD hzFreq )
{
    BYTE temp;
    TMOD               = 0x02;   // Timer0 Mode 2
//    TH0                = MODE_2_TIMER(hzFreq);
    temp                = MODE_2_TIMER(22050);
    TH0                = temp;
    TR0                = 1;   //  Enable Timer0
    ET0                = 1;   //  Enable Timer0 interrupt
}

void EnableTimer0Interrupt( void )
{
    ET0 = 1;    /* Enable timer 0 interrupt */  
}

void DisableTimer0Interrupt( void )
{
    ET0 = 0;    /* Disable Int 0 interrupt */   
}


