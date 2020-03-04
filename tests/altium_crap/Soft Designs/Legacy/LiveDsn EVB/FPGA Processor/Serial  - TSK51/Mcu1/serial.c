#include "serial.h"

//--------------------------------
// Write serial port register
//--------------------------------

#define FOSC 50000000L  // Oscillator frequency

//----------------------------------------------------
// Initialise serial  port
void serial_init(char speed)
{
    TR1 = 0;          // stop timer 1
    TMOD &=0x0F;
    TMOD |= 0x20;     // timer 1 is in mode 2: 8 bit auto reload
    PCON |= 0x80;     // Baud rate is 1/32th FOSC

    SM1 = 1;          // 8 bits, variable
    REN = 1;          // Enable Serial reception

    //Initialise serial comms
    switch (speed)
    {
           case 0:  TH1 = (unsigned char) (256-(((FOSC/192.0)/(float)1200)));        break;
           case 1:  TH1 = (unsigned char) (256-(((FOSC/192.0)/(float)2400)));        break;
           case 2:  TH1 = (unsigned char) (256-(((FOSC/192.0)/(float)4800)));        break;
           case 3:  TH1 = (unsigned char) (256-(((FOSC/192.0)/(float)9600)));        break;
           case 4:  TH1 = (unsigned char) (256-(((FOSC/192.0)/(float)19200)));       break;
    }
    TR1 = 1;          // enable timer 1
}

char serial_getch(void)
{
    char c=0;
    if(RI) // Character received
    {
          c = SBUF;
          RI = 0;
     }
    return c;
}

void serial_putch(char c)
{
     SBUF = c;
     while (TI == 0)    // wait for TI flag to come true
     {

     }
     TI = 0;            // clear flag again
}

void serial_puts(char __rom *p)
{
   while (*p != 0)
   {
           serial_putch(*p);
           p++;
   }
}
