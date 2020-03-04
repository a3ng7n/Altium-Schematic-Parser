/********************************************************************\
|*
|* Version : 1.0
|*
|* Copyright : Copyright (C) 2006, Altium
|*
|* Description : This project flashes the Status Leds of the Logic
|*               Zoom development board.
|*
\********************************************************************/


/*
* Registers definitions
*/
#define GPIO2Base                       0xFFFDD000
#define GPIO2_BASE(base)                ((volatile unsigned char *) base)
#define GPIO2_PFDR(base)                GPIO2_BASE(base)[0x04]  // Port F Data Register
#define GPIO2_PFDDR(base)               GPIO2_BASE(base)[0x0C]  // Port F Data Direction Register


/***********************************************************************
|*
|* Function : delay
|*
|* Parameters :
|*
|* Returns :
|*
|* Description : Creates a delay
*/

void delay (void)
{                         /* Wait function */
  unsigned long i, j;

  for ( i = 0 ; i < 2048 ; i++ )
  {
      for ( j = 0 ; j < 2048 ; j++ )
      {
          __asm("NOP");
      }
  }
}


/***********************************************************************
|*
|* Function : main
|*
|* Parameters :
|*
|* Returns :
|*
|* Description : main function
*/

void main (void)
{
    // Trun Off Status Led 0 & 1 - PFDR.1 = 1 & PFDR.2 = 1
    GPIO2_PFDR(GPIO2Base)  = 0x06;
    // Set bits 1 & 2 of Port F in Output mode
    GPIO2_PFDDR(GPIO2Base) = 0x06;

    while (1)
    {
        // Trun Off Status Led 1 - PFDR.2 = 1
        GPIO2_PFDR(GPIO2Base) |=  0x04;
        // Trun On  Status Led 0 - PFDR.1 = 0
        GPIO2_PFDR(GPIO2Base) &= ~0x02;
        delay();
        // Trun Off Status Led 0 - PFDR.1 = 1
        GPIO2_PFDR(GPIO2Base) |=  0x02;
        // Trun On  Status Led 1 - PFDR.2 = 0
        GPIO2_PFDR(GPIO2Base) &= ~0x04;
        delay();
    }
}
