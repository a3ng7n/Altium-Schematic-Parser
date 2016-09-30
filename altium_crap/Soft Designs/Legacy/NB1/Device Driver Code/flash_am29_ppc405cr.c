#include "flash_am29.h"


//......................... Functions ..........................................
void write_half_word (unsigned short int* abs_address, unsigned short int data);
void wait_until_ready(void);
//..............................................................................

//..............................................................................
void write_half_word(unsigned short int* abs_address, unsigned short int data)
{
    __asm("sth %0,0(%1)" : :"r"(data),"r"(abs_address): );
}
//..............................................................................

//..............................................................................
void wait_until_ready()
{
    volatile unsigned int* status = (volatile unsigned int*) 0xEF60071C;
    while ((*status & 0x00400000) == 0)
    {
    }
}
//..............................................................................
