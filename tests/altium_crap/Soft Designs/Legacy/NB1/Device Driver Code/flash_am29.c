#include "flash_am29.h"

//........................ dependent functions .................................
// must include processor dependent files for these functions e.g flash_am29_ppc405cr.c
extern void write_half_word (unsigned short int* abs_address, unsigned short int data);
extern void wait_until_ready();
//..............................................................................
                                                                                    
//..............................................................................
void am29_write_half_word(unsigned short int* base_address, unsigned short int* abs_address, unsigned short int data)
{
    write_half_word(base_address + 0x555, 0xAA); // issue write command
    write_half_word(base_address + 0x2AA, 0x55);
    write_half_word(base_address + 0x555, 0xA0);
    write_half_word(abs_address         , data);// issue write data
    wait_until_ready();
}
//..............................................................................

//..............................................................................
void am29_erase_sector(unsigned short int* base_address, unsigned short int* abs_address)
{
    unsigned int erase_address = ((unsigned int) abs_address & 0x7FFFFF) >> 1;

    write_half_word(base_address + 0x555        , 0xAA); // issue write command
    write_half_word(base_address + 0x2AA        , 0x55);
    write_half_word(base_address + 0x555        , 0x80);
    write_half_word(base_address + 0x555        , 0xAA);
    write_half_word(base_address + 0x2AA        , 0x55);
    write_half_word(base_address + erase_address, 0x30);  // << 1 is due to last bit of bus not connected to flash
    wait_until_ready();
}
//..............................................................................

//..............................................................................
void am29_erase_chip(unsigned short int* base_address)
{
    write_half_word(base_address + 0x555, 0xAA); // issue write command
    write_half_word(base_address + 0x2AA, 0x55);
    write_half_word(base_address + 0x555, 0x80);
    write_half_word(base_address + 0x555, 0xAA);
    write_half_word(base_address + 0x2AA, 0x55);
    write_half_word(base_address + 0x555, 0x10);
    wait_until_ready();
}
//..............................................................................


//..............................................................................
// sectors are of sise 0x800 x 2 bytes (x 2 because it's a 16 bit device)
int am29_sector_number(unsigned short int* abs_address)
{
    unsigned int sector_address = (unsigned int) abs_address;
    sector_address >>= 16;
    sector_address &= 0x7F;
    return sector_address;
}
//..............................................................................
