#ifndef __flash_am29_h__
#define __flash_am29_h__


void am29_write_half_word(unsigned short int* base_address, unsigned short int* abs_address, unsigned short int data);     // short int = 16 bits
void am29_erase_sector   (unsigned short int* base_address, unsigned short int* abs_address);
void am29_erase_chip     (unsigned short int* base_address);                                                               // takes 115 sec
int  am29_sector_number  (unsigned short int* abs_address);
#endif
