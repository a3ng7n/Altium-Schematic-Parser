
#include "hardware.h"
#include "proc_bluestreak_arm7_startup.h"

//..............................................................................
#define Base_SDRAM                  0x20000000
#define Size_SDRAM                  0x04000000
//..............................................................................

unsigned int g_error_count = 0;

//..............................................................................
void write_memory(unsigned int* mem_loc, unsigned int mem_size, unsigned int mem_value)
{
    for (unsigned int i = 0; i < mem_size; i++)
    {
        mem_loc[i] = mem_value;
    }
}
//..............................................................................

//..............................................................................
unsigned int read_memory(unsigned int* mem_loc)
{
    return mem_loc[0];
}
//..............................................................................

void write_sdram(unsigned int* mem_loc, unsigned int mem_size, unsigned int mem_value)
{
    for (unsigned int i = 0; i < mem_size; i++)
    {
        mem_loc[i] = mem_value++;
    }
}
//..............................................................................

void write_Port(unsigned char* mem_loc, unsigned char mem_value)
{
        mem_loc[0] = mem_value;
}
//..............................................................................

//..............................................................................
unsigned int memory_test(unsigned int* mem_loc, unsigned int mem_size)
{
    unsigned int j = 0;

    write_Port((unsigned char*)Base_LEDS, 0);
    for (unsigned int i = 0; i < (mem_size/4); i++)
    {
        mem_loc[i] = j;
        if (j != mem_loc[i])
        {
            g_error_count++;
        }
        write_Port((unsigned char*)Base_LEDS, i);
        if (j == 0xFFFFFFFF)
        {
            j = 0;
        }
        else
        {
            j += 0x01010101;
        }
    }
    return g_error_count;
}
//..............................................................................

//..............................................................................
void main()
{
    startup();

    while (1)
    {
        memory_test(0x200,0x100);
        memory_test(Base_DB_RAM, Size_DB_RAM);
        memory_test(Base_NB_RAM, Size_NB_RAM);
        memory_test(Base_BRAM, Size_BRAM);
        memory_test(Base_SDRAM,Size_SDRAM);
    }
}
//..............................................................................


