#include <stdio.h>
#include <stdint.h>
#include <timing.h>

#include "hardware.h"

#define SDRAM_START 0x04000000
#define SDRAM_SIZE  0x4000000
#define SRAM_START  0x0C000000
#define SRAM_SIZE   0x100000
#define BRAM_START  0x08000000
#define BRAM_SIZE   0x1000

uint64_t time_start;
uint64_t time_write;
uint64_t time_read;
volatile uint32_t mem_word;
volatile uint16_t mem_halfword;
volatile uint8_t  mem_byte;

void test_32bit_access(uint32_t start, uint32_t size);
void test_16bit_access(uint32_t start, uint32_t size);
void test_8bit_access(uint32_t start, uint32_t size);
float get_bandwidth(uint64_t ticks, uint32_t size);

void main (void)
{
    float bw;

    printf("SDRAM bandwidth test.\n");
    printf("32-bit access benchmark\n");
    test_32bit_access(SDRAM_START, SDRAM_SIZE);
    bw = get_bandwidth(time_write, SDRAM_SIZE);
    printf("32-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, SDRAM_SIZE);
    printf("32-bit read: %f kB per sec \n", bw);

    printf("BRAM bandwidth test.\n");
    printf("32-bit access benchmark\n");
    test_32bit_access(BRAM_START, BRAM_SIZE);
    bw = get_bandwidth(time_write, BRAM_SIZE);
    printf("32-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, BRAM_SIZE);
    printf("32-bit read: %f kB per sec \n", bw);

    printf("SRAM bandwidth test.\n");
    printf("32-bit access benchmark\n");
    test_32bit_access(SRAM_START, SRAM_SIZE);
    bw = get_bandwidth(time_write, SRAM_SIZE);
    printf("32-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, SRAM_SIZE);
    printf("32-bit read: %f kB per sec \n", bw);

    printf("SDRAM bandwidth test.\n");
    printf("16-bit access benchmark\n");
    test_16bit_access(SDRAM_START, SDRAM_SIZE);
    bw = get_bandwidth(time_write, SDRAM_SIZE);
    printf("16-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, SDRAM_SIZE);
    printf("16-bit read: %f kB per sec \n", bw);

    printf("BRAM bandwidth test.\n");
    printf("16-bit access benchmark\n");
    test_16bit_access(BRAM_START, BRAM_SIZE);
    bw = get_bandwidth(time_write, BRAM_SIZE);
    printf("16-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, BRAM_SIZE);
    printf("16-bit read: %f kB per sec \n", bw);

    printf("SRAM bandwidth test.\n");
    printf("16-bit access benchmark\n");
    test_16bit_access(SRAM_START, SRAM_SIZE);
    bw = get_bandwidth(time_write, SRAM_SIZE);
    printf("16-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, SRAM_SIZE);
    printf("16-bit read: %f kB per sec \n", bw);

    printf("SDRAM bandwidth test.\n");
    printf("8-bit access benchmark\n");
    test_8bit_access(SDRAM_START, SDRAM_SIZE);
    bw = get_bandwidth(time_write, SDRAM_SIZE);
    printf("8-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, SDRAM_SIZE);
    printf("8-bit read: %f kB per sec \n", bw);

    printf("BRAM bandwidth test.\n");
    printf("8-bit access benchmark\n");
    test_8bit_access(BRAM_START, BRAM_SIZE);
    bw = get_bandwidth(time_write, BRAM_SIZE);
    printf("8-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, BRAM_SIZE);
    printf("8-bit read: %f kB per sec \n", bw);

    printf("SRAM bandwidth test.\n");
    printf("8-bit access benchmark\n");
    test_8bit_access(SRAM_START, SRAM_SIZE);
    bw = get_bandwidth(time_write, SRAM_SIZE);
    printf("8-bit write: %f kB per sec \n", bw);
    bw = get_bandwidth(time_read, SRAM_SIZE);
    printf("8-bit read: %f kB per sec \n", bw);

    printf("\n\nBenchmark finished.\n");

}


void test_32bit_access(uint32_t start, uint32_t size)
{
    uint32_t i;

    time_start = clock_ns();
    for (i = 0; i < size; i+=4)
    {
        *(volatile uint32_t*)(start + i) = i;
    }
    time_write = elapsed_time_ns(time_start);

    time_start = clock_ns();
    for (i = 0; i < size; i +=4)
    {
        mem_word = *(volatile uint32_t*)(start + i);
    }
    time_read = elapsed_time_ns(time_start);
}

float get_bandwidth(uint64_t ticks, uint32_t size)
{
    float result;
    float seconds;
    float bytes;
    float tick;
    seconds = ticks / 1000000000.0;
    bytes = size / 1024.0;
    return (bytes / seconds);
}

void test_16bit_access(uint32_t start, uint32_t size)
{
    uint32_t i;

    time_start = clock_ns();
    for (i = 0; i < size; i+=2)
    {
        *(volatile uint16_t*)(start + i) = i;
    }
    time_write = elapsed_time_ns(time_start);

    time_start = clock_ns();
    for (i = 0; i < size; i +=2)
    {
        mem_halfword = *(volatile uint16_t*)(start + i);
    }
    time_read = elapsed_time_ns(time_start);
}


void test_8bit_access(uint32_t start, uint32_t size)
{
    uint32_t i;

    time_start = clock_ns();
    for (i = 0; i < size; i++)
    {
        *(volatile uint8_t*)(start + i) = i;
    }
    time_write = elapsed_time_ns(time_start);

    time_start = clock_ns();
    for (i = 0; i < size; i++)
    {
        mem_byte = *(volatile uint8_t*)(start + i);
    }
    time_read = elapsed_time_ns(time_start);
}

