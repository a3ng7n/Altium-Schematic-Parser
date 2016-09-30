/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   Example of lcd_ks0066u and keypad_4x4 driver use
|*
\*****************************************************************************/

#include <stdio.h>
#include "util_timing.h"
#include "keypad_4x4.h"
#include "lcd_ks0066u.h"
#include "io_lcd_ks0066u.h"
#include "hardware.h"
#include "nb_spi.h"
#include "wb_spi.h"
#include "flash_m25px0.h"

#define MEM_SIZE 0x2000 // 8k
#define FLASH_START_ADDRESS 0x00

int _write ( int fd, const char * buf, int size )
{
    return lcd_ks0066u_write(Base_LCD, buf, size);
}

int _read ( int fd, char *buf, int size )
{
    return keypad_4x4_read (Base_KEYPAD, buf, size);
}



#pragma section "critical"
void acquire_flash_device(void)
{
    nanoboard_spi_open(Base_Nanoboard_SPI,DEVICE_M25P40_EMBEDDED);
    while (m25px0_read_electronic_signature(Base_Nanoboard_SPI) == 0)
    {
        nanoboard_spi_close(Base_Nanoboard_SPI);
        nanoboard_spi_open (Base_Nanoboard_SPI,DEVICE_M25P40_EMBEDDED);
    }
}

void copy_program_image_from_flash(void)
{
    int i;
    acquire_flash_device();
    m25px0_open_read   (Base_Nanoboard_SPI, FLASH_START_ADDRESS);
    for (i = 0; i < MEM_SIZE; i++)
    {
        (*(volatile unsigned char*) (Base_SRAM_NB_ROM + i)) = spi_send_wait_receive(Base_Nanoboard_SPI, 0);
    }
    nanoboard_spi_close(Base_Nanoboard_SPI);
}

void initialize ( void )
{
    timing_set_clock_freq_hz (20 * 1000 * 1000);
    copy_program_image_from_flash();
    lcd_ks0066u_init (Base_LCD);
    lcd_ks0066u_clear_screen (Base_LCD);
    keypad_4x4_reset (Base_KEYPAD);
    keypad_4x4_set_repeat_delay_primary_ms (500);
    keypad_4x4_set_repeat_delay_secondary_ms (100);
}

void main (void)
{
    char ch;
    initialize();
    setbuf( stdout, NULL ); // Disables line buffering on stdout
    setbuf( stdin, NULL );  // Disables line buffering on stdin
    while (1) {
        ch = getchar();
        if (ch != EOF)
        {
            putchar(ch);
        }
    }
}
#pragma endsection

