//..............................................................................
#include "hardware.h"
#include "proc_ppc405cr.h"
#include "util_timing.h"
#include "wb_spi.h"
#include "codec_max1104.h"
#include "flash_am29.h"
#include "clock_ics307.h"
#include "nb_spi.h"
#include "lcd_ks0066u.h"
#include "io_lcd_ks0066u.h"
#include "keypad_4x4.h"
//..............................................................................

//..............................................................................
#define CLOCK_HZ (200 * 1000 * 1000)
#define SOUND_HZ 20000
//..............................................................................

//..............................................................................
typedef enum {eMain, ePlaying, eErasing, eRecording, eEchoing, eWriting} mode_t;
mode_t mode;
unsigned char Amplitude    = 0;
unsigned int  PlayLocation = 0;
//..............................................................................

//..............................................................................
#define RECORD_BUFFER ((unsigned char *) Base_Daughter_Integrated_SDRAM)
unsigned int BufferOffset;
static int   BufferLength = 0x800000; // using 8MB of flash!
//..............................................................................

//..............................................................................
void set_audio_sample_rate(unsigned int SampleFrequency)
{
    ppc405cr_set_programmable_interval_timer(timing_get_clock_freq_hz () / SampleFrequency);
}
//..............................................................................

//..............................................................................
void set_nanoboard_clock_frequency(int FrequencyIndex)
{
    nanoboard_spi_open      (Base_NanoBoard_SPI,DEVICE_CLOCK);
    ics307_program_frequency(Base_NanoBoard_SPI,FrequencyIndex);
    nanoboard_spi_close     (Base_NanoBoard_SPI);
}
//..............................................................................

//..............................................................................
void initialize(void)
{
    ppc405cr_set_exception_vector_prefix(0);
    ppc405cr_disable_interrupts();
    ppc405cr_disable_programmable_interval_timer();
    timing_set_clock_freq_hz(CLOCK_HZ);
    set_nanoboard_clock_frequency(ICS307_40_MHz);
    set_audio_sample_rate(SOUND_HZ);
    lcd_ks0066u_init(Base_LCD);
    keypad_4x4_reset(Base_Keypad);
    keypad_4x4_set_enable_repeat(0);
    mode = eMain;
}
//..............................................................................

//..............................................................................
void update_display(void)
{
    lcd_ks0066u_clear_screen (Base_LCD);
    switch (mode)
    {
      case eMain:      lcd_ks0066u_output_string(Base_LCD, 0, "1-Play  2-Record");
                       lcd_ks0066u_output_string(Base_LCD, 1, "3-Erase C-Echo");
                       break;
      case ePlaying:   lcd_ks0066u_output_string(Base_LCD, 0, "Playing...");
                       lcd_ks0066u_output_string(Base_LCD, 1, "1-Stop");
                       break;
      case eRecording: lcd_ks0066u_output_string(Base_LCD, 0, "Recording...");
                       lcd_ks0066u_output_string(Base_LCD, 1, "1-Stop");
                       break;
      case eErasing:   lcd_ks0066u_output_string(Base_LCD, 0, "Erasing,2min...");
                       break;
      case eEchoing:   lcd_ks0066u_output_string(Base_LCD, 0, "Echoing...");
                       lcd_ks0066u_output_string(Base_LCD, 1, "1-Stop");
                       break;
      case eWriting:   lcd_ks0066u_output_string(Base_LCD, 0, "Writing to");
                       lcd_ks0066u_output_string(Base_LCD, 1, "flash device...");
                       break;
    }
}
//..............................................................................

//..............................................................................
void start_playing(void)
{
    max1104_open_cont_dac(Base_MAX1104);

    mode          = ePlaying;
    PlayLocation  = 0;
    set_audio_sample_rate(SOUND_HZ);
    ppc405cr_clear_programmable_interval_timer_flag();
    ppc405cr_enable_programmable_interval_timer();
    ppc405cr_enable_interrupts();
}
//..............................................................................

//..............................................................................
void stop_playing(void)
{
    mode = eMain;
    ppc405cr_disable_programmable_interval_timer();
    ppc405cr_disable_interrupts();
    max1104_close(Base_MAX1104);
}
//..............................................................................

//..............................................................................
void write_buffer(void)
{
    unsigned int       flash_addr          = 0;
    unsigned int       flash_sector_number = 0;
    unsigned short int flash_data          = 0;

    am29_erase_sector(Base_Daughter_Integrated_Flash, Base_Daughter_Integrated_Flash);
    while (flash_addr < BufferOffset)
    {
        if (flash_addr % 2 == 0)
        {
            flash_data = RECORD_BUFFER[flash_addr] << 8;
        }
        else
        {
            flash_data |= RECORD_BUFFER[flash_addr];
            am29_write_half_word(Base_Daughter_Integrated_Flash, Base_Daughter_Integrated_Flash + flash_addr - 1, flash_data);
        }
        flash_addr++;

        // If new sector, erase before writing
        if (flash_sector_number != am29_sector_number(Base_Daughter_Integrated_Flash + flash_addr) )
        {
            flash_sector_number = am29_sector_number(Base_Daughter_Integrated_Flash + flash_addr);
            am29_erase_sector(Base_Daughter_Integrated_Flash, Base_Daughter_Integrated_Flash + flash_addr);
        }
    }
    mode = eMain;
}
//..............................................................................

//..............................................................................
void start_recording(void)
{
    max1104_open_cont_adcdac(Base_MAX1104);
    Amplitude = spi_send_wait_receive(Base_MAX1104, 0);

    BufferOffset = 0;

    mode = eRecording;
    set_audio_sample_rate(SOUND_HZ);
    ppc405cr_clear_programmable_interval_timer_flag();
    ppc405cr_enable_programmable_interval_timer();
    ppc405cr_enable_interrupts();
}
//..............................................................................

//..............................................................................
void stop_recording(void)
{
    mode = eWriting;
    ppc405cr_disable_programmable_interval_timer();
    ppc405cr_disable_interrupts();
    max1104_close(Base_MAX1104);
    update_display();
    write_buffer();
    mode = eMain;
}
//..............................................................................

//..............................................................................
void start_echoing(void)
{
    max1104_open_cont_adcdac(Base_MAX1104);

    mode      = eEchoing;
    set_audio_sample_rate(SOUND_HZ);
    ppc405cr_clear_programmable_interval_timer_flag();
    ppc405cr_enable_programmable_interval_timer();
    ppc405cr_enable_interrupts();
}
//..............................................................................

//..............................................................................
void stop_echoing(void)
{
    mode = eMain;
    ppc405cr_disable_programmable_interval_timer();
    ppc405cr_disable_interrupts();
    max1104_close(Base_MAX1104);
}
//..............................................................................

//..............................................................................
void erase_flash(void)
{
    am29_erase_chip(Base_Daughter_Integrated_Flash);
    mode = eMain;
    update_display();
}
//..............................................................................

//..............................................................................
void update_mode(unsigned char key)
{
    switch (key)
    {
      case 0:
        switch (mode)
        {
          case eMain:       mode = ePlaying;
                            update_display();
                            start_playing();
                            break;
          case ePlaying:    mode = eMain;
                            update_display();
                            stop_playing();
                            break;
          case eRecording:  mode = eMain;
                            update_display();
                            stop_recording();
                            update_display();
                            break;
          case eEchoing:    mode = eMain;
                            update_display();
                            stop_echoing();
                            break;
        }
        break;
      case 1:
        switch (mode)
        {
          case eMain:       mode = eRecording;
                            update_display();
                            start_recording();
                            break;
        }
        break;
      case 2:
        switch (mode)
        {
          case eMain:       mode = eErasing;
                            update_display();
                            erase_flash();
                            break;
        }
        break;
      case 3:
        switch (mode)
        {
          case eMain:       mode = eEchoing;
                            update_display();
                            start_echoing();
                            break;
        }
        break;
    }
}
//..............................................................................

//..............................................................................
__interrupt(Interrupt_PIT) void SendNextData(void)
{
    set_audio_sample_rate(SOUND_HZ);
    ppc405cr_clear_programmable_interval_timer_flag();
    if (mode == ePlaying)
    {
        //Retrieve the last value from the Flash memory SPI controller.
        Amplitude = ((unsigned char*) Base_Daughter_Integrated_Flash)[PlayLocation++];
        PlayLocation = PlayLocation % BufferLength;
        //Send it to the codec for playing
        spi_send_receive_last(Base_MAX1104,Amplitude);
    }
    else if (mode == eRecording)
    {
        //Retrieve the amplitude from the codec
        Amplitude = spi_send_receive_last(Base_MAX1104,Amplitude);
        RECORD_BUFFER[BufferOffset] = Amplitude;
        BufferOffset++;
        if (BufferOffset > BufferLength) BufferOffset = 0; // wrap around
    }
    else if (mode == eEchoing)
    {
        //Send last byte to codec and get next
        Amplitude = spi_send_receive_last(Base_MAX1104,Amplitude);
    }
}
//..............................................................................

//..............................................................................
void main(void)
{
    unsigned char key;
    startup();
    initialize();
    update_display();
    while (1)
    {
        key = keypad_4x4_get_next_key(Base_Keypad);
        update_mode(key);
    }
}
//..............................................................................



