#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include "process_audio.h"

#define FREQUENCY_MHZ 50000000
#define CYCLE_PERIOD_NS (1000000000 / FREQUENCY_MHZ)
#define MICROSECOND_CYCLES (1000 / CYCLE_PERIOD_NS)
#define MILLISECOND_CYCLES (1000000 / CYCLE_PERIOD_NS)

#define SPI_CTRL_ENABLE  0x00
#define SPI_CTRL_DISABLE 0x01
#define SPI_CTRL_CS      0x02
#define SPI_CTRL_MODE    0x04
#define SPI_CTRL_PHASE   0x10

__SPI volatile uint8_t  spi_data    __at(0);
__SPI volatile uint8_t  spi_control __at(1);
__SPI volatile uint8_t  spi_divider __at(2);

//void IO(void);

void spi_wait(void)
{
    while (spi_control & 0x80)
    {
    }
}

void spi_control_low(unsigned char bits)
{
    spi_control &= ~bits;
}

void spi_control_high(unsigned char bits)
{
    spi_control |= bits;
}

void spi_sendandwait(int8_t data)
{
    spi_data = data;
    spi_wait();
}

int8_t spi_sendwaitreceive(int8_t data)
{
    spi_data = data;
    spi_wait();
    return spi_data;
}

void nanoboard_spi_open(unsigned char device )
{
    spi_divider = 0x19;                 // 50 MHz / 31 = 1.61 MHz ?
    spi_control = 0;                    // toggle mode pin low/high to reset address bit counter
    spi_control = 4;                    // access SPI address register in Nanoboard Controller

    while (spi_sendwaitreceive( 0x80 | device ) != 0)
    {
    }
    spi_control = 0;
}

void nanoboard_spi_close( void )
{
    spi_control_high( SPI_CTRL_MODE | SPI_CTRL_CS );
    spi_sendwaitreceive(0);             // send 0, receive 1
}

int8_t cs4270_read(int8_t address)
{
    int8_t result;

    spi_control_low(SPI_CTRL_CS);
    spi_sendandwait(0x9E);              // address | write
    spi_sendandwait(address);           // write address
    spi_control_high(SPI_CTRL_CS);

    spi_control_low(SPI_CTRL_CS);
    spi_sendandwait(0x9F);              // address | read
    result = spi_sendwaitreceive(0xFE); // get result
    spi_control_high(SPI_CTRL_CS);

    __wait(MICROSECOND_CYCLES);

    return result;
}

void cs4270_write(int8_t address, int8_t data)
{
    spi_control_low(SPI_CTRL_CS);
    spi_sendandwait(0x9E);              // write 0x9E
    spi_sendandwait(address);           // write address
    spi_sendandwait(data);              // write data
    spi_control_high(SPI_CTRL_CS);
    __wait(MICROSECOND_CYCLES);
}

void cs4270_write_power(int8_t data)
{
    cs4270_write( 2, data );
    __wait(MILLISECOND_CYCLES);
}

void cs4270_init(void)
{
    cs4270_write_power(1);              // power down
    cs4270_write_power(0x80 );          // power up, freeze
    cs4270_write(3, 0x30);              // mode control: slave mode, divide by 1
    cs4270_write(4, 0x09);              // adc/dac: DAC: digital, ADC: i2s, 24 bit
    cs4270_write(5, 0x60);              // transition control: both soft ramp and zero cross
    cs4270_write(6, 0x00);              // mute control: auto-mute
    cs4270_write(7, 0);                 // DAC channel A volume: gain = 0 dB.
    cs4270_write(8, 0);                 // DAC channel B volume: gain = 0 dB.
    cs4270_write_power(0);              // power: unfreeze
}

void main_loop(void)
{
    nanoboard_spi_open( 0xb );
    cs4270_init();
    nanoboard_spi_close();

    IO();
}


