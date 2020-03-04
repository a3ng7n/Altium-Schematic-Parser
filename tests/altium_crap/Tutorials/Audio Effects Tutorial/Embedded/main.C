#include <drv_cs4270.h>
#include <drv_i2s.h>
#include <drv_ioport.h>
#include "devices.h"

#include <string.h>
#include <stdint.h>
#include <stdio.h>

#define PORT_A         0
#define PORT_B         1
#define I2S_BUF_SIZE   512
#define AUDIO_BUF_SIZE 65536                    //this number MUST be a power of 2
#define I2S_SAMPLERATE 48000
#define MS_SAMPLES     (I2S_SAMPLERATE / 1000)  //millisecond samples

int32_t i2s_inbuf[I2S_BUF_SIZE] = {0};
int16_t in_temp_buf[I2S_BUF_SIZE / 2] = {0};
int16_t process_buf[AUDIO_BUF_SIZE] = {0};

cs4270_t * cs4270_drv;
i2s_t * i2s_drv;
ioport_t* ioport_drv;

void init_audio(void);
void get_audio(void);
void process_audio_echo(uint8_t delay);
void passthrough(void);
void put_audio(void);

void main(void)
{
    uint8_t effect_enable;
    uint8_t delay_coefficient = 0;

    //initialize the audio
    init_audio();

    ioport_drv = ioport_open(DRV_IOPORT_1);

    //output a list of instructions on how to use the digital IO example to control audio on the terminal
    printf("\n\nAudio Reverb Example:\n");
    printf("\n1. Set Bit 0 of BOUT[7..0] on the digital IO instrument for audio pass\n through.\n");
    printf("\n2. Set Bits 1 - 7 to initiate the audio Reverb Effect.\n");
    printf(" The Slider AOUT[7..0] will control the delay used by the reverb effect.\n");
    printf("3. Clear all bits on BOUT[7..0] to stop audio.\n");

    while (1)
    {
        effect_enable = ioport_get_value(ioport_drv, PORT_B); //read the value from the digital IO connected into GPIO port B

        ioport_set_value(ioport_drv, PORT_B, effect_enable);  //loop value of 'b' to the output of the GPIO port B (display to user)

        //create a coefficient to control the delay from value of digital IO slider at port A of GPIO (aka Port 0 or OUTA[7..0])
        delay_coefficient = ioport_get_value(ioport_drv, PORT_A);

        //loop value of delay_coefficient to A input of digital IO to display on the digital IO input channel A
        ioport_set_value(ioport_drv, PORT_A, delay_coefficient);

        //function to go and get the audio -- always gets audio when available and tries to fill input buffer
        get_audio();

        //test for the IO port A status to indicate what type of effect to create
        if (effect_enable == 1)
        {
            //simple fetch and put audio function
            passthrough();

            //function to put the audio in the output buffer
            put_audio();
        }
        else if (effect_enable > 1)
        {
            //function to process the audio and create the echo
            process_audio_echo(delay_coefficient);

            //function to put the audio in the output buffer
            put_audio();
        }
    }
}

/*
*get audio and place into audio buffer
*/
void get_audio(void)
{
    uint32_t rx_size;
    while (i2s_rx_avail(i2s_drv) < I2S_BUF_SIZE / 2)            // if the incoming buffer is < 256 samples (1/2 buffer size), get more samples
    {
        i2s_rx_start(i2s_drv);                                  // if no samples available, make sure the receiver is running
    }

    rx_size = i2s_rx_avail(i2s_drv) & ~1;                       // make even, the same number of samples for both channels
    rx_size = rx_size > I2S_BUF_SIZE ? I2S_BUF_SIZE : rx_size;

    i2s_read16(i2s_drv, in_temp_buf, rx_size);                  // read samples into the incoming buffer
}

/*
*accept incoming audio and create a reverb effect
*/
void process_audio_echo(uint8_t delay)
{
    static    int16_t * prcs_insert_ptr = process_buf;

    //creating 2 pointers, slightly offset from one another to read data at different times in the history of the data acquisition process.
    //the delta between the two corresponds to the length of delay
    int16_t * prcs_echo_ptr = prcs_insert_ptr - ((MS_SAMPLES * ((delay) * 5)) + 1);
    int16_t * curr_ptr = in_temp_buf;

    if (prcs_echo_ptr <= process_buf)
        prcs_echo_ptr += AUDIO_BUF_SIZE;

    for (int i = 0; i < I2S_BUF_SIZE / 2; i++)
    {
        * prcs_insert_ptr = (* prcs_echo_ptr >> 1) + * curr_ptr;
        prcs_insert_ptr++;

        if (prcs_insert_ptr == & process_buf[AUDIO_BUF_SIZE])
            prcs_insert_ptr = process_buf;

        curr_ptr++;
        prcs_echo_ptr++;

        if (prcs_echo_ptr == & process_buf[AUDIO_BUF_SIZE])
            prcs_echo_ptr = process_buf;
    }
}

/*
*passthrough audio triggered from the digital IO
*/
void passthrough(void)
{
    static    int16_t * prcs_insert_ptr = process_buf;

    int16_t * curr_ptr = in_temp_buf;

    for (int i = 0; i < I2S_BUF_SIZE / 2; i++)
    {
        * prcs_insert_ptr = * curr_ptr;

        prcs_insert_ptr++;

        if (prcs_insert_ptr == & process_buf[AUDIO_BUF_SIZE])
            prcs_insert_ptr = process_buf;

        curr_ptr++;
    }
}

/*
* write audio to the I2S output buffer
*/
void put_audio(void)
{
    static int16_t * prcs_extract_ptr = process_buf;

    while (i2s_tx_avail(i2s_drv) < I2S_BUF_SIZE / 2) // wait till there is space for the received samples to store in the transmit buffer
    {
        i2s_tx_start(i2s_drv);                       // if no space available, make sure the transmitter is running
    }

    i2s_write16(i2s_drv, prcs_extract_ptr, I2S_BUF_SIZE / 2);
    prcs_extract_ptr += I2S_BUF_SIZE / 2;

    while (prcs_extract_ptr >= & process_buf[AUDIO_BUF_SIZE])
        prcs_extract_ptr -= AUDIO_BUF_SIZE;
}

/*
* initialize the audio peripherals
*/
void init_audio(void)
{
    while (cs4270_drv == NULL)
    {
        cs4270_drv = cs4270_open(DRV_CS4270_1);
    }
    i2s_drv = i2s_open(DRV_I2S_1);

    i2s_rx_start(i2s_drv);
    i2s_tx_start(i2s_drv);
}
 
