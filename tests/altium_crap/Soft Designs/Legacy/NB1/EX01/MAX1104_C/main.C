#include <stdio.h>
#include <stdlib.h>

#include "ntype.h"
#include "interrupt0.h"

// #include "sinewave.c"
BYTE __rom sin[256] =
{
    128, 131, 134, 137, 140, 143, 146, 149, 152, 156, 159, 162, 165, 168, 171, 
        174, 176, 179, 182, 185, 188, 191, 193, 196, 199, 201, 204, 206, 209, 
        211, 213, 216, 218, 220, 222, 224, 226, 228, 230, 232, 234, 236, 237, 
        239, 240, 242, 243, 245, 246, 247, 248, 249, 250, 251, 252, 252, 253, 
        254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 254, 
        254, 253, 252, 252, 251, 250, 249, 248, 247, 246, 245, 243, 242, 240, 
        239, 237, 236, 234, 232, 230, 228, 226, 224, 222, 220, 218, 216, 213, 
        211, 209, 206, 204, 201, 199, 196, 193, 191, 188, 185, 182, 179, 176, 
        174, 171, 168, 165, 162, 159, 156, 152, 149, 146, 143, 140, 137, 134, 
        131, 127, 124, 121, 118, 115, 112, 109, 106, 103, 99, 96, 93, 90, 87, 84
        , 81, 79, 76, 73, 70, 67, 64, 62, 59, 56, 54, 51, 49, 46, 44, 42, 39, 37
        , 35, 33, 31, 29, 27, 25, 23, 21, 19, 18, 16, 15, 13, 12, 10, 9, 8, 7, 6
        , 5, 4, 3, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3, 3, 4
        , 5, 6, 7, 8, 9, 10, 12, 13, 15, 16, 18, 19, 21, 23, 25, 27, 29, 31, 33, 
        35, 37, 39, 42, 44, 46, 49, 51, 54, 56, 59, 62, 64, 67, 70, 73, 76, 79, 
        81, 84, 87, 90, 93, 96, 99, 103, 106, 109, 112, 115, 118, 121, 124
}

;

// frequency calcs
#define SAMPLE_RATE 15000 // in hz.   
#define CLOCK_FREQ 40000000 // in hz.           

#define RATE_DIVISOR(S) S // todo interrupt rate generator   

WORD SetAudioFrequency(WORD hzFreq)
{
    // calculate a 16 bit addend to increment a wave table pointer fractionally
    // in order to generate a given frequency.
    // assumptions : table size = 256
    // return (hzFreq * TableSize * 256)/SampleRate

    return(WORD)(((long) hzFreq << 16) / SAMPLE_RATE);
//    return(WORD)(((long) 1000 << 16) / SAMPLE_RATE);
}
//#define RAM_INTERFACE

#ifdef RAM_INTERFACE
    #define SPI_DATA  (*(byte __xdata *)(0x8000))
    #define SPI_CTRL  (*(byte __xdata *)(0x8001))
#else
    #define SPI_DATA      P3
    #define SPI_CTRL      P0
    #define SPI_CS        P0_0
    #define SPI_A0        P0_1
    #define SPI_RD        P0_2
    #define SPI_WR        P0_3
#endif
#define SPI_Control   1
#define SPI_Data      0
#define SPI_ENABLE    0x02
#define MODE P0_0
// SPI Control Register Structure
// Bit 0        - Busy Flag (Read Only) 1 = Busy
// Bit 1        - SPI CS                1 = Enable SPI device (applies 0 to SPI hardware SPI_nCS)
// Bit [7..0]   - SPI Clock Divisor     0 .. 63

// these should be bits
BYTE __data SPI_Busy = 0;
BYTE __data AudioActive = 0;
BYTE __data AudioReceive = 0;
BYTE __data TestBit;
WORD wave_increment;
WORD wave_index;

/////////////////////////////

void spiInit()
{
    SPI_Busy = 0;
    AudioActive = 0;
    AudioReceive = 0;
    #ifndef RAM_INTERFACE
    // Bit banging version through ports
    SPI_DATA = 0;
    SPI_CTRL = 0;
    #endif
}

void spiWrite(BYTE Addr, BYTE data)
{
    SPI_Busy = 1;
    #ifdef RAM_INTERFACE
    if (Addr)
         SPI_CTRL = data;
    else
         SPI_DATA = data;
    #else
    // Bit banging version through ports
    SPI_DATA = data;
    SPI_RD = 0;
    SPI_A0 = (Addr != 0);
    SPI_CS = 1;
    SPI_WR = 1;
    SPI_WR = 0;
    SPI_CS = 0;
    #endif
    // Bit banging version through ports
}

BYTE spiRead(BYTE Addr)
{
    BYTE result;
    #ifdef RAM_INTERFACE
    if (Addr)
       result = SPI_CTRL;
    else
       result = SPI_DATA;
    #else
    // Bit banging version through ports
    SPI_WR = 0;
    SPI_A0 = (Addr != 0);
    SPI_CS = 1;
    SPI_RD = 1;
    result = SPI_DATA;
    SPI_RD = 0;
    SPI_CS = 0;
    #endif
    return result;
}


BYTE __data PrevByte;
BYTE __idata outBuf[4];
BYTE __data outBufHead = 0;
BYTE __data outBufTail = 0;
BYTE __idata inBuf[4];
BYTE __data inBufHead = 0;
BYTE __data inBufTail = 0;
BYTE __data Sample=0;
void InitQueues()
{
    PrevByte = 127;
    outBufHead = outBufTail = inBufHead = inBufTail = 0;
}

/*****************************************************************************
Function:           Timer0Interrupt

Description:
This function will be called at the audio sample rate.

********************************************************************************/
void Timer0Interrupt(void)
{
    DISABLE_INTERRUPTS(); // Global interrupt disable   
    TestBit++;
    P0_4 =  1;


    if (!SPI_Busy && AudioActive)
    {
       // Output either a sine wave or the output of the DAC
       if (MODE)
          SPI_DATA = PrevByte;          // output of ADC
       else
       {
          SPI_DATA = sin[Sample];       
          Sample+=12;
       }
       SPI_A0 = 0;
       SPI_CS = 1;
       SPI_WR = 1;
       SPI_WR = 0;
       SPI_CS = 0;
       SPI_Busy = 1;
    }

    P0_4 = 0;

    ENABLE_INTERRUPTS(); // Global interrupt enable   
}

/*****************************************************************************
Function:           Interrupt0

Description:
This function will be called every interrupt.

********************************************************************************/

void Interrupt0(void)
{
//    BYTE __data temp;

    DISABLE_INTERRUPTS(); // Global interrupt disable   
    P0_4 =  1;
    SPI_Busy = 0;
    if (AudioReceive) // if we are receiving audio data
    {
        SPI_A0 = 0;
        SPI_CS = 1;
        SPI_RD = 1;
        PrevByte = SPI_DATA;
        SPI_RD = 0;
        SPI_CS = 0;
    } 
    P0_4 =  0;

    ENABLE_INTERRUPTS(); // Global interrupt enable   

}

void Interrupt1(void)
{
    DISABLE_INTERRUPTS(); // Global interrupt disable
    ENABLE_INTERRUPTS(); // Global interrupt enable   
    
}

void Initialise(void)
{
    ET1 = 0;
    EX1 = 0;
    ES = 0;
    InitInterrupt0(); //Initialize interrupt 0   
    InitTimer0(22050);
//      InitInterrupt1();
}

// MAX1104 Codec Control Byte
#define START 0x80       // start of control byte
#define A1    0x40       // DAC Addressed - The control byte configures the DAC
#define A0    0x20       // ADC Addressed - The control byte configures the ADC
#define C1    0x10       // ADC input to VDD/2 to measure supply voltage (MAX1102-MAX1103 only)
#define C0    0x08       // Continuous Conversion
#define E2    0x04       // Enable Reference Voltage - don't care for MAX1104
#define E1    0x02       // Enable ADC
#define E0    0x01       // Enable DAC


void StartCodec()
{
    DISABLE_INTERRUPTS();   // Global interrupt disable
    spiWrite(SPI_Control,SPI_ENABLE+0x00);   // enable signal and SPI Timing
    ENABLE_INTERRUPTS();    // Global interrupt enable
    while (SPI_Busy);       // Wait
    DISABLE_INTERRUPTS();   // Global interrupt disable   
    spiWrite(SPI_Data, START|A0|A1|C0|E1|E0); // send control byte
    ENABLE_INTERRUPTS();            // Global interrupt enable
    while (SPI_Busy);       // Wait
    DISABLE_INTERRUPTS();   // Global interrupt disable   
    AudioActive = 1;        // Enable Audio Ouput
    AudioReceive = 1;       // Start Receiving Data
    ENABLE_INTERRUPTS();    // Global interrupt disable   
}

void StopCodec()
{
    DISABLE_INTERRUPTS();   // Global interrupt disable
    AudioActive = 0;        // Disable Audio Ouput
    AudioReceive = 0;       // Stop Receiving Data
    spiWrite(SPI_Control,0x00);     // disable signal and SPI Timing
    ENABLE_INTERRUPTS();            // Global interrupt enable
    while (SPI_Busy);       // Wait
}
                         
void main(void)
{
    //   BYTE result;
    BYTE count = 0;
    BYTE SPI_Rate = 0;
    byte temp;
 //   int Freq = 3000;
    
 //   wave_increment = SetAudioFrequency(Freq);

    Initialise();
 //   InitQueues();
    
    spiInit();
    
    StartCodec();
        
    wave_index = 0;
    count = 255;
    while (1)
    {
       count++;
    }
    StopCodec();

}
