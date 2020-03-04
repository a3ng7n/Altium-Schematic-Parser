/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   MAX1104 Codec device driver
|*
\*****************************************************************************/

#include "codec_max1104.h"
#include "wb_spi.h"
#include "util_timing.h"

#define MAX1104_Continuous_ADCDAC MAX1104_START | MAX1104_A1 | MAX1104_A0 | MAX1104_C0 | MAX1104_E1 | MAX1104_E0
#define MAX1104_Continuous_ADC    MAX1104_START |              MAX1104_A0 | MAX1104_C0 | MAX1104_E1 | MAX1104_E0
#define MAX1104_Continuous_DAC    MAX1104_START | MAX1104_A1 |              MAX1104_C0 | MAX1104_E1 | MAX1104_E0
#define MAX1104_Single_ADCDAC     MAX1104_START | MAX1104_A1 | MAX1104_A0 |              MAX1104_E1 | MAX1104_E0
#define MAX1104_Single_ADC        MAX1104_START |              MAX1104_A0 |              MAX1104_E1 | MAX1104_E0
#define MAX1104_Single_DAC        MAX1104_START | MAX1104_A1 |                           MAX1104_E1 | MAX1104_E0

//..............................................................................
void max1104_open_cont_adcdac  ( unsigned int base )
{
  spi_open      (base);
  timing_delay_ms(1);
  spi_data_write(base,MAX1104_Continuous_ADCDAC);
  timing_delay_ms(1);
}
//..............................................................................

//..............................................................................
void max1104_open_cont_adc     ( unsigned int base )
{
  spi_open      (base);
  timing_delay_ms(1);
  spi_data_write(base,MAX1104_Continuous_ADC);
  timing_delay_ms(1);
}
//..............................................................................

//..............................................................................
void max1104_open_cont_dac     ( unsigned int base )
{
  spi_open      (base);
  timing_delay_ms(1);
  spi_data_write(base,MAX1104_Continuous_DAC);
  timing_delay_ms(1);
}
//..............................................................................

//..............................................................................
void max1104_open_single_adcdac  ( unsigned int base )
{
  spi_open      (base);
  spi_data_write(base,MAX1104_Single_ADCDAC);
  timing_delay_ms(1);
}
//..............................................................................

//..............................................................................
void max1104_open_single_adc     ( unsigned int base )
{
  spi_open      (base);
  spi_data_write(base,MAX1104_Single_ADC);
  timing_delay_ms(1);
}
//..............................................................................

//..............................................................................
void max1104_open_single_dac     ( unsigned int base )
{
  spi_open      (base);
  spi_data_write(base,MAX1104_Single_DAC);
  timing_delay_ms(1);
}
//..............................................................................

//..............................................................................
void max1104_close(unsigned int base)
{
  spi_control_write(base,SPI_CTRL_ENABLE);
}
//..............................................................................

//..............................................................................
void          max1104_set_enabled     ( unsigned int base, unsigned int enable_adc, unsigned int enable_dac )
{
    unsigned char control;
    spi_open(base);
    control = MAX1104_START;
    if (enable_adc) control |= MAX1104_E1;
    if (enable_dac) control |= MAX1104_E0;
    spi_data_write(base, control);
    spi_control_write(base, SPI_CTRL_ENABLE);
}
//..............................................................................

