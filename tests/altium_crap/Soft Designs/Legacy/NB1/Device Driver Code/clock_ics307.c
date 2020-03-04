/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   ICS307 Clock generator device driver
|*
\*****************************************************************************/

//..............................................................................
#include "wb_spi.h"
#include "clock_ics307.h"
//..............................................................................

//..............................................................................
// Programs the ICS307 to generate a new output frequency
// Output Frequency for CLK1 = Fref * 2 * (VDW + 8) / ((RDW + 2) * OD)
// Parameters:
//   Config: Configuration word :
//
//         MSB                                 LSB
//        +----+----+-----+----+----+----+----+----+
//        | C1   C0 | TTL | F1   F0 | S2   S1   S0 |
//        +----+----+--+--+--+-+-+--+-+--+----+--+-+
//           |   |     |     |   |    |          |
//           +-+-+     |     +-+-+    +-----+----+
//             |       |       |            |
//        Crystal    Output  CLK2      Output Divide
//        Load       Duty    Output
//        Impedance  Cycle   Select
//
//  VDW    : VCO Divider Word  (9 significant bits)
//  RDW    : Reference Divider Word  (7 significant bits)
//..............................................................................
void ics307_program(unsigned int  base,
                    unsigned char config,
                    unsigned int  VDW,
                    unsigned char RDW)
{
  register unsigned char temp;
  VDW  &= 0x1FF;            // only 9 bits count
  RDW  &= 0x7F;             // only 7 bits count
  temp  = ((unsigned char) VDW) << 7;   // bit-mangle 3rd byte
  temp |= RDW;

  spi_cs_lo           (base  );
  spi_wait_and_send   (base,0);
  spi_wait_and_send   (base, config      );
  spi_wait_and_send   (base, VDW >> 1    );
  spi_wait_and_send   (base, temp        );
  spi_wait_while_busy (base);
  spi_cs_hi           (base  );
}

//..............................................................................
// Programs command word 'data' into the clock generator chip
//..............................................................................
void ics307_program_word(unsigned int base,
                         unsigned int data)
{
  spi_cs_lo           (base  );
  spi_wait_and_send   (base,0);
  spi_wait_and_send   (base, (data >>16) & 0xFF);
  spi_wait_and_send   (base, (data >> 8) & 0xFF);
  spi_wait_and_send   (base, (data     ) & 0xFF);
  spi_wait_while_busy (base);
  spi_cs_hi           (base  );
}
//..............................................................................

//..............................................................................
// Programs command word based on predefined frequency table
//..............................................................................
void ics307_program_frequency(unsigned int base,
                              unsigned int index)
{
    unsigned int ProgramWord = 0;
    switch (index)
    {
        case 0  : ProgramWord = 0x200206; break; // 6  Mhz from 20 Mhz
        case 1  : ProgramWord = 0x220204; break; //10  Mhz from 20 Mhz
        case 2  : ProgramWord = 0x220201; break; //20  Mhz from 20 Mhz
        case 3  : ProgramWord = 0x220381; break; //25  Mhz from 20 Mhz
        case 4  : ProgramWord = 0x220501; break; //30  Mhz from 20 Mhz
        case 5  : ProgramWord = 0x230201; break; //40  Mhz from 20 Mhz
        case 6  : ProgramWord = 0x230381; break; //50  Mhz from 20 Mhz
        case 7  : ProgramWord = 0x230501; break; //60  Mhz from 20 Mhz
        case 8  : ProgramWord = 0x230681; break; //70  Mhz from 20 Mhz
        case 9  : ProgramWord = 0x210382; break; //75  Mhz from 20 Mhz
        case 10 : ProgramWord = 0x210201; break; //80  Mhz from 20 Mhz
        case 11 : ProgramWord = 0x230981; break; //90  Mhz from 20 Mhz
        case 12 : ProgramWord = 0x230B01; break; //100 Mhz from 20 Mhz
        case 13 : ProgramWord = 0x210882; break; //125 Mhz from 20 Mhz
        case 14 : ProgramWord = 0x213E92; break; //133 Mhz from 20 Mhz
        case 15 : ProgramWord = 0x210B02; break; //150 Mhz from 20 Mhz
        case 16 : ProgramWord = 0x210D82; break; //175 Mhz from 20 Mhz
        case 17 : ProgramWord = 0x210B01; break; //200 Mhz from 20 Mhz
        case 18 : ProgramWord = 0x222D88; break; //47.5 Mhz from 20 Mhz
    }

    if (ProgramWord)
    {
       ics307_program_word(base, ProgramWord);
    }
}
//..............................................................................







