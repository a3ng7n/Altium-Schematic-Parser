/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   ICS307 Clock generator device driver
|*
\*****************************************************************************/

#ifndef __CLOCK_ICS307_H__
#define __CLOCK_ICS307_H__

//..............................................................................
// Bit Masks for CLK1 output divider Setting  S[0..2]
#define ICS307_OD_10   0x00   // BitMask for CLK1 Output Divide by 10
#define ICS307_OD_2    0x01   // BitMask for CLK1 Output Divide by 2
#define ICS307_OD_8    0x02   // BitMask for CLK1 Output Divide by 8
#define ICS307_OD_4    0x03   // BitMask for CLK1 Output Divide by 4 (default)
#define ICS307_OD_5    0x04   // BitMask for CLK1 Output Divide by 5
#define ICS307_OD_7    0x05   // BitMask for CLK1 Output Divide by 7
#define ICS307_OD_3    0x06   // BitMask for CLK1 Output Divide by 3
#define ICS307_OD_6    0x07   // BitMask for CLK1 Output Divide by 6
//..............................................................................

//..............................................................................
// Bit Masks for Clock 2 Output MUX bits (F[0..1])
#define ICS307_CLK2_REF        0x00  // Clk2 output = REF    (default)
#define ICS307_CLK2_REF_DIV2   0x08  // Clk2 output = REF/2
#define ICS307_CLK2_OFF        0x10  // Clk2 output = 0
#define ICS307_CLK2_CLK1_DIV2  0x18  // Clk2 output = CLK1/2
//..............................................................................

//..............................................................................
// Bit Masks for TTL Bit (Output Duty Cycle Configuration)
#define ICS307_TTL_5V          0x00  // optimised Dyty Cycle for VDD = 5V
#define ICS307_TTL_3V3         0x20  // optimised Dyty Cycle for VDD = 3.3V (default)
//..............................................................................

//..............................................................................
// Bit Mask for internal Crystal Load Capacitance
#define ICS307_XTAL_LOAD_0     0x00  // Lowest Range (default)
#define ICS307_XTAL_LOAD_1     0x40  // ...
#define ICS307_XTAL_LOAD_2     0x80  // ...
#define ICS307_XTAL_LOAD_3     0xC0  // Highest Range
//..............................................................................

//..............................................................................
#define ICS307_Default_Config  (ICS307_XTAL_LOAD_0 | ICS307_TTL_3V3 | ICS307_CLK2_REF | ICS307_OD_4)
#define ICS307_Default_VCO_DIV 8    // default VCO divider
#define ICS307_Default_REF_DIV 6    // default Reference divider
//..............................................................................

//..............................................................................
#define ICS307_30MHZ 0x220501L   // command word for 30MHz
#define ICS307_25MHZ 0x220381L   // command word for 25MHz
#define ICS307_20MHZ 0x230406L   // reset command word 20MHz
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
//
void ics307_program ( unsigned int base, unsigned char config, unsigned int VDW, unsigned char RDW );

//..............................................................................
// Programs command word 'data' into the clock generator chip
void ics307_program_word ( unsigned int base, unsigned int data );

//..............................................................................
// Program the frequency from a simple table of predefined control words
// For other values or for other base frequencies go to:
// http://www.icst.com/Products/ics307inputForm.html
void ics307_program_frequency ( unsigned int base, unsigned int index );

#define ICS307_6_MHz     0 //   6 MHz from 20_MHz
#define ICS307_10_MHz    1 //  10 MHz from 20_MHz
#define ICS307_20_MHz    2 //  20 MHz from 20_MHz
#define ICS307_25_MHz    3 //  25 MHz from 20_MHz
#define ICS307_30_MHz    4 //  30 MHz from 20_MHz
#define ICS307_40_MHz    5 //  40 MHz from 20_MHz
#define ICS307_50_MHz    6 //  50 MHz from 20_MHz
#define ICS307_60_MHz    7 //  60 MHz from 20_MHz
#define ICS307_70_MHz    8 //  70 MHz from 20_MHz
#define ICS307_75_MHz    9 //  75 MHz from 20_MHz
#define ICS307_80_MHz   10 //  80 MHz from 20_MHz
#define ICS307_90_MHz   11 //  90 MHz from 20_MHz
#define ICS307_100_MHz  12 // 100 MHz from 20_MHz
#define ICS307_125_MHz  13 // 125 MHz from 20_MHz
#define ICS307_133_MHz  14 // 133 MHz from 20_MHz
#define ICS307_150_MHz  15 // 150 MHz from 20_MHz
#define ICS307_175_MHz  16 // 175 MHz from 20_MHz
#define ICS307_200_MHz  17 // 200 MHz from 20_MHz

//..............................................................................

#endif // __ICS307_H__

