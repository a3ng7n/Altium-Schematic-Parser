//    _  _  ___  _____   ___    _    ___   ___  ___    _    ___  _  _
//   | \| || _ )|_   _| | _ )  /_\  | _ \ / __|| _ \  /_\  | _ \| || |
//   | .` || _ \  | |   | _ \ / _ \ |   /| (_ ||   / / _ \ |  _/| __ |
//   |_|\_||___/  |_|___|___//_/ \_\|_|_\ \___||_|_\/_/ \_\|_|  |_||_|
//                  |___|
// (c) 2004 Altium
// Started: 27.01.2004 Ch.Weimann
// LCD Bargraph driver Routines for NanoBoard Tester

#ifndef __NBT_BARGRAPH_H__
#define __NBT_BARGRAPH_H__

#define BG_RES_PERDIGIT 3      // set to either 3 for linear graph or 5 for high-res non-linear
                               // setting to 5 only works for right-going bargraphs at the moment

//--------------------------------------------------
// must be called before using bargraph routines to
// load Character generator RAM
//--------------------------------------------------
void GenerateBarGraphCustomCharacters(void);


//-------------------------------------------------------------------------------
// draws bargraph on LCD screen
// Parameters:
//   Value:     current bargraph value
//   MaxValue:  Value for 'MaxLength'
//   x,y  :     origin for graph (in LCD characters)
//   MaxLength: Maximum Length of Graph in increments per LCD pixel (default 3)
// direction  : 0 = going left from x/y
//              1 = going right from x/y
//-------------------------------------------------------------------------------
void LCD_BarGraph( unsigned int  Value,     unsigned int  MaxValue,
                   unsigned char x,         unsigned char y,
                   unsigned char MaxLength, unsigned char direction);

//-------------------------------------------------------------------------------
// draws bidirectional bargraph on LCD screen
// Parameters:
//   Value:     current bargraph value
//   MaxValue:  Value for 'MaxLength'
//   x,y  :     origin for graph (in LCD characters)
//   MaxLength: Maximum Length of Graph in increments per LCD pixel (default 3)
//-------------------------------------------------------------------------------
void LCD_BarGraph_Bi( signed int  Value,       unsigned int  MaxValue,
                      unsigned char x,         unsigned char y,
                      unsigned char MaxLength);

#endif
