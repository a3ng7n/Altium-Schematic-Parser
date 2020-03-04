//      __  __ ____      _     __  __
//      \ \/ /|  _ \    / \   |  \/  |
//       \  / | |_) |  / _ \  | |\/| |
//       /  \ |  _ <  / ___ \ | |  | |
//      /_/\_\|_| \_\/_/   \_\|_|  |_|
//
//
// (c) 2003 Altium
// Started: 17.11.2003 Ch.W.
// Xdata Ram Test Functions

#include "xram.h"
#include "hware.h"


// --------------------------------------------------------
// Sets all control lines for onboard ram to a safe state
// --------------------------------------------------------
void ObRamInit(void)
{
   XR_SelectBank(0);
}

void ObRamWrite(unsigned long address, unsigned char data)
{
  XR_SelectBank((unsigned) (address >> 16));
  *((__xdata  volatile unsigned char *) (address & 0xFFFF) ) = data;
}

unsigned char ObRamRead(unsigned long address)
{
  XR_SelectBank((unsigned) (address >> 16));
  return *((__xdata  volatile unsigned char *) (address & 0xFFFF) );
}




__idata static unsigned char XR_CurrentBank = 0;

// selects Xdata bank according to parameter 'bank'
// selects one 64k block of SRAM
void XR_SelectBank(unsigned char bank)
{
  OB_RAM_BANK    = bank;
  XR_CurrentBank = bank;
  return;
}

// http://www.netrino.com/Articles/MemoryTesting/paper.html

/**********************************************************************
 *
 * Function:    memTestDataBus()
 *
 * Description: Test the data bus wiring in a memory region by
 *              performing a walking 1's test at a fixed address
 *              within that region.  The address (and hence the
 *              memory region) is selected by the caller.
 *
 * Notes:
 *
 * Returns:     0 if the test succeeds.
 *              A non-zero result is the first pattern that failed.
 *
 **********************************************************************/
unsigned long  memTestDataBus(unsigned long address)
{
    unsigned char pattern;
     /*
     * Perform a walking 1's test at the given address.
     */
    for (pattern = 1; pattern !=0; pattern <<= 1)
    {
        /*
         * Write the test pattern.
         */
        ObRamWrite(address, pattern);

        /*
         * Read it back (immediately is okay for this test).
         */
        if(ObRamRead(address) != pattern)
        {
            return (pattern);
        }
    }
    return (0);
}   /* memTestDataBus() */




 /**********************************************************************
 *
 * Function:    memTestAddressBus()
 *
 * Description: Test the address bus wiring in a memory region by
 *              performing a walking 1's test on the relevant bits
 *              of the address and checking for aliasing. This test
 *              will find single-bit address failures such as stuck
 *              -high, stuck-low, and shorted pins.
 *
 * Returns:     0 if the test succeeds.
 *              A non-zero result indicates a failure
 ***********************************************************************/
unsigned int memTestAddressBus(unsigned long nBytes)
{
    unsigned long addressMask = (nBytes/sizeof(unsigned char) - 1);
    unsigned long offset;
    unsigned long testOffset;

    unsigned char pattern     = 0xAA;
    unsigned char antipattern = 0x55;


    /*
     * Write the default pattern at each of the power-of-two offsets.
     */
    for (offset = 1; (offset & addressMask) != 0; offset <<= 1)
    {
      ObRamWrite(offset, pattern);
//        baseAddress[offset] = pattern;
    }

    /*
     * Check for address bits stuck high.
     */
    testOffset = 0;
    ObRamWrite(testOffset, antipattern);
//    baseAddress[testOffset] = antipattern;

    for (offset = 1; (offset & addressMask) != 0; offset <<= 1)
    {
        if(ObRamRead(offset) != pattern)
//        if (baseAddress[offset] != pattern)
        {
            return (1);
        }
    }

    ObRamWrite(testOffset, pattern);
//   baseAddress[testOffset] = pattern;

    /*
     * Check for address bits stuck low or shorted.
     */
    for (testOffset = 1; (testOffset & addressMask) != 0; testOffset <<= 1)
    {
       ObRamWrite(testOffset, antipattern);
//        baseAddress[testOffset] = antipattern;

       if(ObRamRead(0L) != pattern)
//       if (baseAddress[0] != pattern)
       {
          return (2);
       }

        for (offset = 1; (offset & addressMask) != 0; offset <<= 1)
        {
            if ((ObRamRead(offset) != pattern) && (offset != testOffset))
//            if ((baseAddress[offset] != pattern) && (offset != testOffset))
            {
                return (3);
            }
        }
          ObRamWrite(testOffset, pattern);
//        baseAddress[testOffset] = pattern;
    }

    return (0);
}   /* memTestAddressBus() */



//---------------------------------------------------------
// Tests one 64k memory bank at a time
// performs two passes:
// Pass 1 writes a test pattern to each byte of the bank
// and verifies it.
// Pass 2 writes an inverted test pattern to each byte
// and verifies it.
// this way we can detect 'stuck' bits as well as 'missing' chips
// the test pattern skewed after 13 bytes every 256 to prevent 'aliasing'
// returns 0: success
//         1: verify error for test pattern
//         2: verify error for inverted test pattern
//---------------------------------------------------------
unsigned char memTestBank(unsigned char bank)
{
    unsigned int offset;
    unsigned char pattern;
    unsigned char antipattern;
    static unsigned char i;
    XR_SelectBank(bank);
// write defined pattern
    /*
     * Fill memory with a known pattern.
     */
    for (pattern = 1, offset = 0; offset != 0xFFFF; pattern++, offset++)
    {
        if(pattern==12) pattern++;   // skip pattern occasionally to make less uniform
        *((__xdata  volatile unsigned char *) (offset)) = pattern;
    }

    /*
     * Check each location and invert it for the second pass.
     */
    for (pattern = 1, offset = 0; offset != 0xFFFF; pattern++, offset++)
    {
        if(pattern==12) pattern++;   // skip pattern occasionally to make less uniform
        i = *((__xdata  volatile unsigned char *) (offset));
        if(i!= pattern)
        {
          return (1);
        }
        antipattern = ~pattern;
        *((__xdata  volatile unsigned char *) (offset)) = antipattern;
//        baseAddress[offset] = antipattern;
    }

    /*
     * Check each location for the inverted pattern and zero it.
     */

    for (pattern = 1, offset = 0; offset != 0xFFFF; pattern++, offset++)
    {
        if(pattern==12) pattern++;   // skip pattern occasionally to make less uniform
        antipattern = ~pattern;
        if (*((__xdata  volatile unsigned char *) (offset)) != antipattern)
        {
            return (2);
        }
    }

    return (0);
}   /* memTestDevice() */

// ----------------------------------------------------------------------------------------
//  writes the bank number to each bank at address 'address' and reads it back to verify
//  if a bank select line is not present or stuck, it will read back the wrong data
//  NoOfBanks is the Number of memory banks to be tested
//  returns 0 if success
//  bank with 1st error +1 if failed;
// ----------------------------------------------------------------------------------------
unsigned char memTestBankSwitching(unsigned int address, unsigned char NoOfBanks)
{
  unsigned char bank;
  for(bank=0; bank<NoOfBanks; bank++)  // write different pattern to same address in each bank
  {
    XR_SelectBank(bank);
    *((__xdata  volatile unsigned char *) (address)) = bank;
  }
  for(bank=0; bank<NoOfBanks; bank++)  //  verify pattern at address in each bank
  {
    unsigned char volatile c;
    XR_SelectBank(bank);
    c=*((__xdata  volatile unsigned char *) (address));
    if (bank != c)
      return bank +1;
  }
  return 0;
}





