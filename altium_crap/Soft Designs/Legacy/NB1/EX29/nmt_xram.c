//      _   _  __  __  _____    __  __ ____      _     __  __
//     | \ | ||  \/  ||_   _|   \ \/ /|  _ \    / \   |  \/  |
//     |  \| || |\/| |  | |      \  / | |_) |  / _ \  | |\/| |
//     | |\  || |  | |  | |      /  \ |  _ <  / ___ \ | |  | |
//     |_| \_||_|  |_|  |_|_____/_/\_\|_| \_\/_/   \_\|_|  |_|
//                        |_____|
//
// (c) 2003 Altium
// Started: 17.11.2003 Ch.Weimann
// Xdata Ram Test Functions for
// the Altium Nanoboard Tester
// 19.04.04 Ch.W. Added skipping of test vectors; added test for bank switching hardware
// 16.08.04 Ch.W. added typecasts to suppress implicit int --> unsigned char warnings

#include "nmt_xram.h"
#include "hware.h"

__idata static unsigned char XR_CurrentBank = 0;

// selects Xdata bank according to parameter 'bank'
// bank 0 selects LCD, Banks 4..7 64k blocks of SRAM
void XR_SelectBank(unsigned char bank)
{
  register unsigned char NewP0;
  if (bank > 7)
    return;
  else
  NewP0 = P0;
  NewP0 &= ~0x07;
  NewP0 |= bank;
  P0 = NewP0;
  XR_CurrentBank = 0;
  return;
}

// ---------------------------------------------------------
// tests one bank of Xdata SRAM
// TopAddress = Maximum address that gets tested in that bank
// returns 0 if success
//         Bank+1 for the first failure
unsigned char TestXRAM(unsigned char bank, unsigned int TopAddress)
{
  static unsigned char temp;
  temp = 0;
  XR_SelectBank(bank);
  temp  = memTestDataBus(42);
  temp |= memTestAddressBus();
  temp |= memTestDevice(TopAddress);
  XR_SelectBank(BANK_LCD);
  return temp;
}


// http://www.esacademy.com/faq/docs/memtest/index.htm

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
unsigned char  memTestDataBus(unsigned int address)
{
    unsigned char pattern;
    /*
     * Perform a walking 1's test at the given address.
     */
    for (pattern = 1; pattern != 0; pattern <<= 1)
    {
        /*
         * Write the test pattern.
         */
        *(__xdata volatile unsigned char *) address = pattern;

        /*
         * Read it back (immediately is okay for this test).
         */
        if (*(__xdata volatile unsigned char *) address != pattern)
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
unsigned int memTestAddressBus(void)
{
    unsigned int addressMask = (0xFFFF/sizeof(unsigned char) - 1);
    unsigned int offset;
    unsigned int testOffset;

    unsigned char pattern     = 0xAA;
    unsigned char antipattern = 0x55;


    /*
     * Write the default pattern at each of the power-of-two offsets.
     */
    for (offset = 1; offset != 0; offset <<= 1)
    {
        *(__xdata volatile unsigned char *) offset = pattern;
    }

    /*
     * Check for address bits stuck high.
     */
    testOffset = 0;
    *(__xdata volatile unsigned char *) testOffset = antipattern;

    for (offset = 1; (offset & addressMask) != 0; offset <<= 1)
    {
        if (*(__xdata volatile unsigned char *)offset != pattern)
        {
            return (offset);
        }
    }

    *(__xdata volatile unsigned char *)testOffset = pattern;

    /*
     * Check for address bits stuck low or shorted.
     */
    for (testOffset = 1; testOffset != 0; testOffset <<= 1)
    {
        *(__xdata volatile unsigned char *) testOffset = antipattern;

    if (*(__xdata volatile unsigned char *) 0 != pattern)
    {
      return (testOffset);
    }

        for (offset = 1; offset != 0; offset <<= 1)
        {
            if ((*(__xdata volatile unsigned char *)offset != pattern) && (offset != testOffset))
            {
                return (testOffset);
            }
        }

        *(__xdata volatile unsigned char *) testOffset = pattern;
    }

    return (0);

}   /* memTestAddressBus() */



/**********************************************************************
 *
 * Function:    memTestDevice()
 *
 * Description: Test the integrity of a physical memory device by
 *              performing an increment/decrement test over the
 *              entire region.  In the process every storage bit
 *              in the device is tested as a zero and a one.
 *
 * Notes:
 *
 * Parameters:  TopAddress maximum address that the test evaluates
 * Returns:     0 if the test succeeds.  Also, in that case, the
 *              entire memory region will be filled with zeros.
 *
 *              A non-zero result indicates a fault
 **********************************************************************/
unsigned int memTestDevice(unsigned int TopAddress)
{
    unsigned int offset;
    unsigned char pattern;
    TopAddress += 1;
    /*
     * Fill memory with a known pattern.
     */
    pattern =  0xAA;
    offset = 0;
    do
    {
      *(__xdata volatile unsigned char *) (offset) = pattern;
      offset++;
      if(pattern ==12) pattern++;
      pattern++;
    } while (offset != TopAddress);
    /*
     * Check each location.
     */
    pattern =  0xAA;
    offset = 0;
    do
    {
      if ((*(__xdata volatile unsigned char *) (offset) ) != pattern)
        return(1);
 //    *(__xdata volatile unsigned char *) (offset)  = 0;

      offset++;
      if(pattern ==12) pattern++;
      pattern++;
    } while (offset != TopAddress);
    /*
     * Check each location for the inverted pattern and zero it.
     */
    pattern =  0x55;
    offset = 0;
    do
    {
      *(__xdata volatile unsigned char *) (offset) = pattern;
      offset++;
      if(pattern ==12) pattern++;
      pattern++;
    } while (offset != TopAddress);
    /*
     * Check each location and invert it for the second pass.
     */
    pattern =  0x55;
    offset = 0;
    do
    {
      if ((*(__xdata volatile unsigned char *) (offset) ) != pattern)
        return(1);
      offset++;
      if(pattern ==12) pattern++;
      pattern++;
    } while (offset != TopAddress);
    /*
     * Check each location for the inverted pattern.
     */

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
    XR_SelectBank(bank);
    if (bank != *((__xdata  volatile unsigned char *) (address)))
      return bank +1;
  }
  return 0;
}






