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
// The tester FPGA divides RAM that into 16 banks of 64k

#ifndef __NBT_XRAM_H__
#define __NBT_XRAM_H__

// --------------------------------------------------------
// Sets all control lines for onboard ram to a safe state
// --------------------------------------------------------
void ObRamInit(void);

void ObRamWrite(unsigned long address, unsigned char);

unsigned char ObRamRead(unsigned long address);


// The following memory test functions are adapted from :
// http://www.netrino.com/Articles/MemoryTesting/paper.html

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
unsigned int memTestAddressBus(unsigned long nBytes);

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
unsigned long  memTestDataBus(unsigned long address);

// selects Xdata bank according to parameter 'bank'
// bank 0 selects LCD, Banks 4..7 64k blocks of SRAM
void XR_SelectBank(unsigned char bank);

// ----------------------------------------------------------------------------------------
//  writes the bank number to each bank at address 'address' and reads it back to verify
//  if a bank select line is not present or stuck, it will read back the wrong data
//  NoOfBanks is the Number of memory banks to be tested
//  returns 0 if success
//  bank with 1st error +1 if failed;
// ----------------------------------------------------------------------------------------
unsigned char memTestBankSwitching(unsigned int address, unsigned char NoOfBanks);

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
unsigned char memTestBank(unsigned char bank);

#endif // __NBT_XRAM_H__

