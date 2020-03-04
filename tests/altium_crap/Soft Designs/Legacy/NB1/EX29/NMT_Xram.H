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
//
// Nanoboard XRAM is 2*128k static ram data bus is shared with LCD
// The tester FPGA divides that into 4 banks of 64k
// The top four bytes in each bank are mapped to the LCD display

#ifndef __NBT_XRAM_H__
#define __NBT_XRAM_H__

// The following memory test functions are adapted from :
// http://www.esacademy.com/faq/docs/memtest/index.htm

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
unsigned int memTestAddressBus(void);
 
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
unsigned char  memTestDataBus(unsigned int address);

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
unsigned int memTestDevice(unsigned int TopAddress);



// selects Xdata bank according to parameter 'bank'
// bank 0 selects LCD, Banks 4..7 64k blocks of SRAM
void XR_SelectBank(unsigned char bank);

// ---------------------------------------------------------
// tests one bank of Xdata SRAM
// TopAddress = Maximum address that gets tested in that bank
// returns 0 if success
//         Bank+1 for the first failure
unsigned char TestXRAM(unsigned char bank, unsigned int TopAddress);


// ----------------------------------------------------------------------------------------
//  writes the bank number to each bank at address 'address' and reads it back to verify
//  if a bank select line is not present or stuck, it will read back the wrong data
//  NoOfBanks is the Number of memory banks to be tested
//  returns 0 if success
//  bank with 1st error +1 if failed;
// ----------------------------------------------------------------------------------------
unsigned char memTestBankSwitching(unsigned int address, unsigned char NoOfBanks);



#endif // __NBT_XRAM_H__

