/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   M25PX0 Flash memory controller device driver
|*
\*****************************************************************************/

#ifndef __FLASH_M25PX0_H__
#define __FLASH_M25PX0_H__

//..............................................................................
// Memory organisation
//
// Memory in the M25PX0 is viewed in the following ways for different commands
//    512K bytes              - view for reading and writing. in both cases a
//                              byte address is given to open a read / write session
//    X/2K pages of 256 bytes - view for writing, in that only one page can be
//                              written at a time. Durine one write, the current
//                              address will wrap from the end of the page back
//                              to the start.
//    X*2 sectors of 512Kbits - view for protection, erasing

//..............................................................................
// M25PX0 XMbit Flash Memory Instructions
#define M25PX0_WRSR                  0x01     // Write Status Register Instruction
#define M25PX0_PP                    0x02     // Page Program Instruction
#define M25PX0_READ                  0x03     // Read Data Bytes Instruction
#define M25PX0_WRDI                  0x04     // Write Disable Instruction
#define M25PX0_RDSR                  0x05     // Read Status Register Instruction
#define M25PX0_WREN                  0x06     // Write Enable Instruction
#define M25PX0_FAST_READ             0x0B     // Read Data Bytes at Higher Speed Instruction
#define M25PX0_BE                    0xC7     // Bulk Erase Instruction
#define M25PX0_DP                    0xB9     // Deep Powerdown
#define M25PX0_RES                   0xAB     // Release from Deep Powerdown/Read Electronic Signature
#define M25PX0_SE                    0xD8     // Sector Erase Instruction

//..............................................................................
// "English" versions of Flash Memory Instructions
#define M25PX0_WriteStatusRegister   M25PX0_WRSR
#define M25PX0_DataWrite             M25PX0_PP
#define M25PX0_SlowRead              M25PX0_READ
#define M25PX0_WriteDisable          M25PX0_WRDI
#define M25PX0_ReadStatusRegister    M25PX0_RDSR
#define M25PX0_WriteEnable           M25PX0_WREN
#define M25PX0_FastRead              M25PX0_FAST_READ
#define M25PX0_BulkErase             M25PX0_BE
#define M25PX0_EnterDeepPowerDown    M25PX0_DP
#define M25PX0_ExitDeepPowerDown     M25PX0_RES
#define M25PX0_EraseSector           M25PX0_SE

//..............................................................................
//       Status Register Format
//
//           b7                                   B0
//       +------+---+---+-----+-----+-----+-----+-----+
//       | SRWD | 0 | 0 | BP2 | BP1 | BP0 | WEL | WIP |
//       +---+--+---+---+--+--+--+--+--+--+--+--+--+--+
//           |             |     |     |     |     |
//        Status Register  +-----+-----+     |     |
//        Write Protect          |           |     |
//                       Block Protect Bits  |     |
//                                           |     |
//                                  Write Enable   |
//                                  Latch Bit      |
//                                                 |
//                                            Write In
//                                            Progress Bit
//
#define M25PX0_SRWD             0x80    // Mask for Status Register Write Protect
#define M25PX0_BP0              0x04    // Mask for Block Protect Bit 0
#define M25PX0_BP1              0x08    // Mask for Block Protect Bit 1
#define M25PX0_BP2              0x10    // Mask for Block Protect Bit 2
#define M25PX0_WEL              0x02    // Mask for Write Enable Latch Bit
#define M25PX0_WIP              0x01    // Mask for Write in Progress Bit

//..............................................................................
// Masks for Status Register Block Protection Bits BP[0..2]
#define M25P10_PROTECT_NONE     0x00  // Mask for no write protection
#define M25P10_PROTECT_3        0x04  // Mask for write protection sector 3 only
#define M25P10_PROTECT_2_3      0x08  // Mask for write protection sector 2 and 3
#define M25P10_PROTECT_ALL      0x0C  // Mask for write protection all sectors

#define M25P20_PROTECT_NONE     0x00  // Mask for no write protection
#define M25P20_PROTECT_3        0x04  // Mask for write protection sector 3 only
#define M25P20_PROTECT_2_3      0x08  // Mask for write protection sector 2 and 3
#define M25P20_PROTECT_ALL      0x0C  // Mask for write protection all sectors

#define M25P40_PROTECT_NONE     0x00  // Mask for no write protection
#define M25P40_PROTECT_7        0x04  // Mask for write protection sector 7 only
#define M25P40_PROTECT_6_7      0x08  // Mask for write protection sector 6 and 7
#define M25P40_PROTECT_4_7      0x0C  // Mask for write protection sector 4 to 7
#define M25P40_PROTECT_ALL      0x10  // Mask for write protection all sectors

#define M25P80_PROTECT_NONE     0x00  // Mask for no write protection
#define M25P80_PROTECT_15       0x04  // Mask for write protection sector 15 only
#define M25P80_PROTECT_14_15    0x08  // Mask for write protection sector 14 and 15
#define M25P80_PROTECT_12_15    0x0C  // Mask for write protection sector 12 to 15
#define M25P80_PROTECT_8_15     0x10  // Mask for write protection sector 8 to 15
#define M25P80_PROTECT_ALL      0x14  // Mask for write protection all sectors

//..............................................................................
// RES Response (electronic signature)
#define M25P10_RES_RESPONSE     0x10   // expected response to RES command
#define M25P20_RES_RESPONSE     0x11   // expected response to RES command
#define M25P40_RES_RESPONSE     0x12   // expected response to RES command
#define M25P80_RES_RESPONSE     0x13   // expected response to RES command

//..............................................................................
// Open a flash file for reading starting at 'address' or for writing at page
//   'page'. Subsequent spi_send_receive_last commands will read or write a
//   byte and autoincrement the address. Any number of bytes can be read
//   or written.
// When reading, the device will wrap around to address 0, making the whole
//   memory space accessible.
// When writing, the device will wrap back to the start of the page, making
//   only the current page accessible.
void          m25px0_open_read  ( unsigned int base, unsigned int address );
void          m25px0_open_write ( unsigned int base, unsigned int address );
void          m25px0_close      ( unsigned int base );

//..............................................................................
// Set all bits in device to 1
void          m25px0_erase_all                 ( unsigned int base );

//..............................................................................
// Set all bits in the sector containing 'address' to 1
void          m25px0_erase_sector              ( unsigned int base, unsigned int address );

//..............................................................................
// read the electronic signature to wake the device
// also takes chip out of deep powerdown
unsigned char m25px0_read_electronic_signature ( unsigned int base );

//..............................................................................
// Use m25px0_is_writing to determine when the device is busy writing to the
// status register, to memory or is erasing
unsigned int  m25px0_is_writing                ( unsigned int base );

//..............................................................................
// Use m25px0_wait_while_writing to ensure the device is not busy before sending
// another command.
void          m25px0_wait_while_writing        ( unsigned int base );

//..............................................................................
// Enable block protection. Use M25PX0_PROTECT_ defines to set protection level.
void          m25px0_set_block_protect         ( unsigned int base, unsigned char protection );

void          m25px0_enter_deep_powerdown      ( unsigned int base );
void          m25px0_exit_deep_powerdown       ( unsigned int base );


#endif





































