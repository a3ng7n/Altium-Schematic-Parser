#ifndef __HWARE_H__
#define __HWARE_H__

#include "hardware.h"

#define BAUDRATE 115200
#define XTALFREQ (50000000.0)
#define FCLK (XTALFREQ / 1.5)

//#define MAX_FILESIZE 500000   // Maximum number of bytes that are read from the MOD file
#define RAM_SIZE (1024 * 1024)  // size of external SRAM in bytes
#define RX_TIMEOUT 5          // timeout in seconds for end of file detection
#define WAIT_FOR_FILE_DOWNLOAD     // waits for a character before playing

#define INT1FREQ (50.0) // frequency of Tick Timer interrupt (20ms for mod files)

#define PCON_OFS    0x00
#define SCON_OFS    0x04
#define SBUF_OFS    0x08
#define SRELL_OFS   0x0C
#define SRELH_OFS   0x10
#define TCON_OFS    0x14
#define TL_OFS      0x18
#define TH_OFS      0x1C
#define ADCON_OFS   0x20

#define SERIAL_BASE_ADDR   0xFF000100
#define SER_REG(x) *((volatile unsigned char*)(Base_Serial + (x)))

#define PARALLEL_BASE_ADDR 0xFF000000
#define PAR_REG(x)  *((volatile unsigned int *)(Base_Audio + (x)))

#define OFS_START_ADR    0
#define OFS_END_ADR      1
#define OFS_START_REPEAT 2
#define OFS_END_REPEAT   3
#define OFS_VOLUME       4
#define OFS_CONTROL      5
#define OFS_DATA_RATE    6

#define OFS_DSP          0x20
#define OFS_KBD          0x21
#define ADR_START        0x22
#define OFS_BARGRAPH     0x23

extern const unsigned int __no_sdata _lc_ub_stack;   // symbol from *.map file to determine top of RAM used by C program

#endif  //__HWARE_H__
