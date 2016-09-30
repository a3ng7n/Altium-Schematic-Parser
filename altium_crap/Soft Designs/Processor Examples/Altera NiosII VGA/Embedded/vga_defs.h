#ifndef __VGA_DEFS_H__
#define __VGA_DEFS_H__

//..............................................................................
// VGA Control Register bits
// CTRL.0   - Video System Enable
//            1 - enabled
//            0 - disabled
// CTRL.1   - Vertical Synchronization Interrupt Enable
//            1 - interrupt generation enabled
//            0 - interrupt generation disabled
// CTRL.2   - Horizontal Synchronization Interrupt Enable
//            1 - interrupt generation enabled
//            0 - interrupt generation disabled
// CTRL.3   - Video Memory Page
//            1 - Active Video Memory Page 1
//            0 - Active Video Memory Page 0
// CTRL.4   - Color LookUp Table Bank
//            1 - Active Color LookUp Table Bank 1
//            0 - Active Color LookUp Table Bank 0
// CTRL.6..5 -- Reserved for futer use, not implemented now, returns "00" when being read
//            00 - 8  bit per pixel mode
//            01 - 16 bit per pixel mode - not supported yet
//            10 - 24 bit per pixel mode - not supported yet
//            11 - 32 bit per pixel mode - not supported yet
// CTRL.7   - Pseudo Color Mode
//            0 - Black And White mode
//            1 - Color LookUp mode
// CTRL.8   - Horizontal Syncronization Output Polarity
//            0 - Sync pulse active high
//            1 - Sync pulse active low (most monitors)
// CTRL.9   - Vertical Syncronization Output Polarity
//            0 - Sync pulse active high
//            1 - Sync pulse active low (most monitors)
// CTRL.10  - Compsite Synchronization Output Polarity
//            0 - Sync pulse active high
//            1 - Sync pulse active low
// CTRL.11  - Blanking Syncronization Output Polarity
//            0 - Sync pulse active high
//            1 - Sync pulse active low
// CTRL.31..12 - Unused
//..............................................................................

//..............................................................................
// VGA STAT Register bits
// STAT.0   - Unused
// STAT.1   - Vertical Interrupt Pending - cleared by software
// STAT.2   - Horizontal Interrupt Pending - cleared by software
// STAT.3   - Current Video Memory Page
//            0 - Page 0
//            1 - Page 1
// STAT.4   - Current Color LookUp Table bank
//            0 - Bank 0
//            0 - Bank 1
//..............................................................................

//..............................................................................
// VGA Configuration Registers
//..............................................................................
// HTIM - Horizontal Timing Register
// HTIM.31..24 - Horizontal Syncronization pulse length (in pixels - 1)
// HTIM.32..16 - Back Porch length (in pixels - 1)
// HTIM.15..0  - Visible area (in pixels - 1)
//..............................................................................
// For 800*600 @72 Hz
// Syncronization pulse length - 120 pixels
// Back Porch length           - 67  pixels
// Visible area                - 800 pixels

//..............................................................................
// This set works except is one pixel off - compensated for in graphics software
#define HSyncLengthHighRes    0x78000000
#define HPorchDelayHighRes    0x00420000
#define HVisibleAreaHighRes   0x0000031F
#define HTIM_RegisterHighRes  HSyncLengthHighRes | HPorchDelayHighRes | HVisibleAreaHighRes
//..............................................................................

//..............................................................................
// For 640*480 @72 Hz
// Syncronization pulse length - 40  pixels
// Back Porch length           - 125 pixels
// Visible area                - 640 pixels
#define HSyncLengthLowRes     0x27000000
#define HPorchDelayLowRes     0x00820000
#define HVisibleAreaLowRes    0x0000027F
#define HTIM_RegisterLowRes   HSyncLengthLowRes | HPorchDelayLowRes | HVisibleAreaLowRes

#define HSyncLengthLowResDiv     0x5F000000
#define HPorchDelayLowResDiv     0x00320000
#define HVisibleAreaLowResDiv    0x0000027F
#define HTIM_RegisterLowResDiv   HSyncLengthLowResDiv | HPorchDelayLowResDiv | HVisibleAreaLowResDiv
//..............................................................................

//..............................................................................
// VTIM - Vertical Timing Register
// VTIM.31..24 - Vertical Syncronization pulse length (in lines - 1)
// VTIM.23..16 - Back porch length (in lines - 1)
// VTIM.15..0  - Visible area (in lines - 1)
//..............................................................................
// For 800*600 @72 Hz
// Syncronization pulse length - 6   lines
// Back porch length           - 25  lines
// Visible lines               - 600 lines
#define VSyncLengthHighRes  0x05000000
#define VPorchDelayHighRes  0x00180000
#define VVisibleAreaHighRes 0x00000257
#define VTIM_RegisterHighRes VSyncLengthHighRes | VPorchDelayHighRes | VVisibleAreaHighRes
//..............................................................................

//..............................................................................
// For 640*480 @72 Hz
// Syncronization pulse length - 3   lines
// Back porch length           - 21  lines
// Visible lines               - 480 lines
#define VSyncLengthLowRes   0x02000000
#define VPorchDelayLowRes   0x001D0000
#define VVisibleAreaLowRes  0x000001DF
#define VTIM_RegisterLowRes VSyncLengthLowRes | VPorchDelayLowRes | VVisibleAreaLowRes

#define VSyncLengthLowResDiv   0x02000000
#define VPorchDelayLowResDiv   0x00210000
#define VVisibleAreaLowResDiv  0x000001DF
#define VTIM_RegisterLowResDiv VSyncLengthLowResDiv | VPorchDelayLowResDiv | VVisibleAreaLowResDiv
//..............................................................................

//..............................................................................
// HVLEN - Horizontal and Vertical total length definition register
// HVLEN.31..24 - Total number of pixels per line
// HVLEN.23..0  - Total number of lines per frame
//..............................................................................
// For 800*600 @72 Hz
// Total number of pixels per line - 1040
// Total number of lines per frame - 666
#define HLengthHighRes 0x040F0000
#define VLengthHighRes 0x0000029A
#define HVLEN_RegisterHighRes HLengthHighRes | VLengthHighRes
//..............................................................................

//..............................................................................
// For 640*480 @72 Hz
// Total number of pixels per line - 832
// Total number of lines per frame - 520
//..............................................................................
#define HLengthLowRes 0x033F0000
#define VLengthLowRes 0x00000207
#define HVLEN_RegisterLowRes HLengthLowRes | VLengthLowRes

#define HLengthLowResDiv 0x031F0000
#define VLengthLowResDiv 0x0000020C
#define HVLEN_RegisterLowResDiv HLengthLowResDiv | VLengthLowResDiv
//..............................................................................

//..............................................................................
// Color LookUp table entries for 8 bit per pixel
#define CWhite     0x00FFFFFF
#define CBlack     0x00000000

// those are not grey at all just didn't know their names so named them all as grey
#define CGrey1     0x00FFFFFE
#define CGrey2     0x00FFFFFD
#define CGrey3     0x00FFFEFF
#define CGrey4     0x00FFFEFE
#define CGrey5     0x00FFFEFD
#define CGrey6     0x00FFFDFF
#define CGrey7     0x00FFFDFE
#define CGrey8     0x00FFFDFD
#define CGrey9     0x00FEFFFF
#define CGrey10    0x00FEFFFE
#define CGrey11    0x00FEFFFD
#define CGrey12    0x00FEFEFF
#define CGrey13    0x00FEFEFE
#define CGrey14    0x00FEFEFD
#define CGrey15    0x00FEFDFF
#define CGrey16    0x00FEFDFE
#define CGrey17    0x00FEFDFD
#define CGrey18    0x00FDFFFF
#define CGrey19    0x00FDFFFE
#define CGrey20    0x00FDFFFD
#define CGrey21    0x00FDFEFF
#define CGrey22    0x00FDFEFE
#define CGrey23    0x00FDFEFD
#define CGrey24    0x00FDFDFF
#define CGrey25    0x00FDFDFE
#define CGrey26    0x00FDFDFD

#define CRed       0x00FF0000
#define CRed1      0x00FE0000
#define CRed2      0x00FD0000

#define CGreen     0x0000FF00
#define CGreen1    0x0000FE00
#define CGreen2    0x0000FD00

#define CBlue      0x000000FF
#define CBlue1     0x000000FE
#define CBlue2     0x000000FD

#define CYellow    0x00FFFF00
#define CYellow1   0x00FEFF00
#define CYellow2   0x00FDFF00
#define CYellow3   0x00FFFE00
#define CYellow4   0x00FEFE00
#define CYellow5   0x00FDFE00
#define CYellow6   0x00FFFD00
#define CYellow7   0x00FEFD00
#define CYellow8   0x00FDFD00

#define CMagenta   0x00FF00FF
#define CMagenta1  0x00FE00FF
#define CMagenta2  0x00FD00FF
#define CMagenta3  0x00FF00FE
#define CMagenta4  0x00FE00FE
#define CMagenta5  0x00FD00FE
#define CMagenta6  0x00FF00FD
#define CMagenta7  0x00FE00FD
#define CMagenta8  0x00FD00FD

#define CCyan      0x0000FFFF
#define CCyan1     0x0000FEFF
#define CCyan2     0x0000FDFF
#define CCyan3     0x0000FFFE
#define CCyan4     0x0000FEFE
#define CCyan5     0x0000FDFE
#define CCyan6     0x0000FFFD
#define CCyan7     0x0000FEFD
#define CCyan8     0x0000FDFD
//..............................................................................

//..............................................................................
#define LowResolution      0x0
#define HighResolution     0x1
#define LowResolutionDiv   0x3

#define LookUpTableBank0   0x0
#define LookUpTableBank1   0x1

#define VideoPage0         0x0
#define VideoPage1         0x1
//..............................................................................

#endif

