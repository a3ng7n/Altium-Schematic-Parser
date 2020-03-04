#ifndef __VGA_DEFS_H__
#define __VGA_DEFS_H__

//..............................................................................
// VGA Control Register bits
// CTRL.0    - Video System Enable
//             1 - enabled
//             0 - disabled
// CTRL.3..1 - Unused
// CTRL.4    - Color LookUp Table Bank
//             1 - Active Color LookUp Table Bank 1
//             0 - Active Color LookUp Table Bank 0
// CTRL.6..5 - 00 - 8  bit per pixel mode
//             01 - 4  bit per pixel mode
//             10 - 2  bit per pixel mode
//             11 - 1  bit per pixel mode
// CTRL.7    - Pseudo Color Mode
//             0 - Black And White mode
//             1 - Color LookUp mode
// CTRL.8    - Horizontal Syncronization Output Polarity
//             0 - Sync pulse active high
//             1 - Sync pulse active low (most monitors)
// CTRL.9    - Vertical Syncronization Output Polarity
//             0 - Sync pulse active high
//             1 - Sync pulse active low (most monitors)
// CTRL.10   - Compsite Synchronization Output Polarity
//             0 - Sync pulse active high
//             1 - Sync pulse active low
// CTRL.11   - Blanking Syncronization Output Polarity
//             0 - Sync pulse active high
//             1 - Sync pulse active low
// CTRL.12   - CDIV enable
//             0 - 50MHz 800x600 or 25MHz 640x480
//             1 - 25MHz 800x600
// CTRL.31..13 - Unused
//..............................................................................

//..............................................................................
// VGA STAT Register bits
// STAT.3..0- Unused
// STAT.4   - Current Color LookUp Table bank
//            0 - Bank 0
//            0 - Bank 1
//..............................................................................

//..............................................................................
// 640*480 @72 Hz with only 240x240 visible

// Syncronization pulse length - 40  pixels
// Back Porch length           - 125 pixels plus rightshifted (640-240)/2=200
// Visible area                - 240 pixels

#define HSyncLengthLowResDivSub     0x5F000000
#define HPorchDelayLowResDivSub     (0x00320000 + 0x00C80000)
#define HVisibleAreaLowResDivSub    0x000000EF
#define HTIM_RegisterLowResDivSub   HSyncLengthLowResDivSub | HPorchDelayLowResDivSub | HVisibleAreaLowResDivSub

// Syncronization pulse length - 3   lines
// Back porch length           - 21  lines  plus rightshifted (480-240)/2=100
// Visible lines               - 480 lines
#define VSyncLengthLowResDivSub   0x02000000
#define VPorchDelayLowResDivSub   (0x00210000 + 0x00640000)
#define VVisibleAreaLowResDivSub  0x000000EF
#define VTIM_RegisterLowResDivSub VSyncLengthLowResDivSub | VPorchDelayLowResDivSub | VVisibleAreaLowResDivSub

// Total number of pixels per line - 832
// Total number of lines per frame - 520
//..............................................................................
#define HLengthLowResDivSub 0x031F0000
#define VLengthLowResDivSub 0x0000020C
#define HVLEN_RegisterLowResDivSub HLengthLowResDivSub | VLengthLowResDivSub
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
#define TFTPanelResolution 0x4
#define PALResolution      0x5
#define PAL8Resolution     0x6
#define CustomResolution   0x7

#define LookUpTableBank0   0x0
#define LookUpTableBank1   0x1

#define VideoPage0         0x0
#define VideoPage1         0x1
//..............................................................................


#define HSyncLengthTFT    0x04000000 // 5
#define HPorchDelayTFT    0x00140000   // 20 -- was h14
#define HVisibleAreaTFT   0x000000EF // 240
#define HTIM_RegisterTFT  HSyncLengthTFT | HPorchDelayTFT | HVisibleAreaTFT

#define VSyncLengthTFT  0x01000000  // 2 or 0?
#define VPorchDelayTFT  0x00010000   // 2
#define VVisibleAreaTFT 0x0000013F // 320
#define VTIM_RegisterTFT VSyncLengthTFT | VPorchDelayTFT | VVisibleAreaTFT

#define HLengthTFT 0x01100000 // 273
#define VLengthTFT 0x00000146  // 327
#define HVLEN_RegisterTFT HLengthTFT | VLengthTFT

//..............................................................................
// HVLEN - Horizontal and Vertical total length definition register
// HVLEN.31..24 - Total number of pixels per line
// HVLEN.23..0  - Total number of lines per frame
//..............................................................................
// For PAL
// Syncronization pulse length - 120 pixels
// Back Porch length           - 18  pixels
// Visible area                - 720 pixels
#define HSyncLengthPALRes    0x78000000
#define HPorchDelayPALRes    0x00120000
#define HVisibleAreaPALRes   0x000002CF
#define HTIM_RegisterPALRes  HSyncLengthPALRes | HPorchDelayPALRes | HVisibleAreaPALRes
//..............................................................................

//..............................................................................
// VTIM - Vertical Timing Register
// VTIM.31..24 - Vertical Syncronization pulse length (in lines - 1)
// VTIM.23..16 - Back porch length (in lines - 1)
// VTIM.15..0  - Visible area (in lines - 1)
//..............................................................................
// For PAL
// Syncronization pulse length - 6   lines
// Back porch length           - 15  lines
// Visible lines               - 291 lines
#define VSyncLengthPALRes  0x05000000
#define VPorchDelayPALRes  0x000E0000
#define VVisibleAreaPALRes 0x00000122
#define VTIM_RegisterPALRes VSyncLengthPALRes | VPorchDelayPALRes | VVisibleAreaPALRes
//..............................................................................

//..............................................................................
// HVLEN - Horizontal and Vertical total length definition register
// HVLEN.31..24 - Total number of pixels per line
// HVLEN.23..0  - Total number of lines per frame
//..............................................................................
// For PAL
// Total number of pixels per line - 864
// Total number of lines per frame - 312
#define HLengthPALRes 0x035F0000
#define VLengthPALRes 0x00000138
#define HVLEN_RegisterPALRes HLengthPALRes | VLengthPALRes
//..............................................................................


//..............................................................................
// HVLEN - Horizontal and Vertical total length definition register
// HVLEN.31..24 - Total number of pixels per line
// HVLEN.23..0  - Total number of lines per frame
//..............................................................................
// For PAL
// Syncronization pulse length - 120 pixels
// Back Porch length           - 18  pixels
// Visible area                - 720 pixels
#define HSyncLengthPAL8Res    0x78000000
#define HPorchDelayPAL8Res    0x00120000
#define HVisibleAreaPAL8Res   0x000002CF
#define HTIM_RegisterPAL8Res  HSyncLengthPAL8Res | HPorchDelayPAL8Res | HVisibleAreaPAL8Res
//..............................................................................

//..............................................................................
// VTIM - Vertical Timing Register
// VTIM.31..24 - Vertical Syncronization pulse length (in lines - 1)
// VTIM.23..16 - Back porch length (in lines - 1)
// VTIM.15..0  - Visible area (in lines - 1)
//..............................................................................
// For PAL
// Syncronization pulse length - 6   lines
// Back porch length           - 15  lines
// Visible lines               - 291 lines
#define VSyncLengthPAL8Res  0x05000000
#define VPorchDelayPAL8Res  0x00180000
#define VVisibleAreaPAL8Res 0x00000237
#define VTIM_RegisterPAL8Res VSyncLengthPAL8Res | VPorchDelayPAL8Res | VVisibleAreaPAL8Res
//..............................................................................

//..............................................................................
// HVLEN - Horizontal and Vertical total length definition register
// HVLEN.31..24 - Total number of pixels per line
// HVLEN.23..0  - Total number of lines per frame
//..............................................................................
// For PAL
// Total number of pixels per line - 864
// Total number of lines per frame - 625
#define HLengthPAL8Res 0x035F0000
#define VLengthPAL8Res 0x00000270
#define HVLEN_RegisterPAL8Res HLengthPAL8Res | VLengthPAL8Res
//..............................................................................


#define HSyncLengthCustRes    0x3C000000
#define HPorchDelayCustRes    0x00480000
#define HVisibleAreaCustRes   0x000002CF
#define HTIM_RegisterCustRes  HSyncLengthCustRes | HPorchDelayCustRes | HVisibleAreaCustRes
#define VSyncLengthCustRes  0x05000000//0x01000000
#define VPorchDelayCustRes  0x00180000//0x00100000
#define VVisibleAreaCustRes 0x00000237//0x00000120
#define VTIM_RegisterCustRes VSyncLengthCustRes | VPorchDelayCustRes | VVisibleAreaCustRes
#define HLengthCustRes 0x035F0000
#define VLengthCustRes 0x00000270//0x00000137
#define HVLEN_RegisterCustRes HLengthCustRes | VLengthCustRes

#endif

