#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#define ANGLE_360_DEGREES       4096
#define ANGLE_300_DEGREES       ( 5 * ANGLE_360_DEGREES / 6 )
#define ANGLE_240_DEGREES       ( 2 * ANGLE_360_DEGREES / 3 )
#define ANGLE_180_DEGREES       ( ANGLE_360_DEGREES / 2 )
#define ANGLE_120_DEGREES       ( ANGLE_360_DEGREES / 3 )
#define ANGLE_60_DEGREES        ( ANGLE_360_DEGREES / 6 )
#define ANGLE_0_DEGREES         0


/* Enable/Disable components */
#define ENABLE_CONTRAST_SATURATION_ADJUST  0

/* Enable/Disable multiplier usage */
//#define USE_RGB_FILTER_HARDWARE_MULTIPLIERS      1
//#define USE_HSI_FILTER_HARDWARE_MULTIPLIERS       1

/* Type definitions */
#ifdef __CHC__
typedef uint24_t                rgb_t;                                          // Pixel Value (R8G8B8)
typedef uint10_t                his_t;                                          // Histogram Value (H10)
typedef uint8_t                 color_t;                                        // Color Channel Value (C8)
typedef uint30_t                hsi_t;                                          // HSI Value (H12S10I8)
typedef uint12_t                hue_t;                                          // Hue (H12)
typedef uint10_t                saturation_t;                                   // Saturation (S10)
typedef uint8_t                 intensity_t;                                    // Intensity (I7.1 = I8)
typedef uint20_t                addr_t;                                         // Address (A20)
typedef uint10_t                y_t;                                            // Vertical pixel pos. (X10)
typedef uint37_t                xyh_t;                                          // Pix.Pos/Hist.Idx (X10Y10H17)
#endif

/* Frequency setup */
#define FREQ_HZ                 ( 50 * 1000 * 1000 )
#define PAL_FREQ_HZ             FREQ_HZ
#define PIXEL_FREQ_HZ           ( 5 * 1000 * 1000 )

/* BT656 color modes */
#define BT656_CM_GREY8          1
#define BT656_CM_GREY16         2
#define BT656_CM_GREY32         3
#define BT656_CM_RGB8           4
#define BT656_CM_RGB16          5
#define BT656_CM_RGB32          6

/* Color setup */
typedef uint16_t                pixel_t;        /* 5-6-5 */
#define BYTES_PER_PIXEL         sizeof( pixel_t )
#define BT656_COLOR_MODE        BT656_CM_RGB16
#define BT656_GRAYSCALE_MODE    BT656_CM_GREY16
#define MASK_R                  0xF800
#define MASK_G                  0x07E0
#define MASK_B                  0x001F
#define EXTRACT_R(rgb16)        ( (rgb16 & MASK_R) >> 10)
#define EXTRACT_G(rgb16)        ( (rgb16 & MASK_G) >> 5 )
#define EXTRACT_B(rgb16)        ( (rgb16 & MASK_B) << 1 )

/* PAL setup */
#define PAL_X_RES               720
#define PAL_Y_RES               576

/* TFT setup */
#define TFT_X_RES               240
#define TFT_Y_RES               320
#define TFT_BUFFER_SIZE         ( TFT_X_RES * TFT_Y_RES * BYTES_PER_PIXEL )
#define TFT_BASE_0              0x00080000  /* Should not be NULL! */

/* VGA setup */
#define VGA_X_RES               800
#define VGA_Y_RES               600
#define VGA_BUFFER_SIZE         ( VGA_X_RES * VGA_Y_RES * BYTES_PER_PIXEL )
#define VGA_BUFFER_BIT          0x00100000
#define VGA_BASE                0x00200000  /* Should not be NULL! */
#define VGA_BASE_0              VGA_BASE
#define VGA_BASE_1              VGA_BASE | VGA_BUFFER_BIT

/* Video setup */
#define VID_X_RES               PAL_X_RES
#define VID_Y_RES               PAL_Y_RES // 520
#define VID_X_ZOOM              0
#define VID_Y_ZOOM              0
#define VID_FRAME_SKIP          0
#define VID_X_BORDER            ( ( VGA_X_RES - VID_X_RES ) / 2 )
#define VID_Y_BORDER            ( ( VGA_Y_RES - VID_Y_RES ) / 2 )
#define VID_BUFFER_SIZE         ( VGA_X_RES * VID_Y_RES * BYTES_PER_PIXEL )
#define VID_BASE_0              0x00000000

/* Histogram setup */
#define HIS_X_RES               256                                             // Number of samples in the histogram
#define HIS_Y_RES               256                                             // Maximum histogram height in pixels
#define HIS_COLOR               0x738E                                          // Gray
#define HIS_MOVING_AVERAGE      16                                              // Moving average weight factor
#define HIS_FRACT_BITS          8                                               // Histogram integer math precision
#define HIS_DELTA_X             ( (HIS_X_RES << HIS_FRACT_BITS) / VID_X_RES )   // Histogram horizontal scaling factor



