#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

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
#define PAL_Y_RES               580

/* TFT setup */
#define TFT_X_RES               240
#define TFT_Y_RES               320
#define TFT_BUFFER_SIZE         ( TFT_X_RES * TFT_Y_RES * BYTES_PER_PIXEL )
#define TFT_BASE_0              0x00000
#define TFT_BASE_1              TFT_BUFFER_SIZE

/* Video setup */
#define VID_X_RES               TFT_X_RES
#define VID_Y_RES               286
#define VID_X_ZOOM              1
#define VID_Y_ZOOM              0
#define VID_FRAME_SKIP          0
#define VID_Y_BORDER            ( ( TFT_Y_RES - VID_Y_RES ) / 2 )
#define VID_BUFFER_SIZE         ( VID_X_RES * VID_Y_RES * BYTES_PER_PIXEL )
#define VID_BASE_0              0x00000
#define VID_BASE_1              0x40000

/* Histogram setup */
#define HIS_Y_RES               200
#define HIS_COLOR               0x738E          /* Gray */
#define HIS_CACHE_SIZE          4
#define HIS_MAX_INTENSITY       ( EXTRACT_R(-1)+EXTRACT_G(-1)+EXTRACT_B(-1) )

