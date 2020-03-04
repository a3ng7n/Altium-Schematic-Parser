/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:
|*
|*  COPYRIGHT:          Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:
|*
 */

#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <math.h>   
#include "hardware.h"

#include <devices.h>
#include <graphics.h>
#include <drv_bt656.h>
#include <drv_tvp5150.h>

#define PI  3.141592654

// Resolution of raw video input, for PAL-B/G
#define PALWIDTH            720
#define PALHEIGHT           580 // 625
#define VIDEOWIDTH          450
#define VIDEOHEIGHT         450
#define VIDEO_BUFFER_SIZE   (VIDEOWIDTH * VIDEOHEIGHT * sizeof( pixel_t) )

// Video: defines for centering
#define VIDEO_X0    (VIDEOWIDTH / 2)
#define VIDEO_XMIN  (VIDEO_X0 - VIDEOWIDTH)
#define VIDEO_XMAX  (VIDEOWIDTH - VIDEO_X0)

#define VIDEO_Y0    (VIDEOHEIGHT / 2)
#define VIDEO_YMIN  (VIDEO_Y0 - VIDEOHEIGHT)
#define VIDEO_YMAX  (VIDEOHEIGHT - VIDEO_Y0)

// TFT type
typedef uint16_t pixel_t;

// Resolution of TFT
#define TFT_XRES    240
#define TFT_YRES    320

// TFT: defines for centering
#define TFT_X0      (TFT_XRES / 2)
#define TFT_XMIN    (TFT_X0 - TFT_XRES)
#define TFT_XMAX    (TFT_XRES - TFT_X0)

#define TFT_Y0      (TFT_YRES / 2)
#define TFT_YMIN    (TFT_Y0 - TFT_YRES)
#define TFT_YMAX    (TFT_YRES - TFT_Y0)

// TFT memory
//#pragma section .bss=.tft
//volatile pixel_t tft[TFT_XRES * TFT_YRES];
//#pragma endsection

// Video memory
#pragma section .bss=.video
volatile pixel_t imgbuf[VIDEO_BUFFER_SIZE];
#pragma endsection


// Local variables
graphics_t *graphics;
canvas_t *canvas;
uintptr_t canvas_buffer;
bt656_t * bt656;
tvp5150_t * tvp5150;

// Function prototypes
static void init( void );

// To be moved to hardware:
void rotate( pixel_t * input, pixel_t * output, uint16_t angle, uint16_t scale );
void set_tabs( uint16_t angle, int sinval, int cosval );

// Sine and cosine tables, must be moved to hardware
int sintab[360];
int costab[360];

/**********************************************************************
|*
|*  FUNCTION    : main
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Start the ball rolling
 */

void main( void )
{
    int16_t scale;
    int16_t delta = +1;
    clock_t t1, t2;
    init();

    scale = 256;

loop:
    for ( uint16_t alpha = 0; alpha < 360; alpha += 3 )
    {
        t1 = clock();
        rotate( (pixel_t *)imgbuf, (pixel_t *)canvas_buffer, alpha, scale );
        t2 = clock();
        *(volatile uint8_t *)Base_GPIO = ((t2 - t1) / (CLOCKS_PER_SEC/1000)) & 0xFF;
        scale += delta;
        if (scale > 1024) delta = -1;
        else if (scale < 256) delta = 1;
    }
    goto loop;
}


/**********************************************************************
|*
|*  FUNCTION    : init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize the hardware
 */

static void init( void )
{
    graphics = graphics_open(GRAPHICS_1);
    canvas = graphics_get_visible_canvas(graphics);
    canvas_buffer = canvas_get_buffer(canvas);

    tvp5150 = tvp5150_open(DRV_TVP5150_1);
    if (tvp5150 != NULL)
    {
        tvp5150_set_crop( tvp5150, (PALWIDTH - VIDEOWIDTH)/ 2, (PALHEIGHT - VIDEOHEIGHT)/ 2, (PALWIDTH - VIDEOWIDTH)/ 2, (PALHEIGHT - VIDEOHEIGHT)/ 2 );
    }

    bt656 = bt656_open(VIDEO);
    if (bt656 != NULL)
    {
        bt656_set_output_line_size(bt656, VIDEOWIDTH);
        bt656_set_scale(bt656, 1, 1);
        bt656_set_frame_rate( bt656, 1 );

        uint16_t x1 = (((PALWIDTH - VIDEOWIDTH)/ 2) < 512) && (((PALWIDTH - VIDEOWIDTH)/ 2) > 0)? ((PALWIDTH - VIDEOWIDTH)/ 2) & 0xFFFE : 0;
        uint16_t x2 = (((PALWIDTH - VIDEOWIDTH)/ 2) < 512) && (((PALWIDTH - VIDEOWIDTH)/ 2) > 0)? ((PALWIDTH - VIDEOWIDTH)/ 2) & 0xFFFE : 0;
        uint16_t visible_byte_per_line = (PALWIDTH - x2 - x1);
        bt656_set_input_line_size(bt656, visible_byte_per_line);

        bt656_set_buffer( bt656, (uint32_t*) imgbuf, VIDEO_BUFFER_SIZE );
        bt656_set_run_mode( bt656, BT656_RUN );
    }

    // Initialize sine and cosine tables
    for ( uint16_t angle = 0; angle < 360; angle++ )
    {
        set_tabs( angle, (int) (sin( angle * PI / 180.0 ) * 1024),  (int) (cos( angle * PI / 180.0 ) * 1024) );
    }
}

/**********************************************************************
|*
|*  FUNCTION    : set_tabs
|*
|*  PARAMETERS  : angle = index in sine / cosine arrays
|*                sinval = value to be set in sine array for said angle
|*                cosval = value to be set in cosine array for said angle
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Fill the sine- and cosine arrays (located in hardware)
|*                This function must be implemented in hardware
 */

void set_tabs( uint16_t angle, int sinval, int cosval )
{
    sintab[angle] = sinval;
    costab[angle] = cosval;
}

/**********************************************************************
|*
|*  FUNCTION    : rotate
|*
|*  PARAMETERS  : input = pointer to input image, sized VIDEOWIDTH * VIDEOHEIGHT pixels
|*                output = pointer to TFT memory, sized TFT_XMAX * TFT_YMAX
|*                angle = index in sine / cosine arrays
|*                scale = scale factor (in parts per 256)
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Rotate the input image by 'angle' degrees and write
|*                the output to TFT memory
|*                This function will be implemented in hardware
 */

void rotate( pixel_t * input, pixel_t * output, uint16_t angle, uint16_t scale )
{
    int x, y;
    int xdest, ydest;
    int xsrc, ysrc;

    for ( y = TFT_YMIN; y < TFT_YMAX; y++ )
    {
        ydest = y * scale / 256;
        for ( x = TFT_XMIN; x < TFT_XMAX; x++ )
        {
            xdest = x * scale / 256;
            xsrc = (xdest * costab[angle] - ydest * sintab[angle]) / 1024 + VIDEO_X0;
            ysrc = (xdest * sintab[angle] + ydest * costab[angle]) / 1024 + VIDEO_Y0;
            *output = ( (xsrc >= 0) && (xsrc < VIDEOWIDTH) && (ysrc >= 0) && (ysrc < VIDEOHEIGHT) ) ? input[xsrc + ysrc * VIDEOWIDTH] : 0x1F;
            output++;
        }
    }
}
