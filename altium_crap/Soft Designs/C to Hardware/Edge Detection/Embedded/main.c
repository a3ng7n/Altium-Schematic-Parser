/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:         Edge detection
|*
|*  COPYRIGHT:          Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:        Detect edges
 */

#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#include <string.h>
#include <ctype.h>
#include "edgedet.h"
#include <per_ioport.h>

#include <devices.h>
#include <interrupts.h>
#include <drv_vga_16bpp.h>
#include <drv_bt656.h>
#include <drv_tvp5150.h>
#include <drv_jpgdec.h>

#define EDGE_TRESHOLD       20

// Video
typedef uint16_t pixel_t;

// Resolution of VGA monitor
#define XRES                640
#define YRES                480
#define IMG_OFFSET          (40 + (25 * XRES))

// Resolution of raw video input, for PAL-B/G
#define PALWIDTH            720
#define PALHEIGHT           580 // 625
#define ZOOM                2
#define VIDEOWIDTH          ((PALWIDTH + ZOOM - 1) / ZOOM)
#define VIDEOHEIGHT         ((PALHEIGHT + ZOOM - 1) / ZOOM)
#define VIDEO_BUFFER_SIZE   (VIDEOWIDTH * VIDEOHEIGHT * sizeof( pixel_t) )

typedef enum { STATE_COPY_MODE, STATE_SOFTWARE_FILTER, STATE_HARDWARE_FILTER } state_t;

uint32_t io_base;

volatile uint8_t * leds;
volatile uint8_t * dipswitches;
volatile uint8_t * usrswitches;

// Local functions
static void init(void);
static void reset_backgrounds( void );
void imgcopy(  uint16_t * input, uint16_t * output, unsigned int width, unsigned int height, unsigned int xres );

// Interrupt handlers
void isr_sw1(void);
void isr_sw2(void);
void isr_sw3(void);
void isr_sw4(void);
void isr_sw5(void);

// The following storage must be located in external memory. This is ensured using the linker settings

#pragma section .bss=xram1
pixel_t vga1[XRES * YRES];
pixel_t video1[VIDEOWIDTH * VIDEOHEIGHT];
#pragma endsection

#pragma section .bss=xram2
pixel_t vga2[XRES * YRES];
pixel_t video2[VIDEOWIDTH * VIDEOHEIGHT];
#pragma endsection

/* Variables */
volatile state_t state = STATE_COPY_MODE;
volatile unsigned short  divisor = 11;
vga_16bpp_t * vga;
bt656_t * bt656;
tvp5150_t * tvp5150;
jpgdec_t * jpgdec;


// The background jpeg is here:
extern uint8_t _lc_ub_background_jpg[];
extern uint8_t _lc_ue_background_jpg[];

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
    pixel_t     * inputimg;             // Points to the input page (where the camera stores its input)
    pixel_t     * outputimg;            // Points to the output page (where the VGA reads its data)

    init();

    *leds |= 0x02;

    inputimg = video1;
    outputimg = vga2;

    bt656_set_run_mode( bt656, BT656_SINGLE );

    for (;;)
    {
        // Get image, filter and display
        if ( bt656_get_run_mode( bt656 ) == BT656_DISABLE )
        {
            switch( state )
            {
            case STATE_HARDWARE_FILTER :
                *leds = (*leds & 0xF0) | 0x01;
                hw_find_edges( inputimg, outputimg + IMG_OFFSET, VIDEOWIDTH, VIDEOHEIGHT, XRES, divisor );
                break;
            case STATE_SOFTWARE_FILTER :
                *leds = (*leds & 0xF0) | 0x02;
                sw_find_edges( inputimg, outputimg + IMG_OFFSET, VIDEOWIDTH, VIDEOHEIGHT, XRES, divisor );
                break;
            default :   // STATE_COPY_MODE
                *leds = (*leds & 0xF0) | 0x04;
                imgcopy( inputimg, outputimg  + IMG_OFFSET, VIDEOWIDTH, VIDEOHEIGHT, XRES );
            }
            vga_16bpp_set_screen( vga, (uintptr_t)outputimg );
            if (outputimg == vga1)
            {
                outputimg = vga2;
                inputimg = video2;

            }
            else
            {
                outputimg = vga1;
                inputimg = video1;
            }
            bt656_set_buffer( bt656, (uint32_t*) inputimg, VIDEO_BUFFER_SIZE );
            bt656_set_run_mode( bt656, BT656_SINGLE );
        }
    }
}

/********************************************************************
|*
|*  FUNCTION    : reset_backgrounds
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Draw the background on both video pages
 */

static void reset_backgrounds( void )
{
    jpgdec = jpgdec_open(DRV_JPGDEC_1);

    // Decode and show the background in video page 1
    jpgdec_set_outputbuffer( jpgdec, (uintptr_t)vga1, XRES * YRES * 2, XRES );
    jpgdec_decode( jpgdec, _lc_ub_background_jpg, (_lc_ue_background_jpg - _lc_ub_background_jpg + 3) & ~3, 0 );
    while( (jpgdec_get_status( jpgdec ) & JPGDEC_STATUS_READY) == 0 ) ;
    // Decode and show the background in video page 2
    jpgdec_set_outputbuffer( jpgdec, (uintptr_t)vga2, XRES * YRES * 2, XRES );
    jpgdec_decode( jpgdec, _lc_ub_background_jpg, (_lc_ue_background_jpg - _lc_ub_background_jpg + 3) & ~3, 0 );
    while( (jpgdec_get_status( jpgdec ) & JPGDEC_STATUS_READY) == 0 ) ;
}

/********************************************************************
|*
|*  FUNCTION    : imgcopy
|*
|*  PARAMETERS  : input = pointer to input image
|*                output = pointer to where filter output should start
|*                width, height = size of image
|*                xres = line size of video (typically 640 or 800 for VGA)
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Copy the image into VGA, not filtering anything
 */

void imgcopy(  uint16_t * input, uint16_t * output, unsigned int width, unsigned int height, unsigned int xres )
{
    uint16_t x, y;
    for ( y = 0; y < height; y++ )
    {
        for ( x = 0; x < width; x++ )
        {
            output[x] = input[x];
        }
        input += width;
        output += xres;
    }
}

/********************************************************************
|*
|*  FUNCTION    : init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize hardware
 */

#define Intr_GPIO_A 5
#define Intr_GPIO_B 6
#define Intr_GPIO_C 7
#define Intr_GPIO_D 8
#define Intr_GPIO_E 9

static void init(void)
{
    vga = vga_16bpp_open( DRV_VGA_16BPP_1 );
    vga_16bpp_set_screen(vga, (uintptr_t)vga1);

    io_base     = per_ioport_get_base_address(GPIO);
    leds        = (volatile uint8_t * const) io_base;
    dipswitches = (volatile uint8_t * const) (io_base + 1);
    usrswitches = (volatile uint8_t * const) (io_base + 2);

    reset_backgrounds();

    for ( clock_t waitforsync = clock() + CLOCKS_PER_SEC; waitforsync > clock(); )
    {
        // Wait for video to sync
    }

    tvp5150 = tvp5150_open(DRV_TVP5150_1);

    if ( *dipswitches & 0x80 )
    {
        *leds |= 0x40;
        tvp5150_set_register( tvp5150, (uint8_t) TVP5150_VIDEO_INPUT, (uint8_t) TVP5150_COMP1 );
    }
    else if ( *dipswitches & 0x40 )
    {
        *leds |= 0x20;
        tvp5150_set_register( tvp5150, (uint8_t) TVP5150_VIDEO_INPUT, (uint8_t) TVP5150_COMP2 );
    }
    else
    {
        *leds |= 0x80;
        tvp5150_set_register( tvp5150, (uint8_t) TVP5150_VIDEO_INPUT, (uint8_t) TVP5150_S_VIDEO );
    }

    bt656 = bt656_open(VIDEO);
    bt656_set_buffer(bt656, (uint32_t*)video1, VIDEO_BUFFER_SIZE);
    bt656_set_output_line_size(bt656, VIDEOWIDTH);
    bt656_set_input_line_size(bt656, PALWIDTH);
    bt656_set_frame_rate( bt656, 1 );
    bt656_set_scale(bt656, ZOOM, ZOOM);

    /* Setup interrupts */
    interrupt_register_native( Intr_GPIO_A, NULL, isr_sw1 );
    interrupt_register_native( Intr_GPIO_B, NULL, isr_sw2 );
    interrupt_register_native( Intr_GPIO_C, NULL, isr_sw3 );
    interrupt_register_native( Intr_GPIO_D, NULL, isr_sw4 );
    interrupt_register_native( Intr_GPIO_E, NULL, isr_sw5 );

    interrupt_configure( Intr_GPIO_A, EDGE_RISING );
    interrupt_configure( Intr_GPIO_B, EDGE_RISING );
    interrupt_configure( Intr_GPIO_C, EDGE_RISING );
    interrupt_configure( Intr_GPIO_D, EDGE_RISING );
    interrupt_configure( Intr_GPIO_E, EDGE_RISING );

    interrupt_acknowledge(Intr_GPIO_A);
    interrupt_acknowledge(Intr_GPIO_B);
    interrupt_acknowledge(Intr_GPIO_C);
    interrupt_acknowledge(Intr_GPIO_D);
    interrupt_acknowledge(Intr_GPIO_E);

    interrupt_enable( Intr_GPIO_A );
    interrupt_enable( Intr_GPIO_B );
    interrupt_enable( Intr_GPIO_C );
    interrupt_enable( Intr_GPIO_D );
    interrupt_enable( Intr_GPIO_E );
}

/*********************************
 *
 * INTERRUPT HANDLERS
 *
 ********************************/

__INTERRUPT_NATIVE void isr_sw1(void)
{
    static clock_t debounce = 0;
    interrupt_acknowledge(Intr_GPIO_A);
    if (( divisor < 16 ) && (clock() > debounce))
    {
        divisor++;
        debounce = clock() + CLOCKS_PER_SEC / 10;
    }
}

__INTERRUPT_NATIVE void isr_sw2(void)
{
    static clock_t debounce = 0;
    interrupt_acknowledge(Intr_GPIO_B);
    if (( divisor > 0 ) && (clock() > debounce))
    {
        divisor--;
        debounce = clock() + CLOCKS_PER_SEC / 10;
    }
}

__INTERRUPT_NATIVE void isr_sw3(void)
{
    interrupt_acknowledge(Intr_GPIO_C);
    state = STATE_COPY_MODE;
}

__INTERRUPT_NATIVE void isr_sw4(void)
{
    interrupt_acknowledge(Intr_GPIO_D);
    state = STATE_SOFTWARE_FILTER;
}

__INTERRUPT_NATIVE void isr_sw5(void)
{
    interrupt_acknowledge(Intr_GPIO_E);
    state = STATE_HARDWARE_FILTER;
}
