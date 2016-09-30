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

#include "swplatform.h"

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

#pragma section SHARED_SRAM
pixel_t video1[VIDEOWIDTH * VIDEOHEIGHT];
#pragma endsection

#pragma section SRAM
pixel_t video2[VIDEOWIDTH * VIDEOHEIGHT];
#pragma endsection

/* Variables */
volatile state_t state = STATE_COPY_MODE;
volatile unsigned short  divisor = 11;
canvas_t    *canvas1;
canvas_t    *canvas2;

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
    pixel_t     * outputimg1;           // Buffer associated with canvas1
    pixel_t     * outputimg2;           // Buffer associated with canvas2

    init();
    outputimg1 = (pixel_t *)canvas_get_buffer(canvas1);
    outputimg2 = (pixel_t *)canvas_get_buffer(canvas2);

    *leds |= 0x02;

    inputimg = video1;
    outputimg = outputimg2;

    bt656_set_run_mode( video_capture, BT656_SINGLE );

    for (;;)
    {
        // Get image, filter and display
        if ( bt656_get_run_mode( video_capture ) == BT656_DISABLE )
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

            if (outputimg == outputimg1)
            {
                graphics_set_visible_canvas( vga_output, canvas1 );
                outputimg = outputimg2;
                inputimg = video2;

            }
            else
            {
                graphics_set_visible_canvas( vga_output, canvas2 );
                outputimg = outputimg1;
                inputimg = video1;
            }
            bt656_set_buffer( video_capture, (uint32_t*) inputimg, VIDEO_BUFFER_SIZE );
            bt656_set_run_mode( video_capture, BT656_SINGLE );
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
    // Decode and show the background in video page 1
    jpgdec_set_outputbuffer( jpeg_decoder, canvas_get_buffer(canvas1), XRES * YRES * 2, XRES );
    jpgdec_decode( jpeg_decoder, _lc_ub_background_jpg, (_lc_ue_background_jpg - _lc_ub_background_jpg + 3) & ~3, 0 );
    while( (jpgdec_get_status( jpeg_decoder ) & JPGDEC_STATUS_READY) == 0 ) ;
    // Decode and show the background in video page 2
    jpgdec_set_outputbuffer( jpeg_decoder, canvas_get_buffer(canvas2), XRES * YRES * 2, XRES );
    jpgdec_decode( jpeg_decoder, _lc_ub_background_jpg, (_lc_ue_background_jpg - _lc_ub_background_jpg + 3) & ~3, 0 );
    while( (jpgdec_get_status( jpeg_decoder ) & JPGDEC_STATUS_READY) == 0 ) ;
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

static void setup_button_interrupt(unsigned int num, interrupt_native_handler_t handler)
{
    interrupt_register_native( num, NULL, handler );
    interrupt_configure( num, EDGE_RISING );
    interrupt_acknowledge(num);
    interrupt_enable( num );
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

#define Intr_SW1 5
#define Intr_SW2 6
#define Intr_SW3 7
#define Intr_SW4 8
#define Intr_SW5 9

static void init(void)
{
    // Generated initialization function
    swplatform_init_stacks();

    canvas1 = graphics_get_canvas(vga_output, 0);
    canvas2 = graphics_get_canvas(vga_output, 1);

    io_base     = per_ioport_get_base_address(GPIO);
    leds        = (volatile uint8_t * const) io_base;
    dipswitches = (volatile uint8_t * const) (io_base + 1);

    reset_backgrounds();

    for ( clock_t waitforsync = clock() + CLOCKS_PER_SEC; waitforsync > clock(); )
    {
        // Wait for video to sync
    }

    if ( *dipswitches & 0x80 )
    {
        *leds |= 0x40;
        tvp5150_set_register( video_input, (uint8_t) TVP5150_VIDEO_INPUT, (uint8_t) TVP5150_COMP1 );
    }
    else if ( *dipswitches & 0x40 )
    {
        *leds |= 0x20;
        tvp5150_set_register( video_input, (uint8_t) TVP5150_VIDEO_INPUT, (uint8_t) TVP5150_COMP2 );
    }
    else
    {
        *leds |= 0x80;
        tvp5150_set_register( video_input, (uint8_t) TVP5150_VIDEO_INPUT, (uint8_t) TVP5150_S_VIDEO );
    }

    bt656_set_buffer(video_capture, (uint32_t*)video1, VIDEO_BUFFER_SIZE);
    bt656_set_output_line_size(video_capture, VIDEOWIDTH);
    bt656_set_input_line_size(video_capture, PALWIDTH);
    bt656_set_frame_rate( video_capture, 1 );
    bt656_set_scale(video_capture, ZOOM, ZOOM);

    /* Setup interrupts */
    setup_button_interrupt( Intr_SW1, isr_sw1 );
    setup_button_interrupt( Intr_SW2, isr_sw2 );
    setup_button_interrupt( Intr_SW3, isr_sw3 );
    setup_button_interrupt( Intr_SW4, isr_sw4 );
    setup_button_interrupt( Intr_SW5, isr_sw5 );
}

/*********************************
 *
 * INTERRUPT HANDLERS
 *
 ********************************/

__INTERRUPT_NATIVE void isr_sw1(void)
{
    static clock_t debounce = 0;
    interrupt_acknowledge(Intr_SW1);
    if (( divisor < 16 ) && (clock() > debounce))
    {
        divisor++;
        debounce = clock() + CLOCKS_PER_SEC / 10;
    }
}

__INTERRUPT_NATIVE void isr_sw2(void)
{
    static clock_t debounce = 0;
    interrupt_acknowledge(Intr_SW2);
    if (( divisor > 0 ) && (clock() > debounce))
    {
        divisor--;
        debounce = clock() + CLOCKS_PER_SEC / 10;
    }
}

__INTERRUPT_NATIVE void isr_sw3(void)
{
    interrupt_acknowledge(Intr_SW3);
    state = STATE_COPY_MODE;
}

__INTERRUPT_NATIVE void isr_sw4(void)
{
    interrupt_acknowledge(Intr_SW4);
    state = STATE_SOFTWARE_FILTER;
}

__INTERRUPT_NATIVE void isr_sw5(void)
{
    interrupt_acknowledge(Intr_SW5);
    state = STATE_HARDWARE_FILTER;
}
