/********************************************************************\
|*
|*  Copyright:      Copyright (c) 2008, Altium
|*
|*  Description:    SSAS JPGDEC driver engineering example
|*
|*                      This file shows some ways to use the JPGDEC core
|*                      through its SSAS driver
|*
\********************************************************************/

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <timing.h>
#include <drv_vga_tft.h>
#include <drv_jpgdec.h>

#include <devices.h>

// some presentation-related constants
#define DELAY_LONG      2500
#define DELAY_MEDIUM    1000
#define DELAY_SHORT     250
#define FILL_COLOR      VGA_TFT_COLOR(0, 0, 255)

// input buffer for the partial read demo, could be any size
#define READBUF_SIZE    2048

// outputbuffer of WRITEBUF_LINES of double-screenwidth lines
// for this demo to work the size has to be a multiple of 16 screenlines
// this supported screensize is hardcoded to a width of 240 pixels
#define WRITEBUFDEMO_WIDTH   240
#define WRITEBUFDEMO_HEIGHT  16
#define WRITEBUF_SIZE        ((2 * WRITEBUFDEMO_HEIGHT) * (2 * WRITEBUFDEMO_WIDTH) * sizeof(uint16_t))

// this data must be in external memory where the JPGDEC core and VGA core has DMA access
#pragma section external
uint8_t readbuf[READBUF_SIZE];
uint8_t __align(4) writebuf[WRITEBUF_SIZE]; // output buffers must be 32 bit aligned
uint16_t __align(4) tftbuf[320*240];        // tft buffer must be 32 bit aligned
#pragma endsection

// pointers to SSAS drivers
vga_tft_t *vga;
jpgdec_t *jpgdec;

// The JPEG is linked to the program in binary form. Find it here:
extern __no_sdata const uint8_t _lc_ub_balloon_jpg;
extern __no_sdata const uint8_t _lc_ue_balloon_jpg;


/**********************************************************************
|*
|*  Function    : fillbuf
|*
|*  Parameters  : buf = start of videobuffer
|*                size = length of buffer in pixels
|*                color = color to use for filling
|*
|*  Returns     : None
|*
|*  Description : Fill a videobuffer with the given color
 */
 void fillbuf(uint16_t *buf, size_t size, uint16_t color)
{
    for (int i = size; i; --i)
    {
        *buf++ = color;
    }
}


/**********************************************************************
|*
|*  Function    : fillscreen
|*
|*  Parameters  : color = color to use for filling
|*
|*  Returns     : None
|*
|*  Description : Fill the visible screen with the given color
 */
void fillscreen(uint16_t color)
{
    fillbuf((uint16_t*) vga_tft_get_screen(vga), vga_tft_get_width(vga) * vga_tft_get_height(vga), color);
}


/**********************************************************************
|*
|*  Function    : print_status
|*
|*  Parameters  : msg = text to print after status
|*
|*  Returns     : None
|*
|*  Description : print JPG decoder status flags in readable form
 */
void print_status(char *msg)
{
    uint32_t status = jpgdec_get_status(jpgdec);

    printf("Status =%s%s%s%s%s%s%s%s, %s\n",
           (status & JPGDEC_STATUS_READY            ) ? " Ready" : "",
           (status & JPGDEC_STATUS_READEMPTY        ) ? " ReadEmpty" : "",
           (status & JPGDEC_STATUS_WRITEFULL        ) ? " WriteFull" : "",
           (status & JPGDEC_STATUS_ERROR            ) ? " Error" : "",
           (status & JPGDEC_STATUS_ERROR_NOTAJPG    ) ? " NotAJpg" : "",
           (status & JPGDEC_STATUS_ERROR_CORRUPT    ) ? " Corrupt" : "",
           (status & JPGDEC_STATUS_ERROR_UNSUPPORTED) ? " Unsupported" : "",
           (status & JPGDEC_STATUS_RESET            ) ? " Reset" : "",
           msg);
}


/**********************************************************************
|*
|*  Function    : demo_oneshot
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Basic usage: decode a JPG image to the screen in a single call
 */
void demo_oneshot(void)
{
    printf("Demo: basic one shot decoding\n");
    fillscreen(FILL_COLOR);
    delay_ms(DELAY_MEDIUM);

    // decode at most the screenarea
    jpgdec_set_area(jpgdec, 0, 0, 240, 320);

    // set output to match screen area with lines truncated at 240 pixels
    jpgdec_set_outputbuffer(jpgdec, vga_tft_get_screen(vga),
                            vga_tft_get_width(vga) * vga_tft_get_height(vga) * sizeof(uint16_t),
                            vga_tft_get_width(vga));

    jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 0);
    while (!jpgdec_get_status(jpgdec)) {}

    printf("finished\n\n");
    delay_ms(DELAY_LONG);
}


/**********************************************************************
|*
|*  Function    : demo_partial
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Decode areas from the middle of a JPG image
 */
void demo_partial(void)
{
    printf("Demo: partial decodings with changing size\n");
    fillscreen(FILL_COLOR);
    delay_ms(DELAY_MEDIUM);

    // set output to match screen area with lines truncated at 240 pixels
    jpgdec_set_outputbuffer(jpgdec, vga_tft_get_screen(vga),
                            vga_tft_get_width(vga) * vga_tft_get_height(vga) * sizeof(uint16_t),
                            vga_tft_get_width(vga));

    // decode varying area sizes of the image
    for (int i = 0; i <= 40; i += 2)
    {
        jpgdec_set_area(jpgdec, 120 - i, 160, 160, 200);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 0);
        while (!jpgdec_get_status(jpgdec)) {}
    }
    for (int i = 0; i <= 40; i += 1)
    {
        jpgdec_set_area(jpgdec, 80, 160 - i, 160, 200);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 0);
        while (!jpgdec_get_status(jpgdec)) {}
    }
    for (int i = 0; i <= 40; i += 2)
    {
        jpgdec_set_area(jpgdec, 80, 120, 160 + i, 200);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 0);
        while (!jpgdec_get_status(jpgdec)) {}
    }
    for (int i = 0; i <= 40; i += 1)
    {
        jpgdec_set_area(jpgdec, 80, 120, 200, 200 + i);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 0);
        while (!jpgdec_get_status(jpgdec)) {}
    }

    printf("finished\n\n");
    delay_ms(DELAY_LONG);

}


/**********************************************************************
|*
|*  Function    : demo_shifting
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Decode partial areas from the middle of a JPG image
|*                while shifting the origin
|*                (this gives the impression of a moving window over the image)
 */
void demo_shifting(void)
{
    int vgawidth = vga_tft_get_width(vga);

    printf("Demo: partial decodings with changing origin\n");
    fillscreen(FILL_COLOR);
    delay_ms(DELAY_MEDIUM);

    // set output to match screen area with lines truncated at 240 pixels
    jpgdec_set_outputbuffer(jpgdec, vga_tft_get_screen(vga),
                            vga_tft_get_width(vga) * vga_tft_get_height(vga) * sizeof(uint16_t),
                            vga_tft_get_width(vga));

    // shift the 240x320 image around in a 200x280 "window"
    for (int i = 0; i <= 40; i += 2)
    {
        jpgdec_set_area(jpgdec, i, 0, 200 + i, 280);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, i);
        while (!jpgdec_get_status(jpgdec)) {}
    }
    for (int i = 0; i <= 40; i += 1)
    {
        jpgdec_set_area(jpgdec, 40, i, 240, 280 + i);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 40 + i * vgawidth);
        while (!jpgdec_get_status(jpgdec)) {}
    }
    for (int i = 0; i <= 40; i += 2)
    {
        jpgdec_set_area(jpgdec, 40 - i, 40, 240 - i, 320);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 40 - i + 40 * vgawidth);
        while (!jpgdec_get_status(jpgdec)) {}
    }
    for (int i = 0; i <= 40; i += 1)
    {
        jpgdec_set_area(jpgdec, 0, 40 - i, 200, 320 - i);
        jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, (40 - i) * vgawidth);
        while (!jpgdec_get_status(jpgdec)) {}
    }

    printf("finished\n\n");
    delay_ms(DELAY_LONG);

}



/**********************************************************************
|*
|*  Function    : demo_blockread
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Decode an image while reading the inputdata under
|*                user control in chunks at a time
|*                (very usable when reading an image from a filesystem)
 */
void demo_blockread(void)
{
    const uint8_t *data;
    size_t size;

    printf("Demo: blockwise reading of the inputdata\n");
    fillscreen(FILL_COLOR);
    delay_ms(DELAY_MEDIUM);

    data = &_lc_ub_balloon_jpg;
    size = &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg;

    // decode at most the screenarea
    jpgdec_set_area(jpgdec, 0, 0, 240, 320);

    // set output to match screen area with lines truncated at 240 pixels
    jpgdec_set_outputbuffer(jpgdec, vga_tft_get_screen(vga),
                            vga_tft_get_width(vga) * vga_tft_get_height(vga) * sizeof(uint16_t),
                            vga_tft_get_width(vga));

    jpgdec_decode(jpgdec, 0, 0, 0);

    for (;;)
    {
        uint32_t status = jpgdec_get_status(jpgdec);

        if (status & JPGDEC_STATUS_READY)
        {
            printf("finished\n\n");
            break;
        }

        if (status & JPGDEC_STATUS_READEMPTY)
        {
            if (size > 0)
            {
                int readsize = size;
                if (readsize > READBUF_SIZE)
                { readsize = READBUF_SIZE; }

                memcpy(readbuf, data, readsize);
                data += readsize;

                jpgdec_set_inputdata(jpgdec, readbuf, readsize);
                delay_ms(DELAY_SHORT);

                print_status("continue decoding");
                jpgdec_decode_continue(jpgdec, 0);
                continue;
            }
            else
            {
                print_status("JPG truncated, abort\n");
                break;
            }
        }

        if (status != 0)
        {
            print_status("unexpected, abort\n");
            break;
        }
    }

    delay_ms(DELAY_LONG);
}


/**********************************************************************
|*
|*  Function    : demo_blockwrite
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Decode an image while writing the outputdata under
|*                user control in separate chunks at a time
|*                (very usable when the output has to be postprocessed
|*                before further usage, and allocating a buffer to hold
|*                the complete decoded JPG image is not feasible)
 */

void demo_blockwrite(void)
{
    int pixeloffset = 0;
    uint16_t *screenpos = (uint16_t*) vga_tft_get_screen(vga);

    printf("Demo: blockwise writing of the resulting image\n");
    fillscreen(FILL_COLOR);
    delay_ms(DELAY_MEDIUM);

    // decode at most twice writebufwidth x twice screenheight (later on we will downsize 50%)
    jpgdec_set_area(jpgdec, 0, 0, 2 * WRITEBUFDEMO_WIDTH, 2 * 320);

    // set output to a workbuffer of twice the screenwidth and twice WRITEBUFDEMO_HEIGHT lines height
    jpgdec_set_outputbuffer(jpgdec, (uintptr_t) writebuf, sizeof(writebuf), 2 * WRITEBUFDEMO_WIDTH);
    fillbuf((uint16_t*) writebuf, (2 * WRITEBUFDEMO_HEIGHT) * (2 *  WRITEBUFDEMO_WIDTH), FILL_COLOR);

    jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, pixeloffset);

    for (;;)
    {
        uint32_t status = jpgdec_get_status(jpgdec);

        if (status & (JPGDEC_STATUS_READY | JPGDEC_STATUS_WRITEFULL))
        {
            // copy dirty writebuffer 50% downscaled to the screen
            uint16_t *writebufpos = (uint16_t*) writebuf;

            for (int x = 0; x < WRITEBUFDEMO_HEIGHT; ++x)
            {
                for (int y = 0; y < WRITEBUFDEMO_WIDTH; ++y)
                {
                    *screenpos++ = *writebufpos++;
                    ++writebufpos; // skip a pixel
                }
                writebufpos += 2 * WRITEBUFDEMO_WIDTH;  // skip a line
            }
        }

        if (status & JPGDEC_STATUS_READY)
        {
            printf("finished\n\n");
            break;
        }

        if (status & JPGDEC_STATUS_WRITEFULL)
        {
            // normally we would have to look at jpgdec_get_writeaddress() to see where
            // the core wants to write next, but as we know decoding of any JPG will always
            // take place in consequetive chunks of 8 or 16 lines, and we specified a buffer
            // of a multiple of 16 screenlines, we can just move on to the next batch of lines

            delay_ms(DELAY_SHORT);
            print_status("continue decoding");

            // re-use the writebuffer...
            fillbuf((uint16_t*) writebuf, (2 * WRITEBUFDEMO_HEIGHT) * (2 *  WRITEBUFDEMO_WIDTH), FILL_COLOR);

            // ...but decode with a different offset (WRITEBUFDEMO_HEIGHT lines down)
            pixeloffset += (2 * WRITEBUFDEMO_HEIGHT) * (2 *  WRITEBUFDEMO_WIDTH);
            jpgdec_decode_continue(jpgdec, pixeloffset);
            continue;
        }

        if (status != 0)
        {
            print_status("unexpected, abort\n");
            break;
        }
    }

    delay_ms(DELAY_LONG);
}


/**********************************************************************
|*
|*  Function    : demo_headerfirst
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Processing only the header of a JPG file,
|*                optionally continuing decoding afterwards
 */
void demo_headerfirst(void)
{
    printf("Demo: reading header\n");
    fillscreen(FILL_COLOR);
    delay_ms(DELAY_MEDIUM);

    // make sure the JPGDEC core cannot write any pixels by setting a zero-sized output buffer
    jpgdec_set_outputbuffer(jpgdec, 0, 0, 0);

    jpgdec_decode(jpgdec, &_lc_ub_balloon_jpg, &_lc_ue_balloon_jpg - &_lc_ub_balloon_jpg, 0);

    for (;;)
    {
        uint32_t status = jpgdec_get_status(jpgdec);

        if (status & JPGDEC_STATUS_READY)
        {
            printf("finished\n\n");
            break;
        }

        if (status & JPGDEC_STATUS_WRITEFULL)
        {
            // when the decoder wants to write the first pixel, it has succesfully parsed the header
            print_status("header has been parsed");
            printf("image size is %i x %i\n", jpgdec_get_size_x(jpgdec), jpgdec_get_size_y(jpgdec));

            delay_ms(DELAY_MEDIUM);

            // we could for instance allocate a buffer based on the size,
            // for this demo just use the screen

            // decode at most the screenarea
            jpgdec_set_area(jpgdec, 0, 0, 240, 320);

            // set output to match screen area with lines truncated at 240 pixels
            jpgdec_set_outputbuffer(jpgdec, vga_tft_get_screen(vga),
                                    vga_tft_get_width(vga) * vga_tft_get_height(vga) * sizeof(uint16_t),
                                    vga_tft_get_width(vga));

            jpgdec_decode_continue(jpgdec, 0);
            continue;
        }

        if (status != 0)
        {
            print_status("unexpected, abort\n");
            break;
        }
    }

    print_status("finished\n");
    delay_ms(DELAY_LONG);
}


/**********************************************************************
|*
|*  Function    : sys_init
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Initialize system
 */
void sys_init(void)
{
    printf("SSAS JPGDEC demonstrations\n\n");

    vga = vga_tft_open(DRV_VGA_TFT_0);
    vga_tft_set_screen(vga, (uintptr_t)tftbuf);

    // JPG decoder core only supports 16 bit (RGB565) color mode
    assert(vga_tft_bits_per_pixel(vga) == 16);

    // this demo is hardcoded for screensize 240x320
    assert(vga_tft_get_width(vga) == 240);
    assert(vga_tft_get_height(vga) == 320);

    jpgdec = jpgdec_open(DRV_JPGDEC_0);
}


/**********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Run all the individual demonstrations
 */
void main(void)
{
    sys_init();

    for (;;)
    {
        demo_oneshot();
        demo_partial();
        demo_shifting();
        demo_blockread();
        demo_blockwrite();
        demo_headerfirst();
    }
}


