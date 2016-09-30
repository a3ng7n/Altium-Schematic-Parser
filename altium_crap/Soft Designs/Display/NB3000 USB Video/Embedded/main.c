#include "generic_devices.h"
#include "devices.h"

#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <timing.h>

#include <interrupts.h>
#include <drv_vga_ili9320.h>
#include <drv_pwm8.h>
#include <drv_jpgdec.h>
#include <touchscreen.h>
#include <usbhost_uvc.h>

// some presentation-related constants
#define FILL_COLOR      VGA_ILI9320_COLOR(0, 0, 255)
#define BRIGHT_MAX      0xFF
#define BRIGHT_MIN      0x10

// input buffer for the partial read demo, could be any size
#define READBUF_SIZE    2048

// this data must be in external memory where the JPGDEC core and VGA core has DMA access
#pragma section external
uint16_t __align(4) tftbuf[320*240];        // tft buffer must be 32 bit aligned
#pragma endsection

// pointers to SSAS drivers
vga_ili9320_t      *vga;
jpgdec_t           *jpgdec;
usbhost_uvc_t      *usbuvc;
usbhost_t          *usbhost;
touchscreen_t      *touchscreen;

// The hufman table used for decoding the jpegs should be addressable by the jpeg decoder core
uint8_t __align(4) huftable[JPGDEC_DEFHUFFMAN_SIZE];

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
    pwm8_t* pwm;

    touchscreen = touchscreen_open(TOUCHSCREEN_1);

    pwm = pwm8_open(DRV_PWM8_1);
    pwm8_set_frequency(pwm, 10000);
    pwm8_enable_controller(pwm);
    pwm8_set_pulsewidth(pwm, BRIGHT_MAX);

    vga = vga_ili9320_open(DRV_VGA_ILI9320_1);
    vga_ili9320_set_screen(vga, (uintptr_t)tftbuf);

    // JPG decoder core only supports 16 bit (RGB565) color mode
    assert(vga_ili9320_bits_per_pixel(vga) == 16);

    // this demo is hardcoded for screensize 240x320
    assert(vga_ili9320_get_width(vga) == 320);
    assert(vga_ili9320_get_height(vga) == 240);

    jpgdec = jpgdec_open(DRV_JPGDEC_1);

    // decode at most the screenarea
    jpgdec_set_area(jpgdec, 0, 0, 320, 240);

    // set output to match screen area with lines truncated at 320 pixels
    jpgdec_set_outputbuffer(jpgdec, vga_ili9320_get_screen(vga),
                            vga_ili9320_get_width(vga) * vga_ili9320_get_height(vga) * sizeof(uint16_t),
                            vga_ili9320_get_width(vga));

    // Parse jpg decoder core a default huffman table.
    jpgdec_set_defhuffman(jpgdec, huftable);
    while (!jpgdec_get_status(jpgdec));

    usbuvc  = usbhost_uvc_open(USBHOST_UVC_1);
    if (usbuvc == NULL)
    {
        printf("Failed usbhost_uvc_open(USBHOST_UVC_1)\n");
    }

    usbhost  = usbhost_open(USBHOST_1);
    if (usbhost == NULL)
    {
        printf("Failed usbhost_open(USBHOST_1)\n");
    }
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
int main(void)
{
    uint8_t            *buf;
    uint32_t           bufsize;
    int32_t            err;
    int32_t            fps;
    touchscreen_data_t ts_data;
    uint64_t           since_ms = 0;

    sys_init();

    printf("USB Video Example\n");
    printf("Insert an USB (Universal Video Class compatible) webcam\n");
    printf("into the USB port. Software will stream video data to the\n");
    printf("LCD Touchscreen.\n\n");

    vga_ili9320_set_refresh_interval(vga, 33);
    vga_ili9320_set_auto_refresh(vga, 1);

    do
    {
        usbhost_process(usbhost);
        if (usbhost_uvc_getstate(usbuvc) == USBHOST_UVC_STATE_CONNECTED)
        {
            fps = usbhost_uvc_init(usbuvc, 30);
            printf("Device framerate initialised to %d fps.\n", fps);
            printf("\nTouch the screen to stop/start the webcam.\n");
            err = usbhost_uvc_start(usbuvc);
            if (err < 0)
            {
                printf("Error %d\n", err);
                return err;
            }
        }
        if ((elapsed_time_ms(since_ms) > 100) && (touchscreen_get_pos(touchscreen, &ts_data) && ts_data.pendown))
        {
            since_ms = clock_ms();
            if (usbhost_uvc_getstate(usbuvc) == USBHOST_UVC_STATE_STOPPED)
            {
                err = usbhost_uvc_start(usbuvc);
                if (err < 0)
                {
                    printf("Error %d\n", err);
                    return err;
                } else
                    printf("Started\r");

            }
            else
            {
                usbhost_uvc_stop(usbuvc);
                printf("Stopped\r");
            }
        }

        buf = usbhost_uvc_get_frame(usbuvc, &bufsize);
        if (buf)
            jpgdec_decode(jpgdec, buf, bufsize, 0);
    } while(1);
}







































































































