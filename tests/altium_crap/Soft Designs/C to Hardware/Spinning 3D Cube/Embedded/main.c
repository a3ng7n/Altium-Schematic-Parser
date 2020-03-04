/*
 * This example displays a spinning cube on the TFT. It shows the difference in speed between the software version and the CHC (hardware) version of the same code.
 * To run the software version push button SW1, for the CHC version push button SW2.
 */


#include <stdio.h>
#include <string.h>
#include <time.h>
#include <io.h>
#include <math.h>
#include <stdlib.h>
#include <devices.h>
#include <interrupts.h>
#include <drv_vga_tft.h>

#include "bitmap.h"
#include "font.h"

#include "3dcube.h"
#include "3dcube_chc.h"
#include "bmp.h"

#include <per_ioport_cfg_instance.h>
/*
 * Constants
 */

#define CLKFREQ  50000000L

#define VGA_RAM1 0x1000000
#define VGA_RAM2 0x1028000

/*
 * Macros
 */

#if 0
#define LEDS    (*(uint8_t *)Base_PORTIO)
#define BUTTONS (*(uint8_t *)Base_PORTIO)
#else
#define LEDS    (*(uint8_t *)per_ioport_instance_table[PORTIO].baseaddress)
#define BUTTONS (*(uint8_t *)per_ioport_instance_table[PORTIO].baseaddress)
#endif

/*
 * Variables
 */

/* static variables */
/* buffers */
static int16_t zbuf[VGA_PIXELS] __align(4);
static uint16_t tft_buf0[VGA_PIXELS] __align(4);
static uint16_t tft_buf1[VGA_PIXELS] __align(4);
static uint16_t *tft_buf;

static t_FONT screenfont = {&arial8, rgb(24, 24, 24), 0};

//static vga_context_t vga_settings;
vga_tft_t *vga_tft;

//static volatile bool vga_vsync;
static volatile uint32_t frames;
static volatile uint32_t frameduration;
static bool chc_mode;


/*
 * Function prototypes
 */

static void init(void);
static void gonio_init(void);


/***************************************/
/* gonio related variables and defines */
/***************************************/

int16_t sintable[ROTATIONSTEPS];
#define r2sin(r) sintable[(r) & (ROTATIONSTEPS - 1)]
#define r2cos(r) sintable[((r) + (ROTATIONSTEPS / 4)) & (ROTATIONSTEPS - 1)]
#define r2deg(r) (((r) * 360) / ROTATIONSTEPS)


/****************************************/
/* matrix related variables and defines */
/****************************************/

static matrix_t m;

#define  matrix_multiply(m, a, b) \
    m.xx = (a.xx*b.xx + a.xy*b.yx + a.xz*b.zx) / (1 << FIXEDPOINTSCALE); \
    m.xy = (a.xx*b.xy + a.xy*b.yy + a.xz*b.zy) / (1 << FIXEDPOINTSCALE); \
    m.xz = (a.xx*b.xz + a.xy*b.yz + a.xz*b.zz) / (1 << FIXEDPOINTSCALE); \
    \
    m.yx = (a.yx*b.xx + a.yy*b.yx + a.yz*b.zx) / (1 << FIXEDPOINTSCALE); \
    m.yy = (a.yx*b.xy + a.yy*b.yy + a.yz*b.zy) / (1 << FIXEDPOINTSCALE); \
    m.yz = (a.yx*b.xz + a.yy*b.yz + a.yz*b.zz) / (1 << FIXEDPOINTSCALE); \
    \
    m.zx = (a.zx*b.xx + a.zy*b.yx + a.zz*b.zx) / (1 << FIXEDPOINTSCALE); \
    m.zy = (a.zx*b.xy + a.zy*b.yy + a.zz*b.zy) / (1 << FIXEDPOINTSCALE); \
    m.zz = (a.zx*b.xz + a.zy*b.yz + a.zz*b.zz) / (1 << FIXEDPOINTSCALE);




/*
 * Next functions must be placed in internal memory
 */


/*********************************************************************
|*
|*  Function    : matrix_unity
|*
|*  Parameters  : none
|*
|*  Returns     : none
|*
|*  Description : Initialize matrix m
 */

void matrix_unity(void)
{
    m.xx = 1 << FIXEDPOINTSCALE; m.xy = 0; m.xz = 0;
    m.yx = 0; m.yy = 1 << FIXEDPOINTSCALE; m.yz = 0;
    m.zx = 0; m.zy = 0; m.zz = 1 << FIXEDPOINTSCALE;
}


/*********************************************************************
|*
|*  Function    : matrix_rotate_x
|*
|*  Parameters  : r - rotation index
|*
|*  Returns     : none
|*
|*  Description : Rotate matrix m by r steps in x direction (step-size is defined by ROTATIONSTEPS
 */

void matrix_rotate_x(int16_t r)
{
    matrix_t a =
    {
        1 << FIXEDPOINTSCALE,    0,         0,
                0,          r2cos(r), -r2sin(r),
                0,          r2sin(r),  r2cos(r)
    };
    matrix_t b = m;

    matrix_multiply(m, a, b);
}


/*********************************************************************
|*
|*  Function    : matrix_rotate_y
|*
|*  Parameters  : r - rotation index
|*
|*  Returns     : none
|*
|*  Description : Rotate matrix m by r steps in y direction (step-size is defined by ROTATIONSTEPS
 */

void matrix_rotate_y(int16_t r)
{
    matrix_t a =
    {
         r2cos(r),         0,          r2sin(r),
            0,     1 << FIXEDPOINTSCALE,    0,
        -r2sin(r),         0,          r2cos(r)
    };
    matrix_t b = m;

    matrix_multiply(m, a, b);
}


/*********************************************************************
|*
|*  Function    : matrix_rotate_z
|*
|*  Parameters  : r - rotation index
|*
|*  Returns     : none
|*
|*  Description : Rotate matrix m by r steps in z direction (step-size is defined by ROTATIONSTEPS
 */

void matrix_rotate_z(int16_t r)
{
    matrix_t a =
    {
        r2cos(r), -r2sin(r),         0,
        r2sin(r),  r2cos(r),         0,
           0,        0,      1 << FIXEDPOINTSCALE
    };
    matrix_t b = m;

    matrix_multiply(m, a, b);
}


/*********************************************************************
|*
|*  Function    : check_button
|*
|*  Parameters  : none
|*
|*  Returns     : none
|*
|*  Description : Check if a button is pressed, change mode
 */

void check_button(void)
{
    if ( !(BUTTONS & 0x01) && chc_mode)
    {
        chc_mode = false;
        frames = 0;
        frameduration = 0;
    }
    else if ( !(BUTTONS & 0x02) && !chc_mode)
    {
        chc_mode = true;
        frames = 0;
        frameduration = 0;
    }
}


/*********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : none
|*
|*  Returns     : never returns from function
|*
|*  Description : Main function
 */

int main(void)
{
    int16_t rotate_x, rotate_y, rotate_z;
    uint8_t rotatemode;
    uint8_t randomtimer;
    uint32_t avg_duration;
    float avg_frames;
    uint32_t framestart;
    char displaybuf[100];
    uint16_t *bmp1_ptr;
    uint16_t *bmp2_ptr;
    uint16_t *bmp3_ptr;
    uint16_t *bmp4_ptr;
    uint16_t *bmp5_ptr;
    uint16_t *bmp6_ptr;

    static uint16_t bmp1[2 + BMPSIZE*BMPSIZE];
    static uint16_t bmp2[2 + BMPSIZE*BMPSIZE];
    static uint16_t bmp3[2 + BMPSIZE*BMPSIZE];
    static uint16_t bmp4[2 + BMPSIZE*BMPSIZE];
    static uint16_t bmp5[2 + BMPSIZE*BMPSIZE];
    static uint16_t bmp6[2 + BMPSIZE*BMPSIZE];

    vga_tft = vga_tft_open(DRV_VGA_TFT_1);
    vga_tft_set_screen(vga_tft, (uintptr_t)tft_buf0);
    tft_buf = tft_buf1;

    gonio_init();

    bmp_decode(side1, bmp1, sizeof(bmp1) / sizeof(uint16_t));
    bmp_decode(side2, bmp2, sizeof(bmp2) / sizeof(uint16_t));
    bmp_decode(side3, bmp3, sizeof(bmp3) / sizeof(uint16_t));
    bmp_decode(side4, bmp4, sizeof(bmp4) / sizeof(uint16_t));
    bmp_decode(side5, bmp5, sizeof(bmp5) / sizeof(uint16_t));
    bmp_decode(side6, bmp6, sizeof(bmp6) / sizeof(uint16_t));

    rotate_x = rotate_y = rotate_z = 0;
    rotatemode = 0;
    randomtimer = 1;
    frames = 0;
    avg_duration = 0;
    avg_frames = 0.0;
    frameduration = 0;

    for (;;)
    {
        matrix_unity();
        matrix_rotate_x(rotate_x & (ROTATIONSTEPS - 1));
        matrix_rotate_y(rotate_y & (ROTATIONSTEPS - 1));
        matrix_rotate_z(rotate_z & (ROTATIONSTEPS - 1));

        // at most one of two opposite sides is visible
        if (m.zz > 0)
        {
            bmp1_ptr = (uint16_t *) (int) (bmp1 + 2);
            bmp2_ptr = NULL;
        }
        else
        {
            bmp1_ptr = NULL;
            bmp2_ptr = (uint16_t *) (int) (bmp2 + 2);
        }

        if (m.zx > 0)
        {
            bmp3_ptr = (uint16_t *) (int) (bmp3 + 2);
            bmp4_ptr = NULL;
        }
        else
        {
            bmp3_ptr = NULL;
            bmp4_ptr = (uint16_t *) (int) (bmp4 + 2);
        }

        if (m.zy > 0)
        {
            bmp5_ptr = (uint16_t *) (int) (bmp5 + 2);
            bmp6_ptr = NULL;
        }
        else
        {
            bmp5_ptr = NULL;
            bmp6_ptr = (uint16_t *) (int) (bmp6 + 2);
        }

        framestart = (int) (clock() / (CLKFREQ / 1000000));

        if (chc_mode)
        {
            chc_buf_clear(tft_buf, zbuf);
            chc_plot_init(m.xx, m.xy, m.xz, m.yx, m.yy, m.yz, m.zx, m.zy, m.zz, tft_buf, (int16_t *) (int) zbuf);
            chc_plot_cube(bmp1_ptr, bmp2_ptr, bmp3_ptr, bmp4_ptr, bmp5_ptr, bmp6_ptr);

        }
        else
        {
            buf_clear(tft_buf, zbuf);
            plot_init(m.xx, m.xy, m.xz, m.yx, m.yy, m.yz, m.zx, m.zy, m.zz, tft_buf, (int16_t *) (int) zbuf);
            plot_cube(bmp1_ptr, bmp2_ptr, bmp3_ptr, bmp4_ptr, bmp5_ptr, bmp6_ptr);

        }

        LEDS = 1;

        frameduration += (int) (clock() / (CLKFREQ / 1000000)) - framestart;
        frames++;

        if (chc_mode)
        {
            if (frames == 10)
            {
                avg_duration = frameduration / 10;
                avg_frames = 10000000.0 / frameduration;
                frames = 0;
                frameduration = 0;
            }
            sprintf(displaybuf, "CHC: %5.2f frame/s (%i us/frame)", avg_frames, avg_duration);
        }
        else
        {
            avg_duration = frameduration;
            avg_frames = 1000000.0 / frameduration;
            frames = 0;
            frameduration = 0;
            sprintf(displaybuf, "Software: %5.2f frame/s (%i us/frame)", avg_frames, avg_duration);
        }

        strdraw(10, 5, displaybuf, &screenfont, tft_buf);

        if (!--randomtimer)
        {
            rotatemode = 1 + rand() % 6;
            randomtimer = 5 + rand() % 15;
        }

        switch(rotatemode)
        {
        case 1: ++rotate_x; break;
        case 2: --rotate_x; break;
        case 3: ++rotate_y; break;
        case 4: --rotate_y; break;
        case 5: ++rotate_z; break;
        case 6: --rotate_z; break;
        default: break;
        }

        vga_tft_set_screen(vga_tft, (uintptr_t)tft_buf);
        tft_buf = tft_buf == tft_buf0 ? tft_buf1 : tft_buf0;

        do
        {
            check_button();
        } while (!vga_tft_vsync_passed(vga_tft));

        LEDS = 0;
    }
}


/*********************************************************************
|*
|*  Function    : gonio_init
|*
|*  Parameters  : none
|*
|*  Returns     : none
|*
|*  Description : Initialize sintable
 */

void gonio_init(void)
{
    double a = 0;

    for (int step = 0; step < ROTATIONSTEPS; ++step)
    {
        sintable[step] = sin(a) * (1 << FIXEDPOINTSCALE);

        a += PI / (ROTATIONSTEPS / 2);
    }
}

