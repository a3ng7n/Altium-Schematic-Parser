/********************************************************************\
|*
|* Version      : $Version$
|*
|* Copyright    : Copyright (c) 2006, Altium
|*
|* Description  : Test CHC using pixels, lines and circles
|*
\********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <io.h>
#include <devices.h>
#include <graphics.h>
#include "testfuncs.h"

typedef uint16_t pixel_t;

#define XRES    240
#define YRES    320
#define XMAX    (XRES-1)
#define YMAX    (YRES-1)
#pragma section .bss=.xram
pixel_t *tftbuf;
#pragma endsection

graphics_t *graphics;
canvas_t *canvas;

static void init( void );

/*********************************************************************
|*
|*  Function    : main
|*
|*  Parameters  : None
|*
|*  Returns     : None
|*
|*  Description : Start the ball rolling
 */

void main( void )
{
    volatile clock_t t1, t2;
    clock_t dt_sw_plot, dt_hw_plot;
    clock_t dt_sw_line, dt_hw_line;
    clock_t dt_sw_circle, dt_hw_circle;

    init();

    puts( "CHC Performance test\n" );

    /*
     * Do the test for software plot
     */

    t1 = clock();

    for ( uint16_t x = 0; x < XRES; x++ )
    {
        for ( uint16_t y = 0; y < YRES; y++ )
        {
            sw_plot( (void *)tftbuf, x, y, 0xAAAA );
        }
    }

    t2 = clock();
    dt_sw_plot = t2 - t1;

    t1 = clock();
    for ( uint16_t x = 0; x < XRES; x++ )
    {
        for ( uint16_t y = 0; y < YRES; y++ )
        {
            sw_plot( (void *)tftbuf, x, y, 0x5555 );
            sw_plot( (void *)tftbuf, x, y, 0x5555 );
        }
    }
    t2 = clock();
    dt_sw_plot = (t2 - t1) - dt_sw_plot;


    /*
     * Do same test for hardware generated plot
     */

    t1 = clock();

    for ( uint16_t x = 0; x < XRES; x++ )
    {
        for ( uint16_t y = 0; y < YRES; y++ )
        {
            hw_plot( (void *)tftbuf, x, y, 0xAAAA );
        }
    }

    t2 = clock();
    dt_hw_plot = t2 - t1;

    t1 = clock();
    for ( uint16_t x = 0; x < XRES; x++ )
    {
        for ( uint16_t y = 0; y < YRES; y++ )
        {
            hw_plot( (void *)tftbuf, x, y, 0x5555 );
            hw_plot( (void *)tftbuf, x, y, 0x5555 );
        }
    }
    t2 = clock();
    dt_hw_plot = (t2 - t1) - dt_hw_plot;

    /*
     * Draw lines in software
     */

    t1 = clock();
    for ( uint16_t x = 0; x < XRES; x++ )
    {
        sw_line( (void *)tftbuf, 0, 0, x, YMAX, 0xF800 );
    }
    for ( uint16_t y = 0; y < YRES; y++ )
    {
        sw_line( (void *)tftbuf, XMAX, YMAX, 0, y, 0x07E0 );
    }
    for ( uint16_t y = YMAX; y ; y-- )
    {
        sw_line( (void *)tftbuf, 0, YMAX, XMAX, y, 0x001F );
    }

    for ( uint16_t x = 0; x < XRES; x++ )
    {
        sw_line( (void *)tftbuf, XMAX, 0, x, YMAX, 0xF800 );
    }

    for ( uint16_t x = XMAX; x; x-- )
    {
        sw_line( (void *)tftbuf, XMAX, YMAX, x, 0, 0x07E0 );
    }
    t2 = clock();
    dt_sw_line = (t2 - t1);

    t1 = clock();
    for ( uint16_t x = 0; x < XRES; x++ )
    {
        sw_line( (void *)tftbuf, 0, 0, x, YMAX, 0xF800 );
        sw_line( (void *)tftbuf, 0, 0, x, YMAX, 0xF800 );
    }
    for ( uint16_t y = 0; y < YRES; y++ )
    {
        sw_line( (void *)tftbuf, XMAX, YMAX, 0, y, 0x07E0 );
        sw_line( (void *)tftbuf, XMAX, YMAX, 0, y, 0x07E0 );
    }
    for ( uint16_t y = YMAX; y ; y-- )
    {
        sw_line( (void *)tftbuf, 0, YMAX, XMAX, y, 0x001F );
        sw_line( (void *)tftbuf, 0, YMAX, XMAX, y, 0x001F );
    }

    for ( uint16_t x = 0; x < XRES; x++ )
    {
        sw_line( (void *)tftbuf, XMAX, 0, x, YMAX, 0xF800 );
        sw_line( (void *)tftbuf, XMAX, 0, x, YMAX, 0xF800 );
    }

    for ( uint16_t x = XMAX; x; x-- )
    {
        sw_line( (void *)tftbuf, XMAX, YMAX, x, 0, 0x07E0 );
        sw_line( (void *)tftbuf, XMAX, YMAX, x, 0, 0x07E0 );
    }
    t2 = clock();
    dt_sw_line = (t2 - t1) - dt_sw_line;

    /*
     * Draw lines in hardware
     */

    t1 = clock();
    for ( uint16_t x = 0; x < XRES; x++ )
    {
        hw_line( (void *)tftbuf, 0, 0, x, YMAX, 0xF800 );
    }
    for ( uint16_t y = 0; y < YRES; y++ )
    {
        hw_line( (void *)tftbuf, XMAX, YMAX, 0, y, 0x07E0 );
    }
    for ( uint16_t y = YMAX; y ; y-- )
    {
        hw_line( (void *)tftbuf, 0, YMAX, XMAX, y, 0x001F );
    }

    for ( uint16_t x = 0; x < XRES; x++ )
    {
        hw_line( (void *)tftbuf, XMAX, 0, x, YMAX, 0xF800 );
    }

    for ( uint16_t x = XMAX; x; x-- )
    {
        hw_line( (void *)tftbuf, XMAX, YMAX, x, 0, 0x07E0 );
    }
    t2 = clock();
    dt_hw_line = (t2 - t1);

    t1 = clock();
    for ( uint16_t x = 0; x < XRES; x++ )
    {
        hw_line( (void *)tftbuf, 0, 0, x, YMAX, 0xF800 );
        hw_line( (void *)tftbuf, 0, 0, x, YMAX, 0xF800 );
    }
    for ( uint16_t y = 0; y < YRES; y++ )
    {
        hw_line( (void *)tftbuf, XMAX, YMAX, 0, y, 0x07E0 );
        hw_line( (void *)tftbuf, XMAX, YMAX, 0, y, 0x07E0 );
    }
    for ( uint16_t y = YMAX; y ; y-- )
    {
        hw_line( (void *)tftbuf, 0, YMAX, XMAX, y, 0x001F );
        hw_line( (void *)tftbuf, 0, YMAX, XMAX, y, 0x001F );
    }

    for ( uint16_t x = 0; x < XRES; x++ )
    {
        hw_line( (void *)tftbuf, XMAX, 0, x, YMAX, 0xF800 );
        hw_line( (void *)tftbuf, XMAX, 0, x, YMAX, 0xF800 );
    }

    for ( uint16_t x = XMAX; x; x-- )
    {
        hw_line( (void *)tftbuf, XMAX, YMAX, x, 0, 0x07E0 );
        hw_line( (void *)tftbuf, XMAX, YMAX, x, 0, 0x07E0 );
    }
    t2 = clock();
    dt_hw_line = (t2 - t1) - dt_hw_line;

    /*
     * Do tests for drawing circles in software
     */

    t1 = clock();
    for ( uint16_t r = 1; r < (XRES/2); r++ )
    {
        sw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x5555 );
    }
    t2 = clock();
    dt_sw_circle = t2 - t1;

    t1 = clock();
    for ( uint16_t r = 1; r < (XRES/2); r++ )
    {
        sw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x0000 );
        sw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x5555 );
    }
    t2 = clock();
    dt_sw_circle = (t2 - t1) - dt_sw_circle;

    /*
     * Do tests for drawing circles in hardware
     */

    t1 = clock();
    for ( uint16_t r = 1; r < (XRES/2); r++ )
    {
        hw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x0000 );
    }
    t2 = clock();
    dt_hw_circle = t2 - t1;

    t1 = clock();
    for ( uint16_t r = 1; r < (XRES/2); r++ )
    {
        hw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x0000 );
        hw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x5555 );
    }
    t2 = clock();
    dt_hw_circle = (t2 - t1) - dt_hw_circle;

    printf( "SW plot = %llu cycles\n", dt_sw_plot );
    printf( "HW plot = %llu cycles\n", dt_hw_plot );
    printf( "Performance gain = %lld%%\n\n", (100ULL * dt_sw_plot) / dt_hw_plot - 100ULL);

    printf( "SW line = %llu cycles\n", dt_sw_line );
    printf( "HW line = %llu cycles\n", dt_hw_line );
    printf( "Performance gain = %lld%%\n\n", (100ULL * dt_sw_line) / dt_hw_line - 100ULL);

    printf( "SW circle = %llu cycles\n", dt_sw_circle );
    printf( "HW circle = %llu cycles\n", dt_hw_circle );
    printf( "Performance gain = %lld%%\n\n", (100ULL * dt_sw_circle) / dt_hw_circle - 100ULL);
}

void init( void )
{
    graphics = graphics_open(GRAPHICS_1);
    canvas = graphics_get_visible_canvas(graphics);
    tftbuf = (pixel_t*)canvas_get_buffer(canvas);
}
