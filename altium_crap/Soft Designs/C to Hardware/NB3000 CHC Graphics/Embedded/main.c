/********************************************************************\
|*
|* Copyright    : Copyright (c) 2010, Altium
|*
|* Description  : Test CHC using pixels, lines and circles
|*
\********************************************************************/

#include <time.h>
#include "swplatform.h"
#include "testfuncs.h"

typedef uint16_t pixel_t;

void main( void )
{
    clock_t t1, t2;
    clock_t dt_sw_plot, dt_hw_plot;
    clock_t dt_sw_line, dt_hw_line;
    clock_t dt_sw_circle, dt_hw_circle;
    graphics_t *graphics;
    canvas_t *canvas;
    pixel_t *tftbuf;


    puts( "CHC Performance Test, file " __FILE__ ", compiled on " __DATE__ "\n" );

    graphics = graphics_open(GRAPHICS);
    canvas   = graphics_get_visible_canvas(graphics);
    tftbuf   = (pixel_t*)canvas_get_buffer(canvas);

    puts( "Running Tests:\n"
          "--------------" );

    /*
     * Do the test for software plot
     */
    puts("- SW plot (pixels)");

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
    puts("- HW plot (pixels)");

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
    puts("- SW lines");

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
    puts("- HW lines");

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
    puts("- SW circles");

    t1 = clock();
    for ( uint16_t r = 1; r < (YRES/2); r+=4 )
    {
        sw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x5555 );
    }
    t2 = clock();
    dt_sw_circle = t2 - t1;

    t1 = clock();
    for ( uint16_t r = 1; r < (YRES/2); r+=4 )
    {
        sw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x0000 );
        sw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x5555 );
    }
    t2 = clock();
    dt_sw_circle = (t2 - t1) - dt_sw_circle;

    /*
     * Do tests for drawing circles in hardware
     */
    puts("- HW circles");

    t1 = clock();
    for ( uint16_t r = 1; r < (YRES/2); r+=4 )
    {
        hw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x0000 );
    }
    t2 = clock();
    dt_hw_circle = t2 - t1;

    t1 = clock();
    for ( uint16_t r = 1; r < (YRES/2); r+=4 )
    {
        hw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x0000 );
        hw_circle( (void *)tftbuf, (XRES/2), (YRES/2), r, 0x5555 );
    }
    t2 = clock();
    dt_hw_circle = (t2 - t1) - dt_hw_circle;

    puts("\n"
         "Results\n"
         "-------\n");

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

