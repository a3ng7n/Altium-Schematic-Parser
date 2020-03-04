#include <stdlib.h>
#include <stdint.h>
#include "testfuncs.h"

static inline void plot( uint16_t * vgamem, uint16_t x, uint16_t y, uint16_t color )
{
    if ( (x < 240) && (y < 320) )
    {
        vgamem[x + y * 240] = color;
    }
}

/*********************************************************************
|*
|*  Function    : hw_plot
|*
|*  Parameters  : vgamem = pointer to VGA memory
|*                x, y = position to be plotted
|*                color = color to be used for pixel
|*
|*  Returns     : None
|*
|*  Description : Plot a pixel in the VGA memory (to be implemented in hardware)
 */

void hw_plot( uint16_t * vgamem, uint16_t x, uint16_t y, uint16_t color )
{
    plot( vgamem, x, y, color );
}

/*********************************************************************
|*
|*  Function    : sw_plot
|*
|*  Parameters  : vgamem = pointer to VGA memory
|*                x, y = position to be plotted
|*                color = color to be used for pixel
|*
|*  Returns     : None
|*
|*  Description : Plot a pixel in the VGA memory (to be implemented in software)
 */

void sw_plot( uint16_t * vgamem, uint16_t x, uint16_t y, uint16_t color )
{
    plot( vgamem, x, y, color );
}

/*********************************************************************
|*
|*  Function    : hw_line
|*
|*  Parameters  : vgamem = pointer to VGA memory
|*                x0, y0 = startposition of line to be plotted
|*                x1, y1 = endposition of line to be plotted
|*                color = color to be used for pixel
|*
|*  Returns     : None
|*
|*  Description : Draw a line in the VGA memory (to be implemented in hardware)
 */

void hw_line( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1, uint16_t color )
{
    int16_t dx = (x0 > x1) ? x0 - x1 : x1 - x0;
    int16_t dy = (y0 > y1) ? y0 - y1 : y1 - y0;
    int16_t error;
    int16_t ystep;
    int16_t y;
    int16_t x;
    _Bool steep = dx < dy;

    if ( steep )
    {
        uint16_t tmp;
        tmp = x0; x0 = y0; y0 = tmp;
        tmp = x1; x1 = y1; y1 = tmp;
    }
    if ( x0 > x1 )
    {
        uint16_t tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    dx = x1 - x0;
    dy = (y1 > y0) ? y1 - y0 : y0 - y1;
    error = -dx / 2;
    y = y0;
    ystep = (y0 < y1) ? 1 : -1;
    for ( x = x0; x <= x1; x++ )
    {
        if ( steep )
        {
            plot( vgamem, y, x, color );
        }
        else
        {
            plot( vgamem, x, y, color );
        }
        error += dy;
        if ( error > 0 )
        {
            y += ystep;
            error -= dx;
        }
    }
}

/*********************************************************************
|*
|*  Function    : sw_line
|*
|*  Parameters  : vgamem = pointer to VGA memory
|*                x0, y0 = startposition of line to be plotted
|*                x1, y1 = endposition of line to be plotted
|*                color = color to be used for pixel
|*
|*  Returns     : None
|*
|*  Description : Draw a line in the VGA memory (to be implemented in software)
 */

void sw_line( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1, uint16_t color )
{
    int16_t dx = (x0 > x1) ? x0 - x1 : x1 - x0;
    int16_t dy = (y0 > y1) ? y0 - y1 : y1 - y0;
    int16_t error;
    int16_t ystep;
    int16_t y;
    int16_t x;
    _Bool steep = dx < dy;

    if ( steep )
    {
        uint16_t tmp;
        tmp = x0; x0 = y0; y0 = tmp;
        tmp = x1; x1 = y1; y1 = tmp;
    }
    if ( x0 > x1 )
    {
        uint16_t tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    dx = x1 - x0;
    dy = (y1 > y0) ? y1 - y0 : y0 - y1;
    error = -dx / 2;
    y = y0;
    ystep = (y0 < y1) ? 1 : -1;
    for ( x = x0; x <= x1; x++ )
    {
        if ( steep )
        {
            plot( vgamem, y, x, color );
        }
        else
        {
            plot( vgamem, x, y, color );
        }
        error += dy;
        if ( error > 0 )
        {
            y += ystep;
            error -= dx;
        }
    }
}

/*********************************************************************
|*
|*  Function    : hw_circle
|*
|*  Parameters  : vgamem = pointer to VGA memory
|*                x0,y0 = coordinates of centre
|*                radius = radius of circle
|*                color = color to be used for drawing
|*
|*  Returns     : None
|*
|*  Description : Draw a circle in the VGA memory (to be implemented in hardware)
 */

void hw_circle( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t radius, uint16_t color )
{
    int eps, x, y;

    x = 0;
    y = radius;
    eps = 3 - (radius << 1);

    while( x <= y )
    {
        plot( vgamem, (uint16_t)(x0 + x), (uint16_t)(y0 + y), color );
        plot( vgamem, (uint16_t)(x0 - x), (uint16_t)(y0 + y), color );
        plot( vgamem, (uint16_t)(x0 + x), (uint16_t)(y0 - y), color );
        plot( vgamem, (uint16_t)(x0 - x), (uint16_t)(y0 - y), color );

        plot( vgamem, (uint16_t)(x0 + y), (uint16_t)(y0 + x), color );
        plot( vgamem, (uint16_t)(x0 - y), (uint16_t)(y0 + x), color );
        plot( vgamem, (uint16_t)(x0 + y), (uint16_t)(y0 - x), color );
        plot( vgamem, (uint16_t)(x0 - y), (uint16_t)(y0 - x), color );

        if ( eps < 0 )
        {
            eps += (x << 2) + 6;
        }
        else
        {
            eps += ((x - y) << 2) + 10;
            y--;
        }
        x++;
    }
}

/*********************************************************************
|*
|*  Function    : sw_circle
|*
|*  Parameters  : vgamem = pointer to VGA memory
|*                x,y = coordinates of centre
|*                radius = radius of circle
|*                color = color to be used for drawing
|*
|*  Returns     : None
|*
|*  Description : Draw a circle in the VGA memory (to be implemented in software)
 */

void sw_circle( uint16_t * vgamem, uint16_t x0, uint16_t y0, uint16_t radius, uint16_t color )
{
    int eps, x, y;

    x = 0;
    y = radius;
    eps = 3 - (radius << 1);

    while( x <= y )
    {
        plot( vgamem, (uint16_t)(x0 + x), (uint16_t)(y0 + y), color );
        plot( vgamem, (uint16_t)(x0 - x), (uint16_t)(y0 + y), color );
        plot( vgamem, (uint16_t)(x0 + x), (uint16_t)(y0 - y), color );
        plot( vgamem, (uint16_t)(x0 - x), (uint16_t)(y0 - y), color );

        plot( vgamem, (uint16_t)(x0 + y), (uint16_t)(y0 + x), color );
        plot( vgamem, (uint16_t)(x0 - y), (uint16_t)(y0 + x), color );
        plot( vgamem, (uint16_t)(x0 + y), (uint16_t)(y0 - x), color );
        plot( vgamem, (uint16_t)(x0 - y), (uint16_t)(y0 - x), color );

        if ( eps < 0 )
        {
            eps += (x << 2) + 6;
        }
        else
        {
            eps += ((x - y) << 2) + 10;
            y--;
        }
        x++;
    }
}



