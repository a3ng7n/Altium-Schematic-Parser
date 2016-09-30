// 3D test - PCLS optimizable functions

#include "3dcube_chc.h"


// Next five functions are build in CHC, these are exact copies of the software versions
// These are copied to make it possible to switch between both modes

/****************************************************************
|
| CHC
|
\***************************************************************/

int16_t chc_plot_xx;
int16_t chc_plot_xy;
int16_t chc_plot_xz;
int16_t chc_plot_yx;
int16_t chc_plot_yy;
int16_t chc_plot_yz;
int16_t chc_plot_zx;
int16_t chc_plot_zy;
int16_t chc_plot_zz;
int16_t *chc_plot_zbuf;
uint16_t *chc_plot_vgabuf;


/*********************************************************************
|*
|*  Function    : chc_plot_init
|*
|*  Parameters  : xx..zz - matrix parameters
|*                vgabuf - pointer to non-displaying vga-buffer
|*                zbuf - pointer to z-buffer
|*
|*  Returns     : none
|*
|*  Description : Initialize the multiplier matrix and buffers, runs in hardware(CHC)
 */

void chc_plot_init(int16_t xx, int16_t xy, int16_t xz, int16_t yx, int16_t yy, int16_t yz, int16_t zx, int16_t zy, int16_t zz, uint16_t *vgabuf, int16_t *zbuf)
{
    chc_plot_xx = xx; chc_plot_xy = xy; chc_plot_xz = xz;
    chc_plot_yx = yx; chc_plot_yy = yy; chc_plot_yz = yz;
    chc_plot_zx = zx; chc_plot_zy = zy; chc_plot_zz = zz;

    chc_plot_zbuf = zbuf;
    chc_plot_vgabuf = vgabuf;
}


/*********************************************************************
|*
|*  Function    : chc_plot
|*
|*  Parameters  : none
|*
|*  Returns     : x,y,z - 3d-position
|*                color - color of 3d-position
|*
|*  Description : plot a 3d-position, runs in hardware(CHC)
 */

void chc_plot(int16_t x, int16_t y, int16_t z, uint16_t color)
{
    int16_t rx = (chc_plot_xx * x + chc_plot_xy * y + chc_plot_xz * z) >> FIXEDPOINTSCALE;
    int16_t ry = (chc_plot_yx * x + chc_plot_yy * y + chc_plot_yz * z) >> FIXEDPOINTSCALE;
    int16_t rz = (chc_plot_zx * x + chc_plot_zy * y + chc_plot_zz * z) >> FIXEDPOINTSCALE;

#ifdef ISOMETRIC
    // isometric: drop the z-axis and scale 1/2 to make sure pixels overlap
    // (very brute force, but easy to program for this demo)
    rx >>= 1;
    ry >>= 1;
#else
    // real perspective projection,
    // uses 1/2 worst case scaling to try to make pixels overlap but rounding errors
    // make some pixels 1 pos off for some rotations (looks like pizel-sized holes in cube)
    rx = (rx * (880 - rz)) >> 11;
    ry = (ry * (880 - rz)) >> 11;
#endif

    // shift 0,0 to middle of screen and calculate lineair pixelnr from topleft corner
    uint16_t pixelnr = (VGA_W / 2 + rx) + (VGA_H / 2 + ry) * VGA_W;
    if (rz < chc_plot_zbuf[pixelnr])
    {
        chc_plot_zbuf[pixelnr] = rz;
        chc_plot_vgabuf[pixelnr] = color;
    }
}


/*********************************************************************
|*
|*  Function    : chc_plot_cube
|*
|*  Parameters  : p1..p6 pionters to decoded bitmaps for each side
|*
|*  Returns     : none
|*
|*  Description : Plot the cube, runs in hardware(CHC)
 */

void chc_plot_cube(uint16_t *p1, uint16_t *p2, uint16_t *p3, uint16_t *p4, uint16_t *p5, uint16_t *p6)
{
    for (int16_t bmp_row = -BMPSIZE; bmp_row < BMPSIZE; bmp_row += 2)
    {
        for (int16_t bmp_column = -BMPSIZE; bmp_column < BMPSIZE; bmp_column += 2)
        {
            uint16_t color;

            // front, scaled up 2x
            if (p1)
            {
                color = *p1++;
                chc_plot(bmp_column, bmp_row, -BMPSIZE, color);
                chc_plot(bmp_column, bmp_row + 1, -BMPSIZE, color);
                chc_plot(bmp_column + 1, bmp_row, -BMPSIZE, color);
            }

            // back, scaled up 2x
            if (p2)
            {
                color = *p2++;
                chc_plot(-bmp_column, bmp_row, BMPSIZE, color);
                chc_plot(-bmp_column, bmp_row + 1, BMPSIZE, color);
                chc_plot(-bmp_column + 1, bmp_row, BMPSIZE, color);
            }

            // left, scaled up 2x
            if (p3)
            {
                color = *p3++;
                chc_plot(-BMPSIZE, bmp_column, bmp_row, color);
                chc_plot(-BMPSIZE, bmp_column, bmp_row + 1, color);
                chc_plot(-BMPSIZE, bmp_column + 1, bmp_row, color);
            }

            // right, scaled up 2x
            if (p4)
            {
                color = *p4++;
                chc_plot(BMPSIZE, -bmp_column, bmp_row, color);
                chc_plot(BMPSIZE, -bmp_column, bmp_row + 1, color);
                chc_plot(BMPSIZE, -bmp_column + 1, bmp_row, color);
            }

            // bottom, scaled up 2x
            if (p5)
            {
                color = *p5++;
                chc_plot(-bmp_column, -BMPSIZE, bmp_row, color);
                chc_plot(-bmp_column, -BMPSIZE, bmp_row + 1, color);
                chc_plot(-bmp_column + 1, -BMPSIZE, bmp_row, color);
            }

            // top, scaled up 2x
            if (p6)
            {
                color = *p6++;
                chc_plot(bmp_column, BMPSIZE, bmp_row, color);
                chc_plot(bmp_column, BMPSIZE, bmp_row + 1, color);
                chc_plot(bmp_column + 1, BMPSIZE, bmp_row, color);
            }
        }
    }
}


/*********************************************************************
|*
|*  Function    : chc_zbuf_clear
|*
|*  Parameters  : zbuf - pointer to zbuf
|*
|*  Returns     : none
|*
|*  Description : Set zbuf to initial value, runs in hardware(CHC)
 */

void chc_buf_clear(uint16_t *tft_buf, int16_t *zbuf)
{
    uint32_t *p = (uint32_t *)tft_buf;
    uint32_t *q = (uint32_t *)zbuf;

    for (int i = VGA_PIXELS / 2; i; --i)
    {
        *p++ = 0;
        *q++ = 0x7FFF7FFF;
    }
}

// Next five functions are build in software, these are exact copies of the chc versions
// These are copied to make it possible to switch between both modes

/****************************************************************
|
| SOFTWARE
|
\***************************************************************/

int16_t plot_xx;
int16_t plot_xy;
int16_t plot_xz;
int16_t plot_yx;
int16_t plot_yy;
int16_t plot_yz;
int16_t plot_zx;
int16_t plot_zy;
int16_t plot_zz;
int16_t *plot_zbuf;
uint16_t *plot_vgabuf;


/*********************************************************************
|*
|*  Function    : plot_init
|*
|*  Parameters  : xx..zz - matrix parameters
|*                vgabuf - pointer to non-displaying vga-buffer
|*                zbuf - pointer to z-buffer
|*
|*  Returns     : none
|*
|*  Description : Initialize the multiplier matrix and buffers, runs in software
 */

void plot_init(int16_t xx, int16_t xy, int16_t xz, int16_t yx, int16_t yy, int16_t yz, int16_t zx, int16_t zy, int16_t zz, uint16_t *vgabuf, int16_t *zbuf)
{
    plot_xx = xx; plot_xy = xy; plot_xz = xz;
    plot_yx = yx; plot_yy = yy; plot_yz = yz;
    plot_zx = zx; plot_zy = zy; plot_zz = zz;

    plot_zbuf = zbuf;
    plot_vgabuf = vgabuf;
}


/*********************************************************************
|*
|*  Function    : plot
|*
|*  Parameters  : none
|*
|*  Returns     : x,y,z - 3d-position
|*                color - color of 3d-position
|*
|*  Description : plot a 3d-position, runs in software
 */

void plot(int16_t x, int16_t y, int16_t z, uint16_t color)
{
    int16_t rx = (plot_xx * x + plot_xy * y + plot_xz * z) >> FIXEDPOINTSCALE;
    int16_t ry = (plot_yx * x + plot_yy * y + plot_yz * z) >> FIXEDPOINTSCALE;
    int16_t rz = (plot_zx * x + plot_zy * y + plot_zz * z) >> FIXEDPOINTSCALE;

#ifdef ISOMETRIC
    // isometric: drop the z-axis and scale 1/2 to make sure pixels overlap
    // (very brute force, but easy to program for this demo)
    rx >>= 1;
    ry >>= 1;
#else
    // real perspective projection,
    // uses 1/2 worst case scaling to try to make pixels overlap but rounding errors
    // make some pixels 1 pos off for some rotations (looks like pizel-sized holes in cube)
    rx = (rx * (880 - rz)) >> 11;
    ry = (ry * (880 - rz)) >> 11;
#endif

    // shift 0,0 to middle of screen and calculate lineair pixelnr from topleft corner
    uint16_t pixelnr = (VGA_W / 2 + rx) + (VGA_H / 2 + ry) * VGA_W;
    if (rz < plot_zbuf[pixelnr])
    {
        plot_zbuf[pixelnr] = rz;
        plot_vgabuf[pixelnr] = color;
    }
}


/*********************************************************************
|*
|*  Function    : plot_cube
|*
|*  Parameters  : p1..p6 pionters to decoded bitmaps for each side
|*
|*  Returns     : none
|*
|*  Description : Plot the cube, runs in software
 */

void plot_cube(uint16_t *p1, uint16_t *p2, uint16_t *p3, uint16_t *p4, uint16_t *p5, uint16_t *p6)
{
    for (int16_t bmp_row = -BMPSIZE; bmp_row < BMPSIZE; bmp_row += 2)
    {
        for (int16_t bmp_column = -BMPSIZE; bmp_column < BMPSIZE; bmp_column += 2)
        {
            uint16_t color;

            // front, scaled up 2x
            if (p1)
            {
                color = *p1++;
                plot(bmp_column, bmp_row, -BMPSIZE, color);
                plot(bmp_column, bmp_row + 1, -BMPSIZE, color);
                plot(bmp_column + 1, bmp_row, -BMPSIZE, color);
            }

            // back, scaled up 2x
            if (p2)
            {
                color = *p2++;
                plot(-bmp_column, bmp_row, BMPSIZE, color);
                plot(-bmp_column, bmp_row + 1, BMPSIZE, color);
                plot(-bmp_column + 1, bmp_row, BMPSIZE, color);
            }

            // left, scaled up 2x
            if (p3)
            {
                color = *p3++;
                plot(-BMPSIZE, bmp_column, bmp_row, color);
                plot(-BMPSIZE, bmp_column, bmp_row + 1, color);
                plot(-BMPSIZE, bmp_column + 1, bmp_row, color);
            }

            // right, scaled up 2x
            if (p4)
            {
                color = *p4++;
                plot(BMPSIZE, -bmp_column, bmp_row, color);
                plot(BMPSIZE, -bmp_column, bmp_row + 1, color);
                plot(BMPSIZE, -bmp_column + 1, bmp_row, color);
            }

            // bottom, scaled up 2x
            if (p5)
            {
                color = *p5++;
                plot(-bmp_column, -BMPSIZE, bmp_row, color);
                plot(-bmp_column, -BMPSIZE, bmp_row + 1, color);
                plot(-bmp_column + 1, -BMPSIZE, bmp_row, color);
            }

            // top, scaled up 2x
            if (p6)
            {
                color = *p6++;
                plot(bmp_column, BMPSIZE, bmp_row, color);
                plot(bmp_column, BMPSIZE, bmp_row + 1, color);
                plot(bmp_column + 1, BMPSIZE, bmp_row, color);
            }
        }
    }
}


/*********************************************************************
|*
|*  Function    : zbuf_clear
|*
|*  Parameters  : zbuf - pointer to zbuf
|*
|*  Returns     : none
|*
|*  Description : Set zbuf to initial value, runs in software
 */

void buf_clear(uint16_t *tft_buf, int16_t *zbuf)
{
    uint32_t *p = (uint32_t *)tft_buf;
    uint32_t *q = (uint32_t *)zbuf;

    for (int i = VGA_PIXELS / 2; i; --i)
    {
        *p++ = 0;
        *q++ = 0x7FFF7FFF;
    }
}



