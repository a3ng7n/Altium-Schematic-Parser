// 3D test - RTL optimizable functions

#ifndef _3DCUBE_CHC_H_
#define _3DCUBE_CHC_H_

#include <stdint.h>

#include "3dcube.h"

// if defined use ISOMETRIC perspective
//#define ISOMETRIC

void plot_init(int16_t xx, int16_t xy, int16_t xz, int16_t yx, int16_t yy, int16_t yz, int16_t zx, int16_t zy, int16_t zz, uint16_t *vgabuf, int16_t *zbuf);
void plot_cube(uint16_t *p1, uint16_t *p2, uint16_t *p3, uint16_t *p4, uint16_t *p5, uint16_t *p6);
void buf_clear(uint16_t *tft_buf, int16_t *zbuf);

void chc_plot_init(int16_t xx, int16_t xy, int16_t xz, int16_t yx, int16_t yy, int16_t yz, int16_t zx, int16_t zy, int16_t zz, uint16_t *vgabuf, int16_t *zbuf);
void chc_plot_cube(uint16_t *p1, uint16_t *p2, uint16_t *p3, uint16_t *p4, uint16_t *p5, uint16_t *p6);
void chc_buf_clear(uint16_t *tft_buf, int16_t *zbuf);

#endif
