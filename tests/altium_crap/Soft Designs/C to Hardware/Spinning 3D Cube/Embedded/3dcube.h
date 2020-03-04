// 3D.H - some general settings

#ifndef _3D_H_
#define _3D_H_

#include <stdint.h>

// scaling for fixed point arithmetic
#define FIXEDPOINTSCALE  14

// number of divisions in full circle for gonio
#define ROTATIONSTEPS 64

// side of square bitmaps
#define BMPSIZE 100

#define PI 3.14159265359

#define VGA_W 240
#define VGA_H 320
#define VGA_PIXELS (VGA_W * VGA_H)

#define rgb(r, g, b) ((int16_t) ((((uint16_t) r) << 11) | (((uint16_t) g) << 6) | ((uint16_t) b)))

typedef struct
{
    int16_t xx, xy, xz;
    int16_t yx, yy, yz;
    int16_t zx, zy, zz;
} matrix_t;


#endif
