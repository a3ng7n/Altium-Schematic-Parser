#include <stdio.h>

#include <graphics.h>
#include <touchscreen.h>
#include <pointer.h>
#include "generic_devices.h"
#include "devices.h"

#define WIDTH 320
#define HEIGHT 240

// Function prototypes
static void draw_mark(int x, int y, int width, int height, void *vp);

char *cal1 = "Touch screen at marker";
char *cal2 = "Calibration done";

// Pointers for LCD, Touch panel, and graphics drivers
graphics_t * display;
canvas_t * canvas;
touchscreen_t * tft_touch;
touchscreen_data_t * position;
touchscreen_callback_t callback;
pointer_t * ptr;
pointer_state_t * pointer_state;
int dx, dy;

void main (void)
{
    // Open instances of drivers for LCD, Touch and Graphics
    tft_touch = touchscreen_open(TOUCHSCREEN_1);
    ptr = pointer_open(POINTER_1);
    display = graphics_open(GRAPHICS_1);
    canvas = graphics_get_visible_canvas(display);
    dx = dy = 0;

    // Tell Touch driver which function to call for display during calibration
    touchscreen_set_callback(tft_touch, draw_mark, canvas);
    while(!touchscreen_calibrate(tft_touch, WIDTH, HEIGHT));
    graphics_fill_canvas(canvas, 0x080008);
    while(1)
    {
        if (pointer_update(ptr, pointer_state))
        {
            // Send point to Terminal in Devices View.
            printf( "Touched screen at point (%d,%d)\n",
                    pointer_state->x,
                    pointer_state->y );
            graphics_draw_line(canvas, dx, dy, pointer_state->x,
                    pointer_state->y, 0xFF0000);
            dx = pointer_state->x;
            dy = pointer_state->y;
            graphics_set_visible_canvas(display, canvas);
        }
    }
}

// Calibration call-back function - used to display a cross-hair at
// Calibration points on TFT panel.
static void draw_mark(int x, int y, int width, int height, void *vp)
{
     graphics_draw_circle(canvas, x, y, 10, 0xff00ff);
     graphics_draw_line(canvas, x - 15, y, x + 15, y, 0x00ffff);
     graphics_draw_line(canvas, x, y - 15, x, y + 15, 0x00ffff);
     graphics_draw_string(canvas, 50, 50, cal1, NULL, 0xffffff, 0);
     graphics_set_visible_canvas(display, canvas);
}
