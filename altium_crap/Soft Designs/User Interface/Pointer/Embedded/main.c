#include <stdio.h>
#include <graphics.h>
#include <touchscreen.h>
#include <pointer.h>
#include "devices.h"

graphics_t *graphics;
canvas_t *canvas;
touchscreen_t *touchscreen;
pointer_t *pointer;
pointer_state_t state;

char *str1 = "Write \"Hello World\" with";
char *str2 = "the stylus or the mouse";

char *cal1 = "Touch screen at marker";
char *cal2 = "Calibration done";


/*
 * This routine is called from the touchscreen_calibrate routine.
 * It writes instructions on the screen to tell the user what to do
 *
 * x = x-position in touchscreen coordinates
 * y = y-position in touchscreen coordinates
 * width = touchscreen width in touchscreen coordinates, 0 if calibration succeeded
 * height = touchscreen height in touchscreen coordinates, 0 if calibration succeeded
 * vp = void pointer, not used in this case
 */
static void draw_mark(int x, int y, int width, int height, void *vp)
{
    char *str;

    int w, h;
    color_t fg_color, bg_color;

    w = graphics_get_width(graphics);
    h = graphics_get_height(graphics);
    fg_color = WHITE;
    bg_color = BLACK;

    graphics_fill_canvas(canvas, bg_color);

    if (width && height)
    {

        x = x * w / width;
        y = y * h / height;

        graphics_draw_circle(canvas, x, y, 10, fg_color);
        graphics_draw_line(canvas, x - 15, y, x + 15, y, fg_color);
        graphics_draw_line(canvas, x, y - 15, x, y + 15, fg_color);
        str = cal1;
    }
    else
    {
        str = cal2;
    }
    x = (w - graphics_get_stringwidth(canvas, str, &bitstreamverasans10, FS_NONE)) / 2;
    y = (h - graphics_get_fontheight(canvas, &bitstreamverasans10)) / 2;

    graphics_draw_string(canvas, x, y, str, &bitstreamverasans10, WHITE, FS_NONE);
}


int main( void )
{
    /* open pointer, touchscreen and graphics */
    pointer = pointer_open(POINTER_0);
    /* pointer_open opens underlying services and drivers, including mouse and touchscreen.
       We have to call the touchscreen_open and graphics_open because we need the pointers */
    touchscreen = touchscreen_open(TOUCHSCREEN_0);
    graphics = graphics_open(GRAPHICS_0);
    canvas = graphics_get_visible_canvas(graphics);

    /* set which callback function is called from the calibrate routine */
    touchscreen_set_callback(touchscreen, draw_mark, graphics);

    /* calibrate, repeat until calibrate is ready. Funtion returns true if ready */
    while (!touchscreen_calibrate(touchscreen, 240, 320));

    /* set both graphics and touchscreen to the same rotation */
    touchscreen_set_rotation(touchscreen, 0);
    graphics_set_rotation(graphics, 0);

    /* clear screen */
    graphics_fill_canvas(canvas, BLUE);

    /* draw instructions */
    graphics_draw_string(canvas, 10, 10, str1, &bitstreamverasans10, WHITE, FS_NONE);
    graphics_draw_string(canvas, 10, 10 + graphics_get_fontheight(canvas, &bitstreamverasans10), str2, &bitstreamverasans10, WHITE, FS_NONE);

    while (1)
    {
        /* get pointer-position, state contains new position and button state */
        if ( pointer_update(pointer, &state))
        {
            /* Left button pressed? */
            if (state.buttons == BUTTON_LEFT)
            {
                /* draw pixel */
                graphics_draw_pixel(canvas, state.x, state.y, WHITE);
            }
        }
    }
}

