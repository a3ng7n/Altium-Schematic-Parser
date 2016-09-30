#include <stdio.h>
#include <graphics.h>
#include "devices.h"

graphics_t *graphics;
canvas_t *canvas;

char str[100];

 int main( void )
{
    int y;

    /* open graphics driver */
    graphics = graphics_open(GRAPHICS_1);

    /* get active canvas */
    canvas = graphics_get_visible_canvas(graphics);

    /* start with a white canvas */
    graphics_fill_canvas(canvas, WHITE);

    for (int i = 0; i < 4; i++)
    {
        /* set rotation in stpes of 90 degrees */
        graphics_set_rotation(graphics, i * 90);

        /* format string, string includes a degree-sign! */
        sprintf(str, "Rotation: %d°", graphics_get_rotation(graphics));

        /* draw string at position (10,0), font must include degree-sign, otherwise degree-sign is replaced with a space */
        graphics_draw_string(canvas, 10, 0, str, &bitstreamverasans10, RED, FS_NONE);

        /* get the fontheight to locate line */
        y = graphics_get_fontheight(canvas, &bitstreamverasans10);

        /* draw line, from (10,y) to (125,y), color blue */
        graphics_draw_line(canvas, 10, y, 125, y, BLUE);

        /* fill rectangle, top-left corner at (20,25), width 100, height 150, color red */
        graphics_fill_rect(canvas, 10, 35, 116, 51, FIREBRICK);

        /* draw a filled circle at location (40,60), r=25, color GREEN */
        graphics_fill_circle(canvas, 40, 60, 25, SILVER);

        /* draw a circle at location (40,60), r=15, color WHITE */
        graphics_draw_circle(canvas, 40, 60, 15, GREEN);

        /* draw a pixel at location (40,60), color BLACK */
        graphics_draw_pixel(canvas, 40, 60, BLACK);

        /* draw a sector at location (95,60) (centre-point of corresponding circle), r=25, start at 0 degrees (3 o'clock) sector angle 90 degrees ccw */
        graphics_fill_sector(canvas, 95, 60, 25, 0, 90, BLUE);

        /* draw a segment at location (95,60) (centre-point of corresponding circle), r=25, start at 270 degrees (6 o'clock) sector angle 90 degrees ccw */
        graphics_fill_segment(canvas, 95, 60, 25, 270, 90, LIME);

        /* draw a segment at location (95,60) (centre-point of corresponding circle), r=25, start at 270 degrees (6 o'clock) sector angle 90 degrees cw */
        graphics_fill_segment(canvas, 95, 60, 25, 270, -90, CYAN);

        /* draw a segment at location (95,60) (centre-point of corresponding circle), r=25, start at 90 degrees (12 o'clock) sector angle 90 degrees ccw */
        graphics_fill_segment(canvas, 95, 60, 25, 90, 90, TEAL);

        /* draw an arc at location (95,60) (centre-point of corresponding circle), r=15, start at 90 degrees (12 o'clock) sector angle 90 degrees cw */
        graphics_draw_arc(canvas, 95, 60, 15, 90, -90, MAGENTA);

        /* draw one pixel at (95.60), color WHITE */
        graphics_draw_pixel(canvas, 95, 60, WHITE);

        /* fill triangle at position (80,55)  (100,75)  (85,70), color YELLOW */
        graphics_fill_triangle(canvas, 80, 55, 100, 75, 85, 70, YELLOW);
    }

    return 0;
}
