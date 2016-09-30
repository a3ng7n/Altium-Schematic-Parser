
#include <graphics.h>
#include <pthread.h>
#include "devices.h"

#include "The Dining Philosophers Problem.h"
#include "visualize.h"

#define BACK_COLOR BLACK
#define TEXT_COLOR YELLOW
#define TEXT_TYPE FS_BOLD

static const int graph_center_x[N_PHILOSOPHERS] = {  0, 70, 35,-35,-70};
static const int graph_center_y[N_PHILOSOPHERS] = { 66, 10,-42,-42, 10};
static const int graph_zoom    [N_PHILOSOPHERS] = { 80, 60, 48, 48, 60};

static graphics_t *graphics;
static canvas_t *canvas;
static char *prog_name_1 = "The Dining Philosophers";
static char *prog_name_2 = "Problem";
static int prog_name_size_1;
static int prog_name_size_2;
static int center_x;
static int center_y;

extern graphics_bitmap_t _lc_ub_plate_filled_bmp[];
extern graphics_bitmap_t _lc_ub_plate_empty_bmp[];

static const struct timespec delay_2s = {2, 0L};
static const struct timespec delay_200ms = {0, 200000000L};

void visualize_init(void)
{
   /* open graphics driver */
   graphics = graphics_open(GRAPHICS_1);

   /* get active canvas */
   canvas = graphics_get_visible_canvas(graphics);

   /* get the canvas center */
   center_x = canvas_get_width(canvas) / 2;
   center_y = canvas_get_height(canvas) / 2;

   /* get the size of some strings */
   prog_name_size_1 = graphics_get_stringwidth(canvas, prog_name_1, 0, TEXT_TYPE);
   prog_name_size_2 = graphics_get_stringwidth(canvas, prog_name_2, 0, TEXT_TYPE);
}

void visualize_show_startup_screen(void)
{
   int i;

   /* start with a clean canvas */
   graphics_fill_canvas(canvas, BACK_COLOR);

   /* display startup screen */
   graphics_draw_bitmap(canvas, _lc_ub_plate_filled_bmp, center_x-60, center_y-60, 120, 120, 50);
   graphics_draw_string(canvas, center_x - prog_name_size_1 / 2, center_y+30, prog_name_1, 0, TEXT_COLOR, TEXT_TYPE);
   graphics_draw_string(canvas, center_x - prog_name_size_2 / 2, center_y+46, prog_name_2, 0, TEXT_COLOR, TEXT_TYPE);
   nanosleep(&delay_2s, NULL);

   /* display table with diner plates */
   graphics_fill_canvas(canvas, BACK_COLOR);
   graphics_draw_string(canvas, center_x - prog_name_size_1 / 2, 30, prog_name_1, 0, TEXT_COLOR, TEXT_TYPE);
   graphics_draw_string(canvas, center_x - prog_name_size_2 / 2, 46, prog_name_2, 0, TEXT_COLOR, TEXT_TYPE);

   for (i =0; i < N_PHILOSOPHERS; i++)
   {
      visualize_update_plate(i, false);
      nanosleep(&delay_200ms, NULL);
   }

   nanosleep(&delay_2s, NULL);
}

void visualize_show_end_screen(void)
{
   nanosleep(&delay_2s, NULL);
}

void visualize_update_text(int i, char *s)
{
   static int width[N_PHILOSOPHERS];

   if (width[i])
   {
      /* cleanup previous text */
      graphics_fill_rect(canvas,
                         center_x+graph_center_x[i]-(width[i]/2),
                         center_y+graph_center_y[i]+8, width[i],
                         graphics_get_fontheight(canvas, 0), BACK_COLOR);
   }
   /* show new text */
   width[i] = graphics_get_stringwidth(canvas, s, 0, TEXT_TYPE);
   graphics_draw_string(canvas,
                        center_x+graph_center_x[i]-(width[i]/2),
                        center_y+graph_center_y[i]+8,
                        s, 0, TEXT_COLOR, TEXT_TYPE);
}

void visualize_update_plate(int i, bool filled)
{
   graphics_draw_bitmap(canvas,
                        filled ? _lc_ub_plate_filled_bmp : _lc_ub_plate_empty_bmp,
                        center_x+graph_center_x[i]-graph_zoom[i]/2,
                        center_y+graph_center_y[i]-graph_zoom[i]/2,
                        graph_zoom[i], graph_zoom[i], graph_zoom[i]/240);
}

void visualize_close(void)
{
}


