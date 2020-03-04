#ifndef BASE_GRAPHICS_H
#define BASE_GRAPHICS_H

#include <agui.h>

#define BEGIN_DRAW  struct draw_struct_t draw_struct;\
                    begin_draw(obj, &draw_struct)

#define END_DRAW    end_draw(obj, &draw_struct)

color_t hsl_to_rgb(int hue, int sat, int lum);

void split_rgb(color_t color, int *r, int *g, int *b);

extern void draw_gradient(struct draw_struct_t* draw_struct,
                          int x1, int y1, int x2, int y2,
                          color_t color1, color_t color2, bool vertical);
extern void draw_bi_gradient(struct draw_struct_t* draw_struct,
                            int x1, int y1, int x2, int y2,
                            color_t color11, color_t color12,
                            color_t color21, color_t color22);

#endif
