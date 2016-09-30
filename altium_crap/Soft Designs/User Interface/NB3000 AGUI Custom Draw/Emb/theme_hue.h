#ifndef _THEME_HUE_H_
#define _THEME_HUE_H_

#include "base_graphics.h"

extern int HUE; //0..255
extern int LUM; //1..100

#define LUM_TEXT          64
#define LUM_INVERTEDTEXT 232
#define LUM_FRAME         80
#define LUM_FG_LIGHT     240
#define LUM_FG_DARK      112
#define LUM_BG_LIGHT     255
#define LUM_BG_DARK      232
#define LUM_INNER_LIGHT  255
#define LUM_INNER_DARK   192

extern int GET_LUM(int lum);

//HUE-LUM theme controls drawing
extern void bevel_draw_hue       (struct obj_t* obj);
extern void bitmap_draw_hue      (struct obj_t* obj);
extern void bitmapbutton_draw_hue(struct obj_t* obj);
extern void button_draw_hue      (struct obj_t* obj);
extern void checkbox_draw_hue    (struct obj_t* obj);
extern void form_draw_hue        (struct obj_t* obj);
extern void icon_listbox_draw_hue(struct obj_t* obj);
extern void label_draw_hue       (struct obj_t* obj);
extern void listbox_draw_hue     (struct obj_t* obj);
extern void progressbar_draw_hue (struct obj_t* obj);
extern void radiogroup_draw_hue  (struct obj_t* obj);
extern void scrollbar_draw_hue   (struct obj_t* obj);
extern void slider_draw_hue      (struct obj_t* obj);
extern void textarea_draw_hue    (struct obj_t* obj);
extern void textbox_draw_hue     (struct obj_t* obj);

//AGUI internals
extern void agui_draw_string              (agui_t *agui, int left, int top, int right, int bottom, const string_t *string, bool enabled);
extern void agui_draw_string_clipped      (agui_t *agui, int left, int top, int right, int bottom, const string_t *string, bool enabled, bool clip_mark);
extern int  agui_get_breaking_string_count(agui_t *agui,                                           const string_t *string, int width);
extern int  agui_align_horizontal_count   (agui_t *agui, int left,          int right,             const string_t *string, unsigned int count);

#endif
