#ifndef _AGUI_MAIN_H_
#define _AGUI_MAIN_H_

#include <agui.h>
#include "form1.H"

extern agui_t *agui;
extern form_t *main_form;

extern void agui_main(void);

#define UPDATE_FORM \
    form1.color           = hsl_to_rgb(HUE, 255, GET_LUM(LUM_FG_LIGHT));\
    form1.captionbarcolor = hsl_to_rgb(HUE, 255, GET_LUM(LUM_FG_DARK));\
    form1.caption.color   = hsl_to_rgb(HUE, 255, GET_LUM(LUM_TEXT));

#endif

