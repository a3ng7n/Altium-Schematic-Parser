#ifndef _FORM1_H_
#define _FORM1_H_

#include <agui.h>

extern form_t form1;
extern bevel_t areaframe;
extern bevel_t btn_controls1;
extern button_t button1;
extern bitmapbutton_t bitmapbutton1;
extern checkbox_t checkbox1;
extern bevel_t btn_controls2;
extern radiogroup_t radiogroup1;
extern progressbar_t progressbar1;
extern scrollbar_t scrollbar1;
extern slider_t slider1;
extern bevel_t bevel1;
extern bitmap_t bitmap1;
extern label_t label1;
extern textbox_t textbox1;
extern progressbar_t progressbar2;
extern scrollbar_t scrollbar2;
extern slider_t slider2;
extern textarea_t textarea1;
extern icon_listbox_t listbox2;
extern listbox_t listbox1;
extern slider_t hueslider;
extern slider_t lumslider;
extern label_t lbl_lum;
extern label_t lbl_hue;

extern void areaframedraw(struct obj_t* obj);
extern void btn_controls1draw(struct obj_t* obj);
extern void btn_controls2draw(struct obj_t* obj);
extern void lumsliderdragged(struct obj_t* obj, int x, int y, _action_button_t button);
extern void lumsliderdraw(struct obj_t* obj);
extern void huesliderdragged(struct obj_t* obj, int x, int y, _action_button_t button);
extern void huesliderdraw(struct obj_t* obj);
extern void labeldraw(struct obj_t* obj);
extern void bitmapbutton1draw(struct obj_t* obj);
extern void bitmap1draw(struct obj_t* obj);
extern void button1draw(struct obj_t* obj);
extern void checkbox1draw(struct obj_t* obj);
extern void textbox1draw(struct obj_t* obj);
extern void bevel1draw(struct obj_t* obj);
extern void progressbardraw(struct obj_t* obj);
extern void scrollbardraw(struct obj_t* obj);
extern void sliderdraw(struct obj_t* obj);
extern void radiogroup1draw(struct obj_t* obj);
extern void textarea1draw(struct obj_t* obj);
extern void listbox1draw(struct obj_t* obj);
extern void listbox2draw(struct obj_t* obj);
extern void btn_controls1clicked(struct obj_t* obj, int x, int y, _action_button_t button);
extern void btn_controls2clicked(struct obj_t* obj, int x, int y, _action_button_t button);
extern void form1draw(struct obj_t* obj);

#endif

