#include <stdio.h>
#include <agui.h>
#include "agui_main.H"
#include "base_graphics.h"
#include "theme_hue.h"

int CUR_TAB = 1;

void show_controls(int tag)
{
    for (int i = 0; i < form1.n_children; i++)
    {
        obj_t* ctrl = form1.children[i];
        if (ctrl->tag != 0)
            obj_set_visible(ctrl, ctrl->tag == tag);
    }
}

void switch_tab(int tab)
{
    if (CUR_TAB == tab)
        return;
    CUR_TAB = tab;
    show_controls(CUR_TAB);
    obj_invalidate(AGUI_HANDLE(form1));
}

void btn_controls1clicked(struct obj_t* obj, int x, int y, _action_button_t button)
{
    switch_tab(1);
}

void btn_controls2clicked(struct obj_t* obj, int x, int y, _action_button_t button)
{
    switch_tab(2);
}

void areaframedraw(struct obj_t* obj)
{
    BEGIN_DRAW;

    color_t frame_color = hsl_to_rgb(HUE, 255, GET_LUM(LUM_FRAME));
    color_t back_color = hsl_to_rgb(HUE, 255, GET_LUM(LUM_FG_LIGHT));

    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1, draw_struct.x2, draw_struct.y1, frame_color);
    graphics_draw_line(draw_struct.canvas, draw_struct.x2, draw_struct.y1, draw_struct.x2, draw_struct.y2, frame_color);
    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1, draw_struct.x1, draw_struct.y2, frame_color);
    if (CUR_TAB == 1)
        graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1 + 1,
                           draw_struct.x1, draw_struct.y1 + btn_controls1.obj.height - 1, back_color);
    else //CUR_TAB == 2
        graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1 + btn_controls1.obj.height,
                           draw_struct.x1, draw_struct.y1 + btn_controls1.obj.height + btn_controls2.obj.height - 2, back_color);

    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y2, draw_struct.x2, draw_struct.y2, frame_color);

    graphics_fill_rect(draw_struct.canvas, draw_struct.x1 + 1, draw_struct.y1 + 1,
                       draw_struct.x2 - draw_struct.x1 - 1, draw_struct.y2 - draw_struct.y1 - 1, back_color);

    END_DRAW;
}

void draw_tab(struct obj_t* obj, int tag, char* caption)
{
    BEGIN_DRAW;

    bool active = CUR_TAB == tag;
    bool need_top = !(active && (CUR_TAB == 2));
    bool need_bottom = !(active && (CUR_TAB == 1));
    color_t frame_color = hsl_to_rgb(HUE, 255, GET_LUM(LUM_FRAME));
    if (need_top)
        graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1, draw_struct.x2, draw_struct.y1, frame_color);
    graphics_draw_line    (draw_struct.canvas, draw_struct.x1, draw_struct.y1, draw_struct.x1, draw_struct.y2, frame_color);
    if (need_bottom)
        graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y2, draw_struct.x2, draw_struct.y2, frame_color);

    color_t back_color = hsl_to_rgb(HUE, 255, GET_LUM(active ? LUM_FG_LIGHT : LUM_FG_DARK));
    int y1 = draw_struct.y1;
    if (need_top)
        y1++;
    int y2 = draw_struct.y2;
    if (need_bottom)
        y2--;
    if (active)
        graphics_fill_rect(draw_struct.canvas, draw_struct.x1 + 1, y1,
                           draw_struct.x2 - draw_struct.x1, y2 - draw_struct.y1, back_color);
    else
        draw_gradient(&draw_struct, draw_struct.x1 + 1, y1, draw_struct.x2, y2,
                      hsl_to_rgb(HUE, 255, GET_LUM(LUM_FG_DARK)),
                      hsl_to_rgb(HUE, 255, GET_LUM(LUM_FG_LIGHT)), false);

    string_t text;
    text.x = 0;
    text.y = 0;
    text.text = caption;
    text.font = ((form_t*)obj->parent)->caption.font;
    text.color = hsl_to_rgb(HUE, 255, GET_LUM(LUM_TEXT));
    text.fontstyle = FS_NONE;
    text.align = ALIGN_CENTRE;
    agui_draw_string(draw_struct.agui, draw_struct.x1, draw_struct.y1,
                     draw_struct.x2, draw_struct.y2, &text, true);

    END_DRAW;
}

void btn_controls1draw(struct obj_t* obj)
{
    draw_tab(obj, 1, "controls 1");
}

void btn_controls2draw(struct obj_t* obj)
{
    draw_tab(obj, 2, "controls 2");
}

void lumsliderdragged(struct obj_t* obj, int x, int y, _action_button_t button)
{
    LUM = ((slider_t*)obj)->position;
    UPDATE_FORM;
    obj_invalidate(AGUI_HANDLE(form1));
}

void lumsliderdraw(struct obj_t* obj)
{
    BEGIN_DRAW;

    for (int i = draw_struct.y2; i>= draw_struct.y1; i--)
    {
        int lum = 255 * (draw_struct.y2 - i) / (draw_struct.y2 - draw_struct.y1);
        color_t line_color = hsl_to_rgb(0, 0, lum);
        graphics_draw_line(draw_struct.canvas, draw_struct.x1, i, draw_struct.x2, i, line_color);
    }

    int pos = draw_struct.y2 - obj->height * (((slider_t*)obj)->position - ((slider_t*)obj)->low) / (((slider_t*)obj)->high - ((slider_t*)obj)->low);
    graphics_fill_rect(draw_struct.canvas, draw_struct.x1, pos - 1, obj->width, 3, RED);

    END_DRAW;
}

void huesliderdragged(struct obj_t* obj, int x, int y, _action_button_t button)
{
    HUE = ((slider_t*)obj)->position;
    UPDATE_FORM;
    obj_invalidate(AGUI_HANDLE(form1));
}

void huesliderdraw(struct obj_t* obj)
{
    BEGIN_DRAW;

    for (int i = draw_struct.y2; i>= draw_struct.y1; i--)
    {
        int hue = 255 * (draw_struct.y2 - i) / (draw_struct.y2 - draw_struct.y1);
        color_t line_color = hsl_to_rgb(hue, 255, 128);
        graphics_draw_line(draw_struct.canvas, draw_struct.x1, i, draw_struct.x2, i, line_color);
    }

    int pos = draw_struct.y2 - obj->height * ((slider_t*)obj)->position / 255;
    graphics_fill_rect(draw_struct.canvas, draw_struct.x1, pos - 1, obj->width, 3, BLACK);

    END_DRAW;
}

void labeldraw(struct obj_t* obj)
{
    label_draw_hue(obj);
}

void bitmapbutton1draw(struct obj_t* obj)
{
    bitmapbutton_draw_hue(obj);
}

void bitmap1draw(struct obj_t* obj)
{
    bitmap_draw_hue(obj);
}

void button1draw(struct obj_t* obj)
{
    button_draw_hue(obj);
}

void checkbox1draw(struct obj_t* obj)
{
    checkbox_draw_hue(obj);
}

void textbox1draw(struct obj_t* obj)
{
    textbox_draw_hue(obj);
}

void bevel1draw(struct obj_t* obj)
{
    bevel_draw_hue(obj);
}

void progressbardraw(struct obj_t* obj)
{
    progressbar_draw_hue(obj);
}

void scrollbardraw(struct obj_t* obj)
{
    scrollbar_draw_hue(obj);
}

void sliderdraw(struct obj_t* obj)
{
    slider_draw_hue(obj);
}

void radiogroup1draw(struct obj_t* obj)
{
    radiogroup_draw_hue(obj);
}

void textarea1draw(struct obj_t* obj)
{
    textarea_draw_hue(obj);
}

void listbox1draw(struct obj_t* obj)
{
    listbox_draw_hue(obj);
}

void listbox2draw(struct obj_t* obj)
{
    icon_listbox_draw_hue(obj);
}

void form1draw(struct obj_t* obj)
{
    form_draw_hue(obj);
}
