#include <agui.h>
#include <math.h>
#include "theme_hue.h"

int HUE = 128;
int LUM = 50;

int GET_LUM(int lum)
{
    if (LUM > 50)
        return lum + (255 - lum) * LUM / 50 - (255 - lum);
    else if (LUM < 50)
        return lum * LUM / 50;
    else //LUM = 50
        return lum;
}

#define DECLARATIONS \
    bool enabled = obj->enabled;\
    int saturation = enabled ? 255 : 0

#define DRAW_FRAME \
    color_t frame_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FRAME));\
    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1, draw_struct.x2, draw_struct.y1, frame_color);\
    graphics_draw_line(draw_struct.canvas, draw_struct.x2, draw_struct.y1, draw_struct.x2, draw_struct.y2, frame_color);\
    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1, draw_struct.x1, draw_struct.y2, frame_color);\
    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y2, draw_struct.x2, draw_struct.y2, frame_color)

#define DRAW_BACKGROUND \
    color_t bcolor1 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_LIGHT));\
    color_t bcolor2 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_DARK));\
    draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1,\
                  draw_struct.x2 - 1, draw_struct.y2 - 1, bcolor1, bcolor2, true)

#define BITMAP_SIZE 16

typedef struct
{
    agui_t *agui;
    int x;
    int y;
    int w;
    int h;
    bool enabled;
    unsigned int range;
    unsigned int window;
    unsigned int position;
} _scrollbar_draw_t;

void _scrollbar_draw_hue(draw_struct_t* draw_struct, _scrollbar_draw_t* scrollbar);

void bevel_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;
    END_DRAW;
}

void bitmap_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;

    //TODO: transparent, transparentcolor
    graphics_draw_bitmap_modified(draw_struct.canvas, ((bitmap_t*)obj)->bm,
        draw_struct.x1, draw_struct.y1, obj->width, obj->height, 0, true, RGB(255, 0, 255), !enabled);

    END_DRAW;
}

void bitmapbutton_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;

    //gradient
    color_t light_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t dark_color  = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
    if (obj->pressed)
    {
        color_t tmp = light_color;
        light_color = dark_color;
        dark_color = tmp;
    }
    draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1,
                  draw_struct.x2 - 1, draw_struct.y2 - 1, light_color, dark_color, true);

    //bitmap
    int z = ((bitmapbutton_t*)obj)->fit ? 0 : 100;
    int shift = obj->pressed ? 2 : 1;
    shift += (draw_struct.y2 - draw_struct.y1 - BITMAP_SIZE) / 2;
    //TODO: transparent, transparentcolor
    graphics_draw_bitmap_modified(draw_struct.canvas, ((bitmapbutton_t*)obj)->bm,
        draw_struct.x1 + shift, draw_struct.y1 + shift, obj->width, obj->height, z, true, RGB(255, 0, 255), !enabled);

    END_DRAW;
}

void button_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;

    //gradient
    color_t light_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t dark_color  = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
    if (obj->pressed)
    {
        color_t tmp = light_color;
        light_color = dark_color;
        dark_color = tmp;
    }
    draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1,
                  draw_struct.x2 - 1, draw_struct.y2 - 1, light_color, dark_color, true);

    //caption
    int shift = obj->pressed ? 1 : 0;
    string_t label = ((button_t*)obj)->label;
    label.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    agui_draw_string(draw_struct.agui, draw_struct.x1 + shift, draw_struct.y1 + shift,
                     draw_struct.x2 + shift, draw_struct.y2 + shift, &label, enabled);

    END_DRAW;
}

void checkbox_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;

    int x2 = draw_struct.x1 + AGUI_CHECKBOX_SIZE;
    int y2 = draw_struct.y1 + AGUI_CHECKBOX_SIZE;

    //box frame
    color_t frame_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FRAME));
    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1, x2,             draw_struct.y1, frame_color);
    graphics_draw_line(draw_struct.canvas, x2,             draw_struct.y1, x2,             y2,             frame_color);
    graphics_draw_line(draw_struct.canvas, draw_struct.x1, draw_struct.y1, draw_struct.x1, y2,             frame_color);
    graphics_draw_line(draw_struct.canvas, draw_struct.x1, y2,             x2,             y2,             frame_color);
    //box fill
    color_t light_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t dark_color  = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
    if (obj->pressed)
    {
        color_t tmp = light_color;
        light_color = dark_color;
        dark_color = tmp;
    }
    draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1, x2 - 1, y2 - 1, light_color, dark_color, true);

    if (((checkbox_t*)obj)->checked)
    {
        //check mark
        color_t mark_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
        graphics_draw_line(draw_struct.canvas, draw_struct.x1 + 3, draw_struct.y1 + 3, x2 - 3, y2 - 3, mark_color);
        graphics_draw_line(draw_struct.canvas, draw_struct.x1 + 4, draw_struct.y1 + 3, x2 - 3, y2 - 4, mark_color);
        graphics_draw_line(draw_struct.canvas, draw_struct.x1 + 3, draw_struct.y1 + 4, x2 - 4, y2 - 3, mark_color);
        graphics_draw_line(draw_struct.canvas, x2 - 3, draw_struct.y1 + 3, draw_struct.x1 + 3, y2 - 3, mark_color);
        graphics_draw_line(draw_struct.canvas, x2 - 4, draw_struct.y1 + 3, draw_struct.x1 + 3, y2 - 4, mark_color);
        graphics_draw_line(draw_struct.canvas, x2 - 3, draw_struct.y1 + 4, draw_struct.x1 + 4, y2 - 3, mark_color);
    }

    //caption
    string_t text = ((checkbox_t*)obj)->label;
    text.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    agui_draw_string(draw_struct.agui, x2 + 2, draw_struct.y1, draw_struct.x2, draw_struct.y2, &text, enabled);

    END_DRAW;
}

void form_draw_hue(struct obj_t* obj)
{
    if (!obj->visible)
        return;

    struct draw_struct_t draw_struct;
    if (begin_formdraw(obj, &draw_struct))
    {
        form_t* form = (form_t*)obj;
        DECLARATIONS;
        DRAW_FRAME;
        int captionheight;
        if (form->caption.text)
            captionheight = graphics_get_fontheight(draw_struct.canvas, form->caption.font) + 4;
        else
            captionheight = 0;
        //body
        color_t bcolor1 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_LIGHT));\
        color_t bcolor2 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_DARK));\
        draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + captionheight,
                      draw_struct.x2 - 1, draw_struct.y2 - 1, bcolor1, bcolor2, true);
        /*
        color_t back_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
        graphics_fill_rect(draw_struct.canvas, draw_struct.x1 + 1, draw_struct.y1 + captionheight,
                           draw_struct.x2 - draw_struct.x1 - 1, draw_struct.y2 - draw_struct.y1 - captionheight, back_color);
        */
        //caption
        if (captionheight)
        {
            color_t light_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
            color_t dark_color  = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
            draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1,
                          draw_struct.x2 - 1, draw_struct.y1 + captionheight - 1, light_color, dark_color, true);
            string_t caption = form->caption;
            caption.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
            agui_draw_string(draw_struct.agui, draw_struct.x1 + 1, draw_struct.y1 + 1,
                             draw_struct.x2 - 1, draw_struct.y1 + captionheight - 1, &caption, enabled);
        }
    }
    end_formdraw(obj, &draw_struct);
}

void icon_listbox_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;

    icon_listbox_t* listbox = (icon_listbox_t*)obj;
    int x2 = draw_struct.x1 + obj->width  - 1;
    int y2 = draw_struct.y1 + obj->height - 1;
    int y = draw_struct.y1 + 1;
    int w = obj->width - 2;
    int h = obj->height - 2;

    string_t string = listbox->item_string;
    int string_height = graphics_get_fontheight(draw_struct.canvas, string.font) + 2 * string.y;
    if (string_height < BITMAP_SIZE + 2)
        string_height = BITMAP_SIZE + 2;
    string.y = 0;

    listbox->window = h / string_height;
    bool scrollbar_enabled = (listbox->window < listbox->count) && enabled;
    bool scrollbar_visible = scrollbar_enabled || !listbox->autohide_scrollbar;

    obj->invalidated = false;
    obj->invalidated_child = false;

    if (scrollbar_visible)
    {
        listbox->scrollbar_visible = true;
        w -= AGUI_BAR_WIDTH;
        x2 -= AGUI_BAR_WIDTH;
    }

    //scrollbar
    if (scrollbar_visible)
    {
        _scrollbar_draw_t scrollbar;

        scrollbar.agui = draw_struct.agui;
        scrollbar.x = x2 + 1;
        scrollbar.y = draw_struct.y1 + 1;
        scrollbar.w = AGUI_BAR_WIDTH;
        scrollbar.h = h;
        scrollbar.enabled = scrollbar_enabled;
        scrollbar.range = listbox->count;
        scrollbar.window = listbox->window;
        scrollbar.position = listbox->first;

        _scrollbar_draw_hue(&draw_struct, &scrollbar);
    }

    //background
    color_t bcolor1 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_LIGHT));
    color_t bcolor2 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_DARK));
    draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1, x2, y2 - 1, bcolor1, bcolor2, true);

    //items
    list_t* l = listbox->list;
    bool top = true;
    bool bottom = false;
    int i = listbox->first;
    icon_listbox_item_t* item = list_get(&l, i);
    color_t textcolor = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    color_t fcolor1   = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t fcolor2   = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));

    while (item && ((y + string_height - 1) <= y2))
    {
        bottom = (y + string_height - 1) == y2;

        if (item->selected)
            draw_gradient(&draw_struct, draw_struct.x1 + 1, y, draw_struct.x1 + 1 + w, y + string_height - listbox->separator, fcolor1, fcolor2, true);

        if (item->icon)
            graphics_draw_bitmap_modified(draw_struct.canvas, item->icon, draw_struct.x1 + 1, y + 2, BITMAP_SIZE, BITMAP_SIZE, 0, true, RGB(255, 0, 255), !enabled);

        string.text = item->text;
        string.color = textcolor;
        agui_draw_string_clipped(draw_struct.agui, draw_struct.x1 + 1 + BITMAP_SIZE + string.x, y, x2, y + string_height, &string, enabled, true);

        if (!bottom && listbox->separator)
            graphics_draw_line(draw_struct.canvas, draw_struct.x1 + 1, y + string_height - 1, x2 - 1, y + string_height - 1, frame_color);

        item->invalidated = false;

        y += string_height;
        i++;
        top = false;
        item = list_get(&l, i);
    }

    END_DRAW;
}

void label_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;

    string_t text = ((label_t*)obj)->text;
    text.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    agui_draw_string(draw_struct.agui, draw_struct.x1, draw_struct.y1,
                     draw_struct.x2, draw_struct.y2, &text, enabled);

    END_DRAW;
}

void listbox_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;

    listbox_t* listbox = (listbox_t*)obj;
    int x2 = draw_struct.x1 + obj->width  - 1;
    int y2 = draw_struct.y1 + obj->height - 1;
    int y = draw_struct.y1 + 1;
    int w = obj->width - 2;
    int h = obj->height - 2;

    string_t string = listbox->item_string;
    int string_height = graphics_get_fontheight(draw_struct.canvas, string.font) + 2 * string.y;
    string.y = 0;

    listbox->window = h / string_height;
    bool scrollbar_enabled = (listbox->window < listbox->count) && enabled;
    bool scrollbar_visible = scrollbar_enabled || !listbox->autohide_scrollbar;

    obj->invalidated = false;
    obj->invalidated_child = false;

    if (scrollbar_visible)
    {
        listbox->scrollbar_visible = true;
        w -= AGUI_BAR_WIDTH;
        x2 -= AGUI_BAR_WIDTH;
    }

    //scrollbar
    if (scrollbar_visible)
    {
        _scrollbar_draw_t scrollbar;

        scrollbar.agui = draw_struct.agui;
        scrollbar.x = x2 + 1;
        scrollbar.y = draw_struct.y1 + 1;
        scrollbar.w = AGUI_BAR_WIDTH;
        scrollbar.h = h;
        scrollbar.enabled = scrollbar_enabled;
        scrollbar.range = listbox->count;
        scrollbar.window = listbox->window;
        scrollbar.position = listbox->first;

        _scrollbar_draw_hue(&draw_struct, &scrollbar);
    }

    //background
    color_t bcolor1 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_LIGHT));
    color_t bcolor2 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_DARK));
    draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1, x2, y2 - 1, bcolor1, bcolor2, true);

    //items
    list_t* l = listbox->list;
    bool top = true;
    bool bottom = false;
    int i = listbox->first;
    listbox_item_t* item = list_get(&l, i);
    color_t textcolor = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    color_t fcolor1   = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t fcolor2   = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));

    while (item && ((y + string_height - 1) <= y2))
    {
        bottom = (y + string_height - 1) == y2;

        if (item->selected)
            draw_gradient(&draw_struct, draw_struct.x1 + 1, y, draw_struct.x1 + 1 + w, y + string_height - listbox->separator, fcolor1, fcolor2, true);

        string.text = item->text;
        string.color = textcolor;
        agui_draw_string_clipped(draw_struct.agui, draw_struct.x1 + 1, y, x2, y + string_height, &string, enabled, true);

        if (!bottom && listbox->separator)
            graphics_draw_line(draw_struct.canvas, draw_struct.x1 + 1, y + string_height - 1, x2 - 1, y + string_height - 1, frame_color);

        item->invalidated = false;

        y += string_height;
        i++;
        top = false;
        item = list_get(&l, i);
    }

    END_DRAW;
}

void progressbar_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;

    unsigned int percent = ((progressbar_t*)obj)->percentage;
    if (percent > 100)
        percent = 100;

    color_t pcolor1 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t pcolor2 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
    color_t bcolor1 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_LIGHT));
    color_t bcolor2 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_DARK));

    int done, w, h;
    w = obj->width  - 2;
    h = obj->height - 2;
    if (w > h)
    {
        done = (percent * w) / 100;

        draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1,
                      draw_struct.x1 + 1 + done, draw_struct.y2 - 1, pcolor1, pcolor2, true);
        draw_gradient(&draw_struct, draw_struct.x1 + 1 + done, draw_struct.y1 + 1,
                      draw_struct.x2 - 1, draw_struct.y2 - 1, bcolor1, bcolor2, true);
    }
    else
    {
        done = (percent * h) / 100;

        draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y2 - 1 - done,
                      draw_struct.x2 - 1, draw_struct.y2 - 1, pcolor1, pcolor2, false);
        draw_gradient(&draw_struct, draw_struct.x1 + 1, draw_struct.y1 + 1,
                      draw_struct.x2 - 1, draw_struct.y2 - done, bcolor1, bcolor2, false);
    }

    END_DRAW;
}

void draw_round_gradient(struct draw_struct_t* draw_struct, int x1, int y1, int sz, color_t color1, color_t color2)
{
    int start = y1;
    int end   = y1 + sz;
    if (start > end)
    {
        int tmp = start;
        start = end;
        end = tmp;
    }

    int diff = end - start - 1;
    if (!diff)
        return;

    int c1_r, c1_g, c1_b;
    int c2_r, c2_g, c2_b;
    split_rgb(color1, &c1_r, &c1_g, &c1_b);
    split_rgb(color2, &c2_r, &c2_g, &c2_b);

    for (int i = 0; i <= diff; i++)
    {
        int r, g, b;
        r = c1_r + (c2_r - c1_r) * i / diff;
        g = c1_g + (c2_g - c1_g) * i / diff;
        b = c1_b + (c2_b - c1_b) * i / diff;

        int x = sqrt((sz - i) * i) - 1;

        graphics_draw_line(draw_struct->canvas, x1 + sz / 2 - x, start + i, x1 + sz / 2 + x, start + i, RGB(r, g, b));
    }
}

void _radiobutton_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;

    //mark
    color_t light_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t dark_color  = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
    if (obj->pressed)
    {
        color_t tmp = light_color;
        light_color = dark_color;
        dark_color = tmp;
    }
    draw_round_gradient(&draw_struct, draw_struct.x1, draw_struct.y1, AGUI_CHECKBOX_SIZE, light_color, dark_color);
    color_t frame_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FRAME));
    graphics_draw_circle(draw_struct.canvas, draw_struct.x1 + AGUI_CHECKBOX_SIZE / 2, draw_struct.y1 + AGUI_CHECKBOX_SIZE / 2,
        AGUI_CHECKBOX_SIZE / 2, frame_color);

    if (((radiobutton_t*)obj)->selected)
    {
        color_t mark_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
        graphics_fill_circle(draw_struct.canvas, draw_struct.x1 + AGUI_CHECKBOX_SIZE / 2, draw_struct.y1 + AGUI_CHECKBOX_SIZE / 2,
            (AGUI_CHECKBOX_SIZE - 1) / 4, mark_color);
    }

    //caption
    int x2 = draw_struct.x1 + AGUI_CHECKBOX_SIZE - 1;
    int y2 = draw_struct.y1 + AGUI_CHECKBOX_SIZE - 1;
    string_t text = ((radiobutton_t*)obj)->label;
    text.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    agui_draw_string(draw_struct.agui, x2 + 2, draw_struct.y1, draw_struct.x1 - 2, y2, &text, enabled);

    END_DRAW;
}

void _radiogroup_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;
    DRAW_BACKGROUND;

    radiogroup_t* rgroup = (radiogroup_t*)obj;
    //items
    for (int i = 0; i < rgroup->n_buttons; i++)
    {
        rgroup->buttons[i]->selected = (i == rgroup->selected_button);
        _radiobutton_draw_hue((obj_t*)rgroup->buttons[i]);
    }

    //text
    string_t text = rgroup->label;
    text.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    agui_draw_string(draw_struct.agui, draw_struct.x1 + 1, draw_struct.y1 + 1,
                  draw_struct.x2 - 1, draw_struct.y2 - 1, &text, enabled);

    END_DRAW;
}

void radiogroup_draw_hue(struct obj_t* obj)
{
    radiogroup_t *rgroup = (radiogroup_t*)obj;

    if (obj->invalidated)
    {
        _radiogroup_draw_hue(obj);
    }
    else
    {
        obj->invalidated_child = false;

        for (int i = 0; i < rgroup->n_buttons; i++)
            if (rgroup->buttons[i]->obj.invalidated)
            {
                rgroup->buttons[i]->selected = (i == rgroup->selected_button);
                _radiobutton_draw_hue((obj_t*)rgroup->buttons[i]);
            }
    }
}

void _scrollbar_draw_hue(draw_struct_t* draw_struct, _scrollbar_draw_t* scrollbar)
{
    int x2 = scrollbar->x + scrollbar->w - 1;
    int y2 = scrollbar->y + scrollbar->h - 1;

    int scrollsize = (scrollbar->w > scrollbar->h ? scrollbar->w : scrollbar->h) - 2 * AGUI_BAR_WIDTH;
    int offset = (scrollbar->w > scrollbar->h ? scrollbar->x : scrollbar->y) + AGUI_BAR_WIDTH;
    int handlesize;
    int span;
    int pos;

    if (scrollbar->range > scrollbar->window)
    {
        handlesize = scrollsize * scrollbar->window / scrollbar->range;
        if (handlesize < 3)
            handlesize = 3;
        span = scrollsize - handlesize;
        pos = scrollbar->position * span / (scrollbar->range - scrollbar->window) + offset;
    }
    else
    {
        handlesize = scrollsize;
        pos = offset;
    }

    int saturation = scrollbar->enabled ? 255 : 0;
    color_t frame_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FRAME));
    color_t fcolor1     = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t fcolor2     = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
    color_t bcolor1     = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_LIGHT));
    color_t bcolor2     = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_BG_DARK));
    color_t arrow_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));

    if (scrollbar->h > scrollbar->w)
    {
        //outer frame
        graphics_draw_line(draw_struct->canvas, scrollbar->x, scrollbar->y,                      x2,           scrollbar->y,                      frame_color);
        graphics_draw_line(draw_struct->canvas, x2,           scrollbar->y,                      x2,           y2,                                frame_color);
        graphics_draw_line(draw_struct->canvas, scrollbar->x, scrollbar->y,                      scrollbar->x, y2,                                frame_color);
        graphics_draw_line(draw_struct->canvas, scrollbar->x, y2,                                x2,           y2,                                frame_color);
        //arrow bounds
        graphics_draw_line(draw_struct->canvas, scrollbar->x, scrollbar->y + AGUI_BAR_WIDTH - 1, x2,           scrollbar->y + AGUI_BAR_WIDTH - 1, frame_color);
        graphics_draw_line(draw_struct->canvas, scrollbar->x, y2           - AGUI_BAR_WIDTH + 1, x2,           y2           - AGUI_BAR_WIDTH + 1, frame_color);
        //top arrow
        draw_gradient(draw_struct, scrollbar->x + 1, scrollbar->y + 1, x2 - 1, scrollbar->y + AGUI_BAR_WIDTH - 2,
                      fcolor1, fcolor2, false);
        graphics_fill_triangle(draw_struct->canvas, scrollbar->x +     AGUI_BAR_WIDTH / 3, scrollbar->y - 1 + 2 * AGUI_BAR_WIDTH / 3,
                                                    scrollbar->x +     AGUI_BAR_WIDTH / 2, scrollbar->y - 1 +     AGUI_BAR_WIDTH / 3,
                                                    scrollbar->x + 2 * AGUI_BAR_WIDTH / 3, scrollbar->y - 1 + 2 * AGUI_BAR_WIDTH / 3,
                                                    arrow_color);
        //bottom arrow
        draw_gradient(draw_struct, scrollbar->x + 1, y2 - AGUI_BAR_WIDTH + 2, x2 - 1, y2 - 1, fcolor1, fcolor2, false);
        graphics_fill_triangle(draw_struct->canvas, scrollbar->x +     AGUI_BAR_WIDTH / 3, y2 + 1 - AGUI_BAR_WIDTH +     AGUI_BAR_WIDTH / 3,
                                                    scrollbar->x +     AGUI_BAR_WIDTH / 2, y2 + 1 - AGUI_BAR_WIDTH + 2 * AGUI_BAR_WIDTH / 3,
                                                    scrollbar->x + 2 * AGUI_BAR_WIDTH / 3, y2 + 1 - AGUI_BAR_WIDTH +     AGUI_BAR_WIDTH / 3,
                                                    arrow_color);
        //top shaft
        if (scrollbar->y + AGUI_BAR_WIDTH <= pos - 1)
            draw_gradient(draw_struct, scrollbar->x + 1, scrollbar->y + AGUI_BAR_WIDTH, x2 - 1, pos - 1, bcolor1, bcolor2, false);
        //bottom shaft
        if (pos + handlesize <= y2 - AGUI_BAR_WIDTH)
            draw_gradient(draw_struct, scrollbar->x + 1, pos + handlesize, x2 - 1, y2 - AGUI_BAR_WIDTH, bcolor1, bcolor2, false);
        //handle
        draw_gradient(draw_struct, scrollbar->x + 1, pos + 1, x2 - 1, pos + handlesize - 2,  fcolor1, fcolor2, false);
        graphics_draw_line(draw_struct->canvas, scrollbar->x, pos,                  x2, pos,                  frame_color);
        graphics_draw_line(draw_struct->canvas, scrollbar->x, pos + handlesize - 1, x2, pos + handlesize - 1, frame_color);
    }
    else
    {
        //outer frame
        graphics_draw_line(draw_struct->canvas, scrollbar->x,                      scrollbar->y, x2,                                scrollbar->y, frame_color);
        graphics_draw_line(draw_struct->canvas, x2,                                scrollbar->y, x2,                                y2,           frame_color);
        graphics_draw_line(draw_struct->canvas, scrollbar->x,                      scrollbar->y, scrollbar->x,                      y2,           frame_color);
        graphics_draw_line(draw_struct->canvas, scrollbar->x,                      y2,           x2,                                y2,           frame_color);
        //arrow bounds
        graphics_draw_line(draw_struct->canvas, scrollbar->x + AGUI_BAR_WIDTH - 1, scrollbar->y, scrollbar->x + AGUI_BAR_WIDTH - 1, y2,           frame_color);
        graphics_draw_line(draw_struct->canvas, x2           - AGUI_BAR_WIDTH + 1, scrollbar->y, x2           - AGUI_BAR_WIDTH + 1, y2,           frame_color);
        //left arrow
        draw_gradient(draw_struct, scrollbar->x + 1, scrollbar->y + 1, scrollbar->x + AGUI_BAR_WIDTH - 2, y2 - 1,
                      fcolor1, fcolor2, true);
        graphics_fill_triangle(draw_struct->canvas, scrollbar->x - 1 + 2 * AGUI_BAR_WIDTH / 3, scrollbar->y +     AGUI_BAR_WIDTH / 3,
                                                    scrollbar->x - 1 +     AGUI_BAR_WIDTH / 3, scrollbar->y +     AGUI_BAR_WIDTH / 2,
                                                    scrollbar->x - 1 + 2 * AGUI_BAR_WIDTH / 3, scrollbar->y + 2 * AGUI_BAR_WIDTH / 3,
                                                    arrow_color);
        //right arrow
        draw_gradient(draw_struct, x2 - AGUI_BAR_WIDTH + 2, scrollbar->y + 1, x2 - 1, y2 - 1,
                      fcolor1, fcolor2, true);
        graphics_fill_triangle(draw_struct->canvas, x2 + 1 - AGUI_BAR_WIDTH +     AGUI_BAR_WIDTH / 3, scrollbar->y +     AGUI_BAR_WIDTH / 3,
                                                    x2 + 1 - AGUI_BAR_WIDTH + 2 * AGUI_BAR_WIDTH / 3, scrollbar->y +     AGUI_BAR_WIDTH / 2,
                                                    x2 + 1 - AGUI_BAR_WIDTH +     AGUI_BAR_WIDTH / 3, scrollbar->y + 2 * AGUI_BAR_WIDTH / 3,
                                                    arrow_color);
        //left shaft
        if (scrollbar->x + AGUI_BAR_WIDTH <= pos - 1)
            draw_gradient(draw_struct, scrollbar->x + AGUI_BAR_WIDTH, scrollbar->y + 1, pos - 1, y2 - 1, bcolor1, bcolor2, true);
        //right shaft
        if (pos + handlesize <= x2 - AGUI_BAR_WIDTH)
            draw_gradient(draw_struct, pos + handlesize, scrollbar->y + 1, x2 - AGUI_BAR_WIDTH, y2 - 1, bcolor1, bcolor2, true);
        //handle
        draw_gradient(draw_struct, pos + 1, scrollbar->y + 1, pos + handlesize - 2, y2 - 1, fcolor1, fcolor2, true);
        graphics_draw_line(draw_struct->canvas, pos,                  scrollbar->y, pos,                  y2, frame_color);
        graphics_draw_line(draw_struct->canvas, pos + handlesize - 1, scrollbar->y, pos + handlesize - 1, y2, frame_color);
    }
}

void scrollbar_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;

    _scrollbar_draw_t _scrollbar_i;

    _scrollbar_i.agui = draw_struct.agui;
    _scrollbar_i.x = draw_struct.x1;
    _scrollbar_i.y = draw_struct.y1;
    _scrollbar_i.w = obj->width;
    _scrollbar_i.h = obj->height;
    _scrollbar_i.enabled = enabled;
    _scrollbar_i.range = ((scrollbar_t*)obj)->range;
    _scrollbar_i.window = ((scrollbar_t*)obj)->window;
    _scrollbar_i.position = ((scrollbar_t*)obj)->position;

    _scrollbar_draw_hue(&draw_struct, &_scrollbar_i);

    END_DRAW;
}

void slider_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;

    int x1 = draw_struct.x1 + 1;
    int x2 = draw_struct.x2 - 1;
    int y1 = draw_struct.y1 + 1;
    int y2 = draw_struct.y2 - 1;
    int w = obj->width  - 2;
    int h = obj->height - 2;

    //gradient
    color_t light_color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_INNER_LIGHT));
    color_t dark_color  = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_INNER_DARK));
    if (h > w)
    {
        color_t tmp = light_color;
        light_color = dark_color;
        dark_color = tmp;
    }
    draw_gradient(&draw_struct, x1, y1, x2, y2, light_color, dark_color, h > w);

    //handle
    int high = ((slider_t*)obj)->high;
    int low  = ((slider_t*)obj)->low;
    int handlesize;
    int span;
    int offset;
    int pos;

    if (h > w)
    {
        handlesize = (((slider_t*)obj)->handlesize * h) / 100;
        if (handlesize < 3)
            handlesize = 3;
        span = h - handlesize;
        offset = y1;

        int tmp = high;
        high = low;
        low = tmp;
    }
    else
    {
        handlesize = (((slider_t*)obj)->handlesize * w) / 100;
        if (handlesize < 3)
            handlesize = 3;
        span = w - handlesize;
        offset = x1;
    }
    if (high > low)
        pos = ((span * (((slider_t*)obj)->position - low)) / (high - low)) + offset;
    else
        pos = ((span * (low - ((slider_t*)obj)->position)) / (low - high)) + offset;

    color_t fcolor1 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_LIGHT));
    color_t fcolor2 = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_FG_DARK));
    if (h > w)
    {
        graphics_draw_line(draw_struct.canvas, x1, pos,                  x1 + w - 1, pos,                  frame_color);
        graphics_draw_line(draw_struct.canvas, x1, pos + handlesize - 1, x1 + w - 1, pos + handlesize - 1, frame_color);
        pos++;
        y1++;
        h = handlesize - 2;
        draw_gradient(&draw_struct, x1, pos, x1 + w - 1, pos + h - 1, fcolor1, fcolor2, false);
    }
    else
    {
        graphics_draw_line(draw_struct.canvas, pos,                  y1, pos,                  y1 + h - 1, frame_color);
        graphics_draw_line(draw_struct.canvas, pos + handlesize - 1, y1, pos + handlesize - 1, y1 + h - 1, frame_color);
        pos++;
        x1++;
        w = handlesize - 2;
        draw_gradient(&draw_struct, pos, y1, pos + w - 1, y1 + h - 1, fcolor1, fcolor2, true);
    }

    END_DRAW;
}

void textarea_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;
    DRAW_BACKGROUND;

    //text
    string_t text = ((textarea_t*)obj)->text;
    text.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    int x_text = draw_struct.x1 + 1 + text.x;
    int y_text = draw_struct.y1 + 1 + text.y;
    int w = draw_struct.x2 - draw_struct.x1 - 2;
    int line_height = graphics_get_fontheight(draw_struct.canvas, text.font) + 1;

    while ((text.text != NULL) && (*text.text != '\0') && (y_text + graphics_get_fontheight(draw_struct.canvas, text.font) + 1 < draw_struct.y2 - 1))
    {
        int strl = agui_get_breaking_string_count(draw_struct.agui, &text, w - 2 * text.x);
        x_text = agui_align_horizontal_count(draw_struct.agui, draw_struct.x1 + 1, draw_struct.x2 - 1, &text, strl);
        graphics_draw_nstring(draw_struct.canvas, x_text, y_text, text.text, text.font, text.color, text.fontstyle, strl);  // draw text
        y_text += line_height + text.y;
        text.text += strl;
        if (*text.text == '\n' || *text.text == '\t' || *text.text == ' ')
            text.text++;
    }

    END_DRAW;
}

void textbox_draw_hue(struct obj_t* obj)
{
    BEGIN_DRAW;
    DECLARATIONS;
    DRAW_FRAME;
    DRAW_BACKGROUND;

    //text
    string_t text = ((textbox_t*)obj)->text;
    text.color = hsl_to_rgb(HUE, saturation, GET_LUM(LUM_TEXT));
    agui_draw_string_clipped(draw_struct.agui, draw_struct.x1 + 1, draw_struct.y1 + 1,
                  draw_struct.x2 - 1, draw_struct.y2 - 1, &text, enabled, true);

    END_DRAW;
}
