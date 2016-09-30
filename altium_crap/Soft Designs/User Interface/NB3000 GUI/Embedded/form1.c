#include <agui.h>
#include <stdio.h>
#include "form1.h"
#include "devices.h"
#include <drv_ioport.h>


/* Using the way below to initialize structures (conform C99):
 * 1) Makes it more readable
 * 2) Gives better compatibility for further versions of AGUI,
 *       - if the order of the members change, it still works
 *       - if members are added, it's easier to initialize them
 */

listbox_t form1_lb1 =
{
    .obj.x = 10,
    .obj.y = 30,
    .obj.width = 180,
    .obj.height = 170,
    .obj.draw = listbox_draw,
    .obj.handler = listbox_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI_1,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = true,
    .count = 0,
    .first = 0,
    .select = NULL,
    .window = 0,
    .relief = RELIEF_LOWERED,
    .separator = true,
    .multiselect = false,
    .item_string.x = 2,
    .item_string.y = 1,
    .item_string.font = &bitstreamverasans8,
    .item_string.color = BLACK,
    .item_string.fontstyle = FS_ITALIC,
    .item_string.align = ALIGN_VERTICAL_LEFT,
    .selectioncolor = PALETURQUOISE,
    .color = LIGHTYELLOW
};

checkbox_t form1_cb1 =
{
    .obj.x = 10,
    .obj.y = 210,
    .obj.width = 70,
    .obj.height = 20,
    .obj.draw = checkbox_draw,
    .obj.handler = checkbox_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI_1,
    .obj.action = form1_cb1_action,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = true,
    .label.x = 2,
    .label.y = 0,
    .label.text = "Multi-Select For Listbox",
    .label.font = &bitstreamverasans8,
    .label.color = BLACK,
    .label.fontstyle = FS_NONE,
    .label.align = ALIGN_VERTICAL_LEFT,
    .checked = false,
    .relief = RELIEF_LOWERED,
    .color = CADETBLUE
};

button_t form1_btn1 =
{
    .obj.x = 200,
    .obj.y = 40,
    .obj.width = 110,
    .obj.height = 20,
    .obj.draw = button_draw,
    .obj.handler = button_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI_1,
    .obj.action = form1_btn1_action,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = true,
    .obj.pressed = false,
    .label.x = 0,
    .label.y = 0,
    .label.text = "Add Line To Listbox",
    .label.font = &bitstreamverasans8,
    .label.color = BLACK,
    .label.fontstyle = FS_NONE,
    .label.align = ALIGN_CENTRE,
    .relief = RELIEF_LOWERED,
    .color = CADETBLUE
};

button_t form1_btn2 =
{
    .obj.x = 200,
    .obj.y = 70,
    .obj.width = 110,
    .obj.height = 20,
    .obj.draw = button_draw,
    .obj.handler = button_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI_1,
    .obj.action = form1_btn2_action,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = true,
    .obj.pressed = false,
    .label.x = 0,
    .label.y = 0,
    .label.text = "Sort Listbox",
    .label.font = &bitstreamverasans8,
    .label.color = BLACK,
    .label.fontstyle = FS_NONE,
    .label.align = ALIGN_CENTRE,
    .relief = RELIEF_LOWERED,
    .color = CADETBLUE
};

slider_t form1_slr1 =
{
    .obj.x = 245,
    .obj.y = 120,
    .obj.width = 21,
    .obj.height = 100,
    .obj.draw = slider_draw,
    .obj.handler = slider_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI_1,
    .obj.action = form1_slr1_action,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = true,
    .low = 0,
    .high = 127,
    .position = 0,
    .handlesize = 20,
    .relief = RELIEF_LOWERED,
    .color1 = CADETBLUE,
    .color2 = CADETBLUE,
    .handlecolor = MAROON
};

obj_t *form1_children[] =
{
    AGUI_HANDLE(form1_lb1),
    AGUI_HANDLE(form1_cb1),
    AGUI_HANDLE(form1_btn1),
    AGUI_HANDLE(form1_btn2),
    AGUI_HANDLE(form1_slr1),
};

form_t form1 =
{
    .obj.x = 0,
    .obj.y = 0,
    .obj.width = 320,
    .obj.height = 240,
    .obj.draw = form_draw,
    .obj.handler = form_handler,
    .obj.parent = NULL,
    .obj.agui_index = AGUI_1,
    .obj.cursor_shape = &cursor_crosshair,
    .obj.visible = true,
    .obj.enabled = true,
    .caption.x = 0,
    .caption.y = 0,
    .caption.text = "AGUI Example",
    .caption.font = &bitstreamverasans8,
    .caption.color = BLACK,
    .caption.fontstyle = FS_BOLD_ITALIC,
    .caption.align = ALIGN_CENTRE,
    .captionbarcolor = SKYBLUE,
    .children = form1_children,
    .n_children = sizeof(form1_children) / sizeof(form1_children[0]),
    .relief = RELIEF_LOWERED,
    .color = LIGHTSTEELBLUE
};


void form1_cb1_action(obj_t *obj, const action_event_t *action)
{
    if (action->button == BUTTON_LEFT && action->event == ACTION_CLICKED)
    {
        listbox_set_multiselect(AGUI_HANDLE(form1_lb1), form1_cb1.checked);
    }
}

void form1_btn1_action(obj_t *obj, const action_event_t *action)
{
    char str[10];
    if (action->button == BUTTON_LEFT && action->event == ACTION_CLICKED)
    {
        sprintf(str, "line %d", listbox_count(&form1_lb1));
        listbox_add(&form1_lb1, str, NULL);
    }
}

void form1_btn2_action(obj_t *obj, const action_event_t *action)
{
    if (action->button == BUTTON_LEFT && action->event == ACTION_CLICKED)
    {
        listbox_sort(&form1_lb1, false);
    }
}

void form1_slr1_action( obj_t *obj, const action_event_t *action)
{
    int pos;
    if (action->button == BUTTON_LEFT && action->event == ACTION_DRAGGED)
    {
        /* output position */
        pos = 1 << (form1_slr1.position / 16);
        ioport_set_value(ioport, 0, pos);
    }
}


