#include <agui.h>
#include <stdio.h>
#include "form1.h"
#include "devices.h"

#include <unistd.h>
#include <string.h>

bool gui_play;
bool gui_stop;
char gui_dir_file[300];


obj_t *form1_children[] =
{
    AGUI_HANDLE(form1_dirs),
    AGUI_HANDLE(form1_files),
    AGUI_HANDLE(form1_dirs_label),
    AGUI_HANDLE(form1_files_label),
    AGUI_HANDLE(form1_play),
    AGUI_HANDLE(form1_stop),
    AGUI_HANDLE(form1_info),
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
    .obj.agui_index = AGUI,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = true,
    .caption.x = 0,
    .caption.y = 0,
    .caption.text = "MP3 Player",
    .caption.font = &bitstreamverasans10,
    .caption.color = BLACK,
    .caption.fontstyle = FS_BOLD,
    .caption.align = ALIGN_CENTRE,
    .captionbarcolor = LIGHTSKYBLUE,
    .children = form1_children,
    .n_children = sizeof(form1_children) / sizeof(form1_children[0]),
    .relief = RELIEF_NONE,
    .color = GRAY15
};

listbox_t form1_dirs =
{
    .obj.x = 20,
    .obj.y = 40,
    .obj.width = 135,
    .obj.height = 120,
    .obj.draw = listbox_draw,
    .obj.handler = listbox_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = false,
    .obj.action = form1_dirs_action,
    .count = 0,
    .first = 0,
    .select = NULL,
    .window = 4,
    .relief = RELIEF_LOWERED,
    .separator = true,
    .multiselect = false,
    .item_string.x = 4,
    .item_string.y = 1,
//    .item_string.font = &bitstreamverasans10,
    .item_string.color = BLACK,
    .item_string.fontstyle = FS_NONE,
    .item_string.align = ALIGN_LEFT,
    .selectioncolor = WHITE,
    .color = LIGHTCYAN
};

listbox_t form1_files =
{
    .obj.x = 165,
    .obj.y = 40,
    .obj.width = 135,
    .obj.height = 120,
    .obj.draw = listbox_draw,
    .obj.handler = listbox_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = false,
    .obj.action = form1_files_action,
    .count = 0,
    .first = 0,
    .select = NULL,
    .window = 4,
    .relief = RELIEF_LOWERED,
    .separator = true,
    .multiselect = false,
    .item_string.x = 4,
    .item_string.y = 1,
//    .item_string.font = &bitstreamverasans10,
    .item_string.color = BLACK,
    .item_string.fontstyle = FS_NONE,
    .item_string.align = ALIGN_LEFT,
    .selectioncolor = SNOW,
    .color = POWDERBLUE
};

label_t form1_dirs_label =
{
    .obj.x = 22,
    .obj.y = 40,
    .obj.draw = label_draw,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.visible = true,
    .obj.enabled = true,
    .text.text = "Directories",
    .text.x = 0,
    .text.y = 0,
//    .text.font = &bitstreamverasans10,
    .text.color = LIGHTCYAN,
    .text.align = ALIGN_BOTTOM_LEFT,
    .text.fontstyle = FS_NONE
};

label_t form1_files_label =
{
    .obj.x = 167,
    .obj.y = 40,
    .obj.draw = label_draw,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.visible = true,
    .obj.enabled = true,
    .text.text = "Files",
    .text.x = 0,
    .text.y = 0,
//    .text.font = &bitstreamverasans10,
    .text.color = POWDERBLUE,
    .text.align = ALIGN_BOTTOM_LEFT,
    .text.fontstyle = FS_NONE
};

button_t form1_play =
{
    .obj.x = 20,
    .obj.y = 205,
    .obj.width = 90,
    .obj.height = 25,
    .obj.draw = button_draw,
    .obj.handler = button_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI,
    .obj.action = form1_play_action,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = false,
    .obj.pressed = false,
    .label.x = 0,
    .label.y = 0,
    .label.text = "Play",
    .label.font = &bitstreamverasans10,
    .label.color = BLACK,
    .label.fontstyle = FS_BOLD,
    .label.align = ALIGN_CENTRE,
    .relief = RELIEF_RAISED,
    .color = MEDIUMSPRINGGREEN
};

button_t form1_stop =
{
    .obj.x = 210,
    .obj.y = 205,
    .obj.width = 90,
    .obj.height = 25,
    .obj.draw = button_draw,
    .obj.handler = button_handler,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI,
    .obj.action = form1_stop_action,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = false,
    .obj.pressed = false,
    .label.x = 0,
    .label.y = 0,
    .label.text = "Stop",
    .label.font = &bitstreamverasans10,
    .label.color = BLACK,
    .label.fontstyle = FS_BOLD,
    .label.align = ALIGN_CENTRE,
    .relief = RELIEF_RAISED,
    .color = CRIMSON
};

static char info_text[64] = "";

textbox_t form1_info =
{
    .obj.x = 20,
    .obj.y = 175,
    .obj.width = 280,
    .obj.height = 19,
    .obj.draw = textbox_draw,
    .obj.parent = AGUI_HANDLE(form1),
    .obj.agui_index = AGUI,
    .obj.cursor_shape = &cursor_arrow,
    .obj.visible = true,
    .obj.enabled = true,
    .color = PEACHPUFF,
    .text.x = 2,
    .text.y = 2,
    .text.font = &bitstreamverasans10,
    .text.text = info_text
};

void form1_dirs_action(obj_t *obj, const action_event_t *action)
{
    int index;

    if (action->event == ACTION_RELEASED)
    {
        gui_info("");

        if (listbox_get_selection(&form1_dirs, &index, 1))
        {
            strcpy(gui_dir_file, listbox_get_text(&form1_dirs, index));
            chdir(gui_dir_file);
            getcwd(gui_dir_file, sizeof(gui_dir_file));
            dirlisting(gui_dir_file);
            listbox_sort(&form1_dirs, false);
            obj_set_enabled(AGUI_HANDLE(form1_play), false);
        }
    }
}

void form1_files_action(obj_t *obj, const action_event_t *action)
{
    int index;

    if (action->event == ACTION_RELEASED)
    {
        gui_info("");

        if (listbox_get_selection(&form1_files, &index, 1))
        {
            getcwd(gui_dir_file, sizeof(gui_dir_file));
            strcat(gui_dir_file, "/");
            strcat(gui_dir_file, listbox_get_text(&form1_files, index));

            obj_set_enabled(AGUI_HANDLE(form1_play), true);
        }
        else
        {
            obj_set_enabled(AGUI_HANDLE(form1_play), false);
        }
    }
}

void form1_play_action(obj_t *obj, const action_event_t *action)
{
    int index;

    if (action->event == ACTION_RELEASED)
    {
        if (listbox_get_selection(&form1_files, &index, 1))
        {
            getcwd(gui_dir_file, sizeof(gui_dir_file));
            strcat(gui_dir_file, "/");
            strcat(gui_dir_file, listbox_get_text(&form1_files, index));

            gui_play = true;
        }
    }
}


void form1_stop_action(obj_t *obj, const action_event_t *action)
{
    if (action->event == ACTION_RELEASED)
    {
        gui_info("");
        gui_stop = true;
    }
}

void gui_info(const char *info)
{
    strcpy(form1_info.text.text, info);
    obj_invalidate(AGUI_HANDLE(form1_info));

    printf("%s\n", info); // echo to terminal
}
