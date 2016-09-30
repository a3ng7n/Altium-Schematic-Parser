#ifndef FORM1_H
#define FORM1_H
#include <agui.h>
#include <audio.h>

extern form_t form1;
extern listbox_t form1_dirs;
extern listbox_t form1_files;
extern label_t form1_dirs_label;
extern label_t form1_files_label;
extern button_t form1_play;
extern button_t form1_stop;
extern textbox_t form1_info;
extern obj_t *form1_children[];
extern char info_string[];
extern audio_t * audio;
extern bool play;
extern agui_t * agui;

extern bool gui_play;
extern bool gui_stop;
extern char gui_dir_file[];

extern void form1_dirs_action(obj_t *obj, const action_event_t *action);
extern void form1_files_action(obj_t *obj, const action_event_t *action);
extern void form1_play_action(obj_t *obj, const action_event_t *action);
extern void form1_stop_action(obj_t *obj, const action_event_t *action);
extern void dirlisting( const char *path );
extern void gui_info(const char *info);

#endif
