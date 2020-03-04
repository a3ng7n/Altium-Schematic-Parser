#ifndef FORM1_H
#define FORM1_H

#include <agui.h>
#include <drv_ioport.h>

extern ioport_t *ioport;

extern form_t form1;
extern void form1_btn1_action(obj_t *obj, const action_event_t *action);
extern void form1_btn2_action(obj_t *obj, const action_event_t *action);
extern void form1_slr1_action( obj_t *obj, const action_event_t *action);
extern void form1_cb1_action(obj_t *obj, const action_event_t *action);

extern button_t         form1_btn1;
extern checkbox_t       form1_cb1;
extern slider_t         form1_slr1;
extern listbox_t        form1_lb1;
extern obj_t           *form1_children[];


#endif
