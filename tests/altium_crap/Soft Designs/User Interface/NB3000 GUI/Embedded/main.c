#include <graphics.h>
//#include <pthread.h>
#include <agui.h>
#include <pointer.h>
#include <touchscreen.h>
#include <stdio.h>

#include "devices.h"
#include "form1.h"


void *update(void *arg);
void *handler(void *arg);
void *action(void *arg);

agui_t *agui;
ioport_t *ioport;


int main(void)
{
    agui = agui_open(AGUI_1);
    ioport = ioport_open(DRV_IOPORT_1);

    listbox_add(&form1_lb1, "first line", NULL);
    listbox_add(&form1_lb1, "second line", NULL);
    listbox_insert(&form1_lb1, "inserted line", NULL, 1);
    listbox_insert(&form1_lb1, "This line is much too long to fit in one line", NULL, 1);
    listbox_sort(&form1_lb1, false);

    agui_show_form(AGUI_HANDLE(form1));
    cursor_show(agui);

    while (1)
    {
        agui_service(agui);
    }
}

