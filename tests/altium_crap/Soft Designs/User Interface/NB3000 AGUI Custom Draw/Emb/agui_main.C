#include <agui.h>
#include <drv_vga_ili9320.h>
#include "devices.h"
#include "agui_main.H"
#include "theme_hue.h"

agui_t *agui;
form_t *main_form;

void agui_main(void)
{
    agui = agui_open(AGUI_1);
    main_form = &form1;

    vga_ili9320_t *ili9320;
    ili9320 = vga_ili9320_open(DRV_VGA_ILI9320_1);
    vga_ili9320_set_auto_refresh(ili9320, true);

    UPDATE_FORM;
    agui_show_form(AGUI_HANDLE_PTR(main_form));
    cursor_show(agui);

    while (1)
    {
        agui_service(agui);
    }
}

