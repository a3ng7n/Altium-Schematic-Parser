#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include <graphics.h>
#include <touchscreen.h>
#include <pointer.h>
#include <drv_led.h>
#include "generic_devices.h"
#include "devices.h"
#include "led_info.h"

#define WIDTH 320
#define HEIGHT 240

char *cal1 = "Touch screen at marker";
graphics_t * display;
canvas_t * canvas;
touchscreen_t * tft_touch;
touchscreen_data_t * position;
touchscreen_callback_t callback;
pointer_t * ptr;
pointer_state_t * pointer_state;
led_t * leds;

extern __no_sdata graphics_bitmap_t _lc_ub_rgbcolors_bmp; // linker name for beginning of bitmap block
extern __no_sdata graphics_bitmap_t _lc_ue_rgbcolors_bmp;
graphics_bitmap_t * bmp = &_lc_ub_rgbcolors_bmp;

void update_leds (const uint32_t * col_buff);
void set_all_leds (uint32_t value);
static void draw_mark(int x, int y, int width, int height, void *vp);

void main (void)
{
    // Connect to our drivers for the TFT, touch sensor.
    tft_touch = touchscreen_open(TOUCHSCREEN_1);
    ptr = pointer_open(POINTER_1);
    display = graphics_open(GRAPHICS_1);
    canvas = graphics_get_visible_canvas(display);
    leds = led_open(DRV_LED_1);
    led_turn_all_off(leds);

    // Clear screen
    graphics_fill_canvas(canvas, BLACK);
    graphics_set_visible_canvas(display, canvas);

    // Update display and turn off LEDs
    while(!graphics_visible_canvas_is_set(display));

    // Set up and calibrate touch screen.
    touchscreen_set_callback(tft_touch, draw_mark, canvas);
    while(!touchscreen_calibrate(tft_touch, 320, 240))
    {
        set_all_leds(0xFF0000); // If Touchscreen can't calibrate RED ALERT!
    }
    led_turn_all_off(leds);

    // Put colormap on TFT
    graphics_draw_bitmap(canvas, bmp, 0, 0, 320, 240, 0);
    graphics_set_visible_canvas(display, canvas);

    // Start picking colors!
    while(1)
    {
        if (pointer_update(ptr, pointer_state))
        {
            set_all_leds(graphics_get_pixel(canvas, pointer_state->x, pointer_state->y));
        }
    }
}

void set_all_leds (uint32_t value)
{
    for(int i = 0; i < LEDS_NUM_LED_IDS; )
    {
        led_set_intensity(leds, i++, (uint8_t)(value>>16)); // Red
        led_set_intensity(leds, i++, (uint8_t)(value>>8));  // Green
        led_set_intensity(leds, i++, (uint8_t)value);       // Blue
    }
}

static void draw_mark(int x, int y, int width, int height, void *vp)
{
     graphics_draw_circle(canvas, x, y, 10, 0xff00ff);
     graphics_draw_line(canvas, x - 15, y, x + 15, y, 0x00ffff);
     graphics_draw_line(canvas, x, y - 15, x, y + 15, 0x00ffff);
     graphics_draw_string(canvas, 50, 110, cal1, NULL, 0xffffff, 0);
     graphics_set_visible_canvas(display, canvas);
}
