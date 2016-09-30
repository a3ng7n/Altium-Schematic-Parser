#include "generic_devices.h"
#include "devices.h"
#include <timing.h>
#include <stdint.h>
#include <graphics.h>
#include <SVG_colors.h>
#include <interrupts.h>
#include <touchscreen.h>
#include <drv_pwmx.h>
#include <stdio.h>
#include <drv_vga_ili9320.h>

typedef struct moving_ball
{
    int32_t x, y, x_change, y_change;
    color_t color;
    int32_t x_speed, y_speed;
    int32_t radius;
} moving_ball_t;


int left;
int right;
int top;
int bottom;


void init_ball(bool left, bool top, color_t color, int32_t x_speed, int32_t y_speed, int32_t radius, moving_ball_t* ball)
{
    ball->color = color;
    ball->x_speed = x_speed;
    ball->y_speed = y_speed;
    ball->radius = radius;
    if (left)
    {
        ball->x        = left + radius;
        ball->x_change = x_speed;
    }
    else
    {
        ball->x        = right - radius;
        ball->x_change = -x_speed;
    }
    if (top)
    {
        ball->y        = top + radius;
        ball->y_change = y_speed;
    }
    else
    {
        ball->y        = bottom - radius;
        ball->y_change = -y_speed;
    }
}

void move_ball(moving_ball_t* ball)
{
    if (ball->x >= right - ball->radius && ball->x_change > 0)
        ball->x_change = -ball->x_speed;
    else if (ball->x <= left + ball->radius && ball->x_change < 0)
        ball->x_change = ball->x_speed;
    if (ball->y >= bottom - ball->radius && ball->y_change > 0)
        ball->y_change = -ball->y_speed;
    else if (ball->y <= top + ball->radius && ball->y_change < 0)
        ball->y_change = ball->y_speed;
    ball->x += ball->x_change;
    ball->y += ball->y_change;
}

void move_ball_to(moving_ball_t* ball, int32_t x, int32_t y)
{
    ball->x = x;
    ball->y = y;
}

graphics_t* graphics;
canvas_t *canvas;
uintptr_t buffer;
vga_ili9320_t *vga;

moving_ball_t ball_1, ball_2, ball_3, ball_4;

void draw_ball(moving_ball_t* ball)
{
    graphics_fill_circle(canvas, ball->x, ball->y, ball->radius, ball->color);
}

#define BRIGHT_MAX 0xFF
#define BRIGHT_MIN 0x10

void main(void)
{
    int32_t brightness, bright_dir;
    touchscreen_t* touchscreen;
    pwmx_t* pwm;
    touchscreen_data_t ts_data;
    touchscreen = touchscreen_open(TOUCHSCREEN_1);
    pwm = pwmx_open(DRV_PWMX_1);

    graphics = graphics_open(GRAPHICS_1);
    vga = vga_ili9320_open(DRV_VGA_ILI9320_1);
    canvas = graphics_get_visible_canvas(graphics);
    buffer = canvas_get_buffer(canvas);

    left = 0;
    right = canvas_get_width(canvas) - 1;
    top = 0;
    bottom = canvas_get_height(canvas) - 1;

    init_ball(true , false, CYAN     , 1, 2, 90, &ball_1);
    init_ball(false, true , LAWNGREEN, 2, 2, 50, &ball_2);
    init_ball(true , true , PINK     , 3, 4, 20, &ball_3);
    init_ball(false, false, WHITE    , 5, 1, 10, &ball_4);

    pwmx_set_resolution_mode(pwm, PWMX_MODE_8BIT);
    pwmx_set_frequency(pwm, 10000);
    pwmx_enable_controller(pwm);
    pwmx_set_pulsewidth(pwm, BRIGHT_MAX);
    brightness = BRIGHT_MIN;
    bright_dir = 1;
    while(1)
    {
//        pwmx_set_pulsewidth(pwm, (char)brightness);
        brightness = brightness + bright_dir;
        if (brightness == BRIGHT_MAX)
            bright_dir = -1;
        else if (brightness == BRIGHT_MIN)
            bright_dir = 1;
        graphics_fill_canvas(canvas, BLUE);
        graphics_draw_line(canvas,     0,      0, right,      0, WHITE);
        graphics_draw_line(canvas,     0, bottom, right, bottom, WHITE);
        graphics_draw_line(canvas,     0,      0,     0, bottom, WHITE);
        graphics_draw_line(canvas, right,      0, right, bottom, WHITE);
        graphics_draw_string(canvas, 5, 5, "Top Left", NULL, WHITE, FS_NONE);
        draw_ball(&ball_1);
        draw_ball(&ball_2);
        draw_ball(&ball_3);
        draw_ball(&ball_4);
        if (touchscreen_get_pos(touchscreen, &ts_data) && ts_data.pendown)
        {
            move_ball_to(&ball_1, ts_data.x, ts_data.y);
            move_ball_to(&ball_2, ts_data.x, ts_data.y);
            move_ball_to(&ball_3, ts_data.x, ts_data.y);
            move_ball_to(&ball_4, ts_data.x, ts_data.y);
        }
        else
        {
            move_ball(&ball_1);
            move_ball(&ball_2);
            move_ball(&ball_3);
            move_ball(&ball_4);
        }
        graphics_set_visible_canvas(graphics, canvas);
        while (!graphics_visible_canvas_is_set(graphics));
    }
}
