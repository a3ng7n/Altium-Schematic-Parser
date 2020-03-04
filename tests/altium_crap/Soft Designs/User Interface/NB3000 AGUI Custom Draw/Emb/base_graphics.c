#include "base_graphics.h"
#include "agui_cfg.h"

double hue_to_rgb(double value1, double value2, double value3)
{
    if (value3 < 0)
        value3 = value3 + 1;
    if (value3 > 1)
        value3 = value3 - 1;

    if (6 * value3 < 1)
        return value1 + (value2 - value1) * 6 * value3;
    else if (2 * value3 < 1)
        return value2;
    else if (3 * value3 < 2)
        return value1 + (value2 - value1) * (((double)2 / 3) - value3) * 6;
    else
        return value1;
}

color_t hsl_to_rgb(int hue, int sat, int lum)
{
    double H, S, L, R, G, B;
    double temp1, temp2;

    H = (double)hue / 255;
    S = (double)sat / 255;
    L = (double)lum / 255;

    if (S == 0)
        return RGB(lum, lum, lum);

    if (L < 0.5)
        temp2 = L * (1 + S);
    else
        temp2 = L + S - L * S;
    temp1 = 2 * L - temp2;

    R = hue_to_rgb(temp1, temp2, H + (double)1 / 3);
    G = hue_to_rgb(temp1, temp2, H                );
    B = hue_to_rgb(temp1, temp2, H - (double)1 / 3);

    return RGB((int)(255 * R), (int)(255 * G), (int)(255 * B));
}

void split_rgb(color_t color, int *r, int *g, int *b)
{
    *b = color & 0xFF;
    color >>= 8;
    *g = color & 0xFF;
    color >>= 8;
    *r = color & 0xFF;
}

void draw_gradient(struct draw_struct_t* draw_struct,
                   int x1, int y1, int x2, int y2,
                   color_t color1, color_t color2, bool vertical)
{
    int start, end;
    if (vertical)
    {
        start = y1;
        end   = y2;
    }
    else
    {
        start = x1;
        end   = x2;
    }
    if (start > end)
    {
        int tmp = start;
        start = end;
        end = tmp;
    }

    int diff = end - start;
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

        if (vertical)
            graphics_draw_line(draw_struct->canvas, x1, start + i, x2, start + i, RGB(r, g, b));
        else
            graphics_draw_line(draw_struct->canvas, start + i, y1, start + i, y2, RGB(r, g, b));
    }
}

void draw_bi_gradient(struct draw_struct_t* draw_struct,
                      int x1, int y1, int x2, int y2,
                      color_t color11, color_t color12,
                      color_t color21, color_t color22)
{
    if (x1 == x2)
        return;
    if (y1 == y2)
        return;

    int c11_r, c11_g, c11_b;
    int c12_r, c12_g, c12_b;
    int c21_r, c21_g, c21_b;
    int c22_r, c22_g, c22_b;
    split_rgb(color11, &c11_r, &c11_g, &c11_b);
    split_rgb(color12, &c12_r, &c12_g, &c12_b);
    split_rgb(color21, &c21_r, &c21_g, &c21_b);
    split_rgb(color22, &c22_r, &c22_g, &c22_b);

    int diff = y2 - y1;
    for (int i = 0; i <= diff; i++)
    {
        color_t leftcolor, rightcolor;
        int r, g, b;
        r = c11_r + (c21_r - c11_r) * i / diff;
        g = c11_g + (c21_g - c11_g) * i / diff;
        b = c11_b + (c21_b - c11_b) * i / diff;
        leftcolor = RGB(r, g, b);
        r = c12_r + (c22_r - c12_r) * i / diff;
        g = c12_g + (c22_g - c12_g) * i / diff;
        b = c12_b + (c22_b - c12_b) * i / diff;
        rightcolor = RGB(r, g, b);
        draw_gradient(draw_struct, x1, y1 + i, x2, y1 + i, leftcolor, rightcolor, false);
    }
}
