#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include "graphics.h"
#include "devices.h"

#include "display_vga.h"
#include "tetris.h"

/* x offset in cell units */
#define X_OFF                   5
/* y offset in cell units */
#define Y_OFF                   4

/* size cell pixels */
#define W_CELL                  10

/* board is (H_BOARD+2)(W_BOARD+2) */
#define B_X0                    X_OFF
#define B_X1                    B_X0 + LASTBOARDCOLUMN+1
#define B_Y0                    Y_OFF
#define B_Y1                    B_Y0 + LASTBOARDROW+1

#define S_OFF_Y                 4
#define S_POS_Y                 B_Y1 + S_OFF_Y
#define S_OFF_X                 2
#define S_POS_X                 B_X0 + S_OFF_X
#define S_N_OFF_X               10
#define S_N_POS_X               10 + S_POS_X

#define H_SCREEN 240
#define W_SCREEN 320

#define THE_TIME                "System time:"

/* linker labels */
extern __no_sdata graphics_bitmap_t _lc_ub_left_bmp;
extern __no_sdata graphics_bitmap_t _lc_ub_right_bmp;
extern __no_sdata graphics_bitmap_t _lc_ub_rotate_bmp;
extern __no_sdata graphics_bitmap_t _lc_ub_down_bmp;
extern __no_sdata graphics_bitmap_t _lc_ub_pause_bmp;

DisplayVGA::DisplayVGA()
{
    int buttonSpace;

    // open graphics device and determine canvas
    _graphics = graphics_open(GRAPHICS_1);
    _canvas = graphics_get_visible_canvas(_graphics);

    // set TFT backlight
    _pwm = pwmx_open(DRV_PWMX_1);
    pwmx_set_resolution_mode(_pwm, PWMX_MODE_8BIT);
    pwmx_set_frequency(_pwm, 10000);
    pwmx_enable_controller(_pwm);
    pwmx_set_pulsewidth(_pwm, BRIGHT_MAX);

    // set Fill() flag
    _first = true;

    // other settings
    _width = canvas_get_width(_canvas);
    _height = canvas_get_height(_canvas);

    _cellSize = 10; // make this configurable?
    _numRows = (_height + _cellSize - 1) / _cellSize;
    _numColumns = (_width + _cellSize - 1) / _cellSize;

    _offsetX = 5; // make this configurable?
    _offsetY = 4;

    _boardRows = _numRows - 2 - (2 * _offsetY);
    _boardColumns = _numColumns - 2 - (2 * _offsetX);

    _boardX0 = _offsetX;
    _boardY0 = _offsetY;

    _boardX1 = _boardX0 + _boardColumns + 2;
    _boardY1 = _boardY0 + _boardRows + 2;

    // button settings
    _buttonSizeX = 16;
    _buttonSizeY = 16;

    _buttonY = _height - _buttonSizeY;
    buttonSpace = (_width - (7 * _buttonSizeX)) / 4;
    _buttonX1 = _buttonSizeX;
    _buttonX2 = _buttonX1 + _buttonSizeX + buttonSpace;
    _buttonX3 = _buttonX2 + _buttonSizeX + buttonSpace;
    _buttonX4 = _buttonX3 + _buttonSizeX + buttonSpace;
    _buttonX5 = _buttonX4 + _buttonSizeX + buttonSpace;
}

void DisplayVGA::Initialize()
{
    ClearScreen(BLACK);
    DisplayButtons();
    DrawString(cell2pixel(_offsetX), cell2pixel(_offsetY - 2), "POINTS :", NULL, FS_BOLD, WHITE, BLACK);
}

void DisplayVGA::ClearScreen(color_t fillColor)
{
    graphics_fill_canvas(_canvas, fillColor);
}

void DisplayVGA::DisplayScoreBoard(int points, int lines, int pieces, int phase)
{
    DrawNumber(cell2pixel(_offsetX + 4) + graphics_get_stringwidth(_canvas, "POINTS :", NULL, FS_BOLD), cell2pixel(_offsetY - 2), points);
}

void DisplayVGA::DisplayBorders(color_t foregroundColor)
{
    // the Tetris area is width * height and the frame starts at (0,0). the inside area
    // for tetris starts at (1,1)
    Fill(0, 0, 1 + _boardColumns, 1, foregroundColor);
    Fill(1 + _boardColumns, 0, 1 + _boardColumns + 1, 1 + _boardRows, foregroundColor);
    Fill(1, 1 + _boardRows, 1 + _boardColumns + 1, 1 + _boardRows + 1, foregroundColor);
    Fill(0, 1, 1, 1 + _boardRows + 1, foregroundColor);
}

void DisplayVGA::DisplayEndGame()
{
    int textWidth;

    textWidth = graphics_get_stringwidth(_canvas, "GAME OVER", NULL, FS_BOLD);
    graphics_fill_rect(_canvas, cell2pixel(_boardX0 + 1), cell2pixel(_boardY0 + 2), _boardColumns * _cellSize, 3 * _cellSize, RED);
    graphics_draw_string(_canvas, (_width - textWidth) / 2, cell2pixel(_boardY0 + 3), "GAME OVER", NULL, WHITE, FS_BOLD);
}

void DisplayVGA::DisplayInfo()
{
    struct timespec ts;
    char            buf[48];

    clock_gettime(CLOCK_REALTIME, & ts);
    sprintf(buf, "%02d:%02d:%02d", (int)(ts.tv_sec / 3600), (int)((ts.tv_sec / 60) % 60), (int)(ts.tv_sec % 60));
    DrawString(cell2pixel(_offsetX), cell2pixel(_offsetY - 4), buf, NULL, FS_BOLD, WHITE, BLACK);
}

void DisplayVGA::Fill(int x0, int y0, int x1, int y1, color_t color)
{
    int xMin = (x0 > x1) ? x1: x0;
    int xMax = (x0 > x1) ? x0: x1;
    int yMin = (y0 > y1) ? y1: y0;
    int yMax = (y0 > y1) ? y0: y1;

    // initialize screen in first Fill()
    if (_first)
    {
        Initialize();
        _first = false;
    }

    graphics_fill_rect(_canvas, cell2pixel(_boardX0 + xMin), cell2pixel(_boardY1 + 1 - yMax), cell2pixel(xMax - xMin), cell2pixel(yMax - yMin), color);
}

void DisplayVGA::DisplayButtons(bool paused)
{
    if (paused)
    {
        int textWidth;

        textWidth = graphics_get_stringwidth(_canvas, "PAUSED", NULL, FS_BOLD);
        graphics_fill_rect(_canvas, 0, _buttonY, _width, _buttonY, BLACK);
        graphics_draw_string(_canvas, (_width - textWidth) / 2, _buttonY, "PAUSED", NULL, RED, FS_BOLD);
    }
    else
    {
        graphics_fill_rect(_canvas, 0, _buttonY, _width, _buttonY, BLACK);
        graphics_draw_bitmap(_canvas, & _lc_ub_left_bmp, _buttonX1, _buttonY, _buttonSizeX, _buttonSizeY, 0);
        graphics_draw_bitmap(_canvas, & _lc_ub_right_bmp, _buttonX2, _buttonY, _buttonSizeX, _buttonSizeY, 0);
        graphics_draw_bitmap(_canvas, & _lc_ub_rotate_bmp, _buttonX3, _buttonY, _buttonSizeX, _buttonSizeY, 0);
        graphics_draw_bitmap(_canvas, & _lc_ub_down_bmp, _buttonX4, _buttonY, _buttonSizeX, _buttonSizeY, 0);
        graphics_draw_bitmap(_canvas, & _lc_ub_pause_bmp, _buttonX5, _buttonY, _buttonSizeX, _buttonSizeY, 0);
    }
}

void DisplayVGA::GetBoardDimensions(int & height, int & width)
{
    height = _boardRows;
    width  = _boardColumns;
}

void DisplayVGA::DrawNumber(int x, int y, int value)
{
    char buf[48];

    sprintf(buf, "%d", value);
    DrawString(x, y, buf, NULL, FS_BOLD, WHITE, BLACK);
}

void DisplayVGA::DrawString(int x, int y, const char * str, const font_t * font, const fontstyle_t fontStyle, color_t foregroundColor, color_t backgroundColor)
{
    graphics_fill_rect(_canvas, x, y, graphics_get_stringwidth(_canvas, str, font, fontStyle), graphics_get_fontheight(_canvas, font), backgroundColor);
    graphics_draw_string(_canvas, x, y, str, font, foregroundColor, fontStyle);
}

inline int DisplayVGA::cell2pixel(int cellPosition)
{
    return(cellPosition * _cellSize);
}
