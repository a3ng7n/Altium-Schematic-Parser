// Display class

#ifndef __DISPLAY_CPP_H
#define __DISPLAY_CPP_H         1

#include <graphics.h>
#include <drv_pwmx.h>

class DisplayVGA
{
public:
        // constructor
        DisplayVGA();

        // initialize display
        void Initialize();

        // clear screen
        void ClearScreen(color_t fillColor);

        // display scores
        void DisplayScoreBoard(int points, int lines, int pieces, int phase);

        // display borders for Tetris board
        void DisplayBorders(color_t foregroundColor);

        // display end game string
        void DisplayEndGame();

        // display game info
        void DisplayInfo();

        // fill a square (using real display coordinates)
        void Fill(int x0, int y0, int x1, int y1, color_t color);

        // show the buttons
        void DisplayButtons(bool paused = false);

        // query board dimensions
        void GetBoardDimensions(int& height, int& width);

private:
        // display an integer value at the given display coordinates
        void DrawNumber(int x, int y, int value);

        // display a string at the given display coordinates
        void DrawString(int x, int y, const char* str,
                        const font_t* font, const fontstyle_t fontStyle,
                        color_t foregroundColor, color_t backgroundColor);

        // convert a cell number to a pixel position (for the upper-left corner
        // of the cell)
        inline int cell2pixel(int cellPosition);
        
private:
        graphics_t*     _graphics;
        canvas_t*       _canvas;
        pwmx_t*         _pwm;

        bool            _first;                 // flag to signal first use of Fill()
        
        int             _width;                 // canvas width in pixels
        int             _height;                // canvas height in pixels

        int             _cellSize;              // cell size in pixels (square)
        int             _numRows;               // number of cell rows
        int             _numColumns;            // number of cell columns

        int             _offsetX;               // x offset of the first board cell
        int             _offsetY;               // y offset of the first board cell

        int             _boardRows;             // number of cell rows for the game board
        int             _boardColumns;          // number of cell columns for the game board

        int             _boardX0;               // board coordinates in cell units
        int             _boardY0;
        int             _boardX1;
        int             _boardY1;

        int             _buttonY;               // vertical pixel offset for the buttons
        int             _buttonX1;              // horizontal position of the buttons
        int             _buttonX2;
        int             _buttonX3;
        int             _buttonX4;
        int             _buttonX5;
        int             _buttonSizeX;           // pixel size of a button
        int             _buttonSizeY;
};

#endif /* !defined(__DISPLAY_CPP_H) */

