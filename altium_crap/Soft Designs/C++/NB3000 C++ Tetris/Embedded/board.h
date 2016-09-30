// Tetris board
//

#ifndef __BOARD_CPP_H
#define __BOARD_CPP_H   1

#include <graphics.h>

// forward class declaration
class DisplayVGA;

class TetrisBoard
{
public:
    // constructor
    TetrisBoard(DisplayVGA* display);

    // initialize the board
    void Initialize(color_t backgroundColor, color_t foregroundColor);

    // display a specific cell of the board
    void PaintCell(int row, int column, color_t color);

    // return true when the given cell is free
    bool FreeCell(int row, int column);

    // return true when the given coordinates fall with the board dimensions
    bool OnTheBoard(int row, int column, bool checkBorder = false);

    // display a complete line
    void PaintLine(int row);

    // display multiple lines
    void PaintLines(int rowFrom, int rowTo);

    // return true when the given row is filled
    bool LineIsFull(int row);

    // shift down a complete line
    void ShiftLineDown(int startRow, int topRow);

    // return the foreground and background colors
    color_t GetForegroundColor()
    {
        return _foregroundColor;
    }
    color_t GetBackgroundColor()
    {
        return _backgroundColor;
    }

protected:
    // copy a complete board line to another line
    void CopyLine(int rowFrom, int rowTo);

private:
    int             _width;
    int             _height;
    color_t         _foregroundColor;
    color_t         _backgroundColor;
    color_t *       _board;
    DisplayVGA *    _display;
};

#endif /* !defined(__BOARD_CPP_H) */

