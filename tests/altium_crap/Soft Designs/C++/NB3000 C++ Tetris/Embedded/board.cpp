// Tetris board

#include <stdio.h>
#include <stdlib.h>

#include "board.h"
#include "display_vga.h"

// macro to access the tetris game board
#define BOARD(r,c) *(_board + ((r) * (_width + 2)) + (c))

TetrisBoard::TetrisBoard(DisplayVGA * display) : _display(display)
{
    _display->GetBoardDimensions(_height, _width);
    _board = (color_t *) malloc((_height + 2) * (_width + 2) * sizeof(color_t));
}

void TetrisBoard::Initialize(color_t backgroundColor, color_t foregroundColor)
{
    int i, j;

    _foregroundColor = foregroundColor;
    _backgroundColor = backgroundColor;

    _display->ClearScreen(backgroundColor);
    _display->DisplayBorders(foregroundColor);

    // initialize the whole board to free
    for (i = 0; i <= _height + 1; i++)
    {
        for (j = 0; j <= _width + 1; j++)
        {
            BOARD(i, j) = _backgroundColor;
        }
    }

    // initialize the frame to non-free
    for (i = 0; i <= _height + 1; i++)
    {
        BOARD(i, 0) = foregroundColor;
        BOARD(i, _width + 1) = foregroundColor;
    }
    for (i = 0; i <= _width + 1; i++)
    {
        BOARD(0, i) = foregroundColor;
        BOARD(_height + 1, i) = foregroundColor;
    }
}

void TetrisBoard::PaintCell(int row, int column, color_t color)
{
    if (row >= 1 && row <= _height && column >= 1 && column <= _width)
    {
        BOARD(row, column) = color;
        _display->Fill(column, row, column + 1, row + 1, color);
    }
}

bool TetrisBoard::FreeCell(int row, int column)
{
    return(BOARD(row, column) == _backgroundColor);
}

bool TetrisBoard::OnTheBoard(int row, int column, bool checkBorder)
{
    int     offset = (checkBorder ? 1 : 0);
    bool    result = false;

    if ((row >= (1 - offset)) && (row <= (_height + offset)) &&
        (column >= (1 - offset)) && (column <= (_width + offset)))
    {
        result = true;
    }

    return result;
}

void TetrisBoard::PaintLine(int row)
{
    int column;

    if (row >= 1 && row <= _height)
    {
        for (column = 1; column <= _width; column++)
        {
            PaintCell(row, column, BOARD(row, column));
        }
    }
}

void TetrisBoard::PaintLines(int rowFrom, int rowTo)
{
    int row;

    for (row = rowFrom; row <= rowTo; row++)
    {
        PaintLine(row);
    }
}

bool TetrisBoard::LineIsFull(int row)
{
    int column;

    for (column = 1; column <= _width; column++)
    {
        if (FreeCell(row, column))
        {
            return false;
        }
    }
    return true;
}

// shift down a complete line
void TetrisBoard::ShiftLineDown(int startRow, int topRow)
{
    int row;

    for (row = startRow; row <= topRow; row++)
    {
        CopyLine(row + 1, row);
    }
}

// copy a complete board line to another line
void TetrisBoard::CopyLine(int rowFrom, int rowTo)
{
    int column;

    for (column = 1; column <= _width; column++)
    {
        BOARD(rowTo, column) = BOARD(rowFrom, column);
    }
}

