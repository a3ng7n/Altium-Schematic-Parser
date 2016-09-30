// Tetris pieces

#include "tetris.h"
#include "pieces.h"


// transformation of a piece for a rotate action:
//
//  result = rotateMap[piece]
//
tetris_t   rotateMap[NO_PIECES] =
{
    leftSnakeB,
    leftSnakeC,
    leftSnakeD,
    leftSnakeA,
    rightSnakeB,
    rightSnakeC,
    rightSnakeD,
    rightSnakeA,
    teeB,
    teeC,
    teeD,
    teeA,
    leftGunB,
    leftGunA,
    rightGunB,
    rightGunA,
    beamB,
    beamA,
    square
};

// shapes of all pieces in a 3x3 square
int   shapeMap[NO_PIECES][SHAPE3x3] =
{
    {
        // left snake A
        0, 0, 1,
        0, 1, 1,
        0, 1, 1,
    },
    {
        // left snake B
        0, 0, 0,
        1, 1, 0,
        1, 1, 1,
    },
    {
        // left snake C
        1, 0, 1,
        1, 0, 1,
        0, 0, 1,
    },
    {
        // left snake D
        0, 1, 1,
        0, 0, 0,
        1, 1, 1,
    },
    {
        // right snake A
        0, 1, 1,
        0, 1, 1,
        0, 0, 1,
    },
    {
        // right snake B
        0, 0, 0,
        0, 1, 1,
        1, 1, 1,
    },
    {
        // right snake C
        0, 0, 1,
        1, 0, 1,
        1, 0, 1,
    },
    {
        // right snake D
        1, 1, 0,
        0, 0, 0,
        1, 1, 1,
    },
    {
        // Tee A
        0, 1, 1,
        0, 0, 1,
        0, 1, 1,
    },
    {
        // Tee B
        0, 0, 0,
        1, 0, 1,
        1, 1, 1,
    },
    {
        // Tee C
        1, 0, 1,
        0, 0, 1,
        1, 0, 1,
    },
    {
        // Tee D
        1, 0, 1,
        0, 0, 0,
        1, 1, 1,
    },
    {
        // left Gun A
        1, 0, 1,
        0, 0, 1,
        0, 1, 1,
    },
    {
        // left Gun B*/
        0, 0, 1,
        1, 0, 0,
        1, 1, 1,
    },
    {
        // right Gun A*/
        0, 1, 1,
        0, 0, 1,
        1, 0, 1,
    },
    {
        // right Gun B*/
        1, 0, 0,
        0, 0, 1,
        1, 1, 1,
    },
    {
        // beam A
        1, 0, 1,
        1, 0, 1,
        1, 0, 1,
    },
    {
        // beam B*/
        1, 1, 1,
        0, 0, 0,
        1, 1, 1,
    },
    {
        // square
        0, 0, 1,
        0, 0, 1,
        1, 1, 1,
    }
};

// color map for each piece
color_t colorMap[NO_PIECES] =
{
    WHITE,
    WHITE,
    WHITE,
    WHITE,
    RED,
    RED,
    RED,
    RED,
    BLUE,
    BLUE,
    BLUE,
    BLUE,
    LIGHTGREEN,
    LIGHTGREEN,
    YELLOW,
    YELLOW,
    CYAN,
    CYAN,
    MAGENTA
};


TetrisPiece::TetrisPiece(const int * shape, color_t color)
    : _shape(shape), _color(color)
{
    _rotatePiece = (TetrisPiece *) 0;
}

TetrisPiece * TetrisPiece::GetRotatePiece()
{
    // when no rotate piece is set, return a pointer to this piece
    return (_rotatePiece ? _rotatePiece : this);
}

void TetrisPiece::SetRotatePiece(TetrisPiece * rotatePiece)
{
    _rotatePiece = rotatePiece;
}

void TetrisPiece::Paint(TetrisBoard * board, int y0, int x0, bool erasePiece)
{
    int         x, y;
    const int * ptr = _shape;
    color_t     color = (erasePiece ? board->GetBackgroundColor() : _color);

    for (y = y0 + 1; y >= y0 - 1; y--)
    {
        for (x = x0 - 1; x <= x0 + 1; x++)
        {
            if (*ptr++ == 0)
            {
                board->PaintCell(y, x, color);
            }
        }
    }
}

bool TetrisPiece::MovePossible(TetrisBoard * board, int yo, int xo, int move_type)
{
    int         xc = xo;
    int         yc = yo;
    int         x;
    int         y;
    const int * ptr = _shape;

    // erase piece from the board
    Paint(board, yo, xo, /*erasePiece=*/true);

    switch (move_type)
    {
        case MOVE_ROTATE:
            // get result from the rotated piece if set
            if (_rotatePiece)
            {
                return _rotatePiece->MovePossible(board, yo, xo, MOVE_NONE);
            }
            // no rotated piece so continue with the current data
            break;
        case MOVE_DOWN:
            // one down
            yc = yo - 1;
            break;
        case MOVE_RIGHT:
            // one right
            xc = xo + 1;
            break;
        case MOVE_LEFT:
            // one left
            xc = xo - 1;
            break;
        default:
            break;
    }

    // do I fit in my new position ?
    for (y = yc + 1; y >= yc - 1; y--)
    {
        for (x = xc - 1; x <= xc + 1; x++)
        {
            if (*ptr++ == 0 && board->OnTheBoard(y, x, /*checkBorder=*/true) && !board->FreeCell(y, x))
            {
                return false;
            }
        }
    }

    return true;
}


