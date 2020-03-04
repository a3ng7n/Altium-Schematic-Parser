// Tetris pieces

#ifndef __PIECES_CPP_H
#define __PIECES_CPP_H  1

#include <graphics.h>

// all possible tetris piece types. these are used as indexes
typedef enum
{
    leftSnakeA  = 0,
    leftSnakeB,
    leftSnakeC,
    leftSnakeD,
    rightSnakeA,
    rightSnakeB,
    rightSnakeC,
    rightSnakeD,
    teeA,
    teeB,
    teeC,
    teeD,
    leftGunA,
    leftGunB,
    rightGunA,
    rightGunB,
    beamA,
    beamB,
    square,
    LAST_PIECE_DO_NOT_DELETE
} tetris_t;

#define NO_PIECES   ((int)LAST_PIECE_DO_NOT_DELETE)
#define SHAPE3x3    9

class TetrisBoard;

class TetrisPiece
{
public:
    TetrisPiece(const int * shape, color_t color);

    // get/set the rotated piece
    TetrisPiece * GetRotatePiece();
    void SetRotatePiece(TetrisPiece * rotatePiece);

    // paint the piece where (y,x) are the center coordinates
    void Paint(TetrisBoard * board, int y0, int x0, bool erasePiece = false);

    // check if the piece can move the requested direction
    bool MovePossible(TetrisBoard * board, int yo, int xo, int move_type);

private:
    TetrisPiece *   _rotatePiece;
    const int *     _shape;
    color_t         _color;
};

// global variables
extern  tetris_t    rotateMap[NO_PIECES];
extern  int         shapeMap[NO_PIECES][SHAPE3x3];
extern  color_t     colorMap[NO_PIECES];

#endif /* !defined(__PIECES_CPP_H) */
