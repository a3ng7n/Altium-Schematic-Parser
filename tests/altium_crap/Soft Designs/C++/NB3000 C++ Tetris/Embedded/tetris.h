// Tetris common include file

#ifndef __TETRIS_CPP_H
#define __TETRIS_CPP_H  1

#include <stdbool.h>
#include <graphics.h>
#include <drv_pwmx.h>

#include "tetris_conf.h"

#include "display_vga.h"
#include "board.h"
#include "pieces.h"

#include "input.h"
#include "logger.h"
#include "model.h"
#include "update_info.h"

// real time signals: for tetris thread
#define SIGGRAVITY                      SIGRTMIN
#define SIGBUTTON1                      (SIGGRAVITY+1)
#define SIGBUTTON2                      (SIGBUTTON1+1)
#define SIGBUTTON3                      (SIGBUTTON2+1)
#define SIGBUTTON4                      (SIGBUTTON3+1)
#define SIGBUTTON5                      (SIGBUTTON4+1)
#define SIGUPDATE                       (SIGBUTTON5+1)

// possible piece movement
#define MOVE_NONE                       0
#define MOVE_LEFT                       1
#define MOVE_RIGHT                      2
#define MOVE_DOWN                       3
#define MOVE_ROTATE                     4

// message priorities
#define MSG_EXIT                        2
#define MSG_DISPLAY                     1

#define MSG_MAXSIZE                     32
#define MSG_MAXMSG                      4

// enumeration of score-board types
typedef enum
{
    S_POINT   = 0,
    S_LINE    = 1,
    S_PIECE   = 2,
    S_PHASE   = 3
} scoreboard_type_t;

// scores structure
struct score_t
{
    int  points;
    int  pieces;
    int  lines;
    int  phase;
};

class TetrisGame
{
public:
    // constructor
    TetrisGame();

    // start the game
    void Start();

    // initialize the game
    void Initialize();

    // play the game
    void Play();

    // query functions for the threads
    InputThread& GetInputThread()           { return _inputThread; }
    LoggerThread& GetLoggerThread()         { return _loggerThread; }
    TetrisThread& GetTetrisThread()         { return _tetrisThread; }
    UpdateInfoThread& GetUpdateInfoThread() { return _updateInfoThread; }

    // query functions for the message queues
    mqd_t& GetSendQueue()                   { return _sendQueue; }
    mqd_t& GetReceiveQueue()                { return _receiveQueue; }

    // get the display and board
    DisplayVGA* GetDisplay()        { return _display; }
    TetrisBoard* GetBoard()         { return _board; }

private:
    // return a new Tetris piece
    TetrisPiece * RandomPiece();

    // initialize the scores
    void InitializeScoreBoard();

    // display the scores
    void WriteScoreBoard(scoreboard_type_t s, int update);

    // update the scores
    void UpdateScoreBoard();

private:
    // display and board
    DisplayVGA*         _display;
    TetrisBoard*        _board;

    int                 _firstBoardRow;
    int                 _lastBoardRow;

    int                 _firstBoardColumn;
    int                 _lastBoardColumn;

    // threads
    InputThread         _inputThread;
    LoggerThread        _loggerThread;
    TetrisThread        _tetrisThread;
    UpdateInfoThread    _updateInfoThread;

    // message queues
    mqd_t               _sendQueue;
    mqd_t               _receiveQueue;

    // signal sets
    sigset_t            _signalSet;

    // scores
    score_t             _scoreBoard;

    // pieces
    TetrisPiece*        _pieces[NO_PIECES];
};


#endif /* !defined(__TETRIS_CPP_H) */
