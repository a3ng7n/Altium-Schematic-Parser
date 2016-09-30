// Tetris game class

#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <mqueue.h>
#include <sys/stat.h>

#include "tetris.h"
#include "display_vga.h"
#include "board.h"

static void _nscs_to_timespec(time_t nsecs, struct timespec * tmspec)
{
    if (nsecs == 0)
    {
        tmspec->tv_sec = 0;
        tmspec->tv_nsec = 0;
    }
    else
    {
        tmspec->tv_sec = (nsecs / 1000000000l);
        tmspec->tv_nsec = (nsecs % 1000000000l);
    }
}

TetrisGame::TetrisGame()
{
    struct mq_attr attribute;
    mode_t         mode;

    // initialize the graphics driver
    _display = new DisplayVGA();
    _display->Initialize();

    // initialize random generator
    srand(time(NULL));

    // all created threads inherit all signals blocked
    sigfillset(&_signalSet);
    pthread_sigmask(SIG_BLOCK, &_signalSet, NULL);

    // open ipc channel: logger thread is receiver, other senders
    attribute.mq_flags = 0;
    attribute.mq_maxmsg = MSG_MAXMSG;
    attribute.mq_msgsize = MSG_MAXSIZE;
    mode = S_IRWXU | S_IRWXG | S_IRWXO;
    _sendQueue = mq_open("mq_log", O_WRONLY | O_CREAT, mode, &attribute);
    _receiveQueue = mq_open("mq_log", O_RDONLY);
}

void TetrisGame::Start()
{
    // start the threads
    _updateInfoThread.Create(this);
    _inputThread.Create(this);
    _loggerThread.Create(this);
    _tetrisThread.Create(this);

    // join created threads in reverse termination order */
    _updateInfoThread.Join(NULL);
    _inputThread.Join(NULL);
    _loggerThread.Join(NULL);
    _tetrisThread.Join(NULL);
}

void TetrisGame::Initialize()
{
    int width, height;

    _board = new TetrisBoard(_display);
    _board->Initialize(BLACK, WHITE);

    // get board dimensions
    _display->GetBoardDimensions(height, width);

    // playing field
    _firstBoardRow = 1;
    _lastBoardRow = height;

    _firstBoardColumn = 1;
    _lastBoardColumn = width;

    // pieces
    for (int i = 0; i < NO_PIECES; i++)
    {
        _pieces[i] = new TetrisPiece((int *)(&shapeMap[i]), colorMap[i]);
    }
    for (int i = 0; i < NO_PIECES; i++)
    {
        _pieces[i]->SetRotatePiece(_pieces[(int)rotateMap[i]]);
    }
}

void TetrisGame::Play()
{
    int               high, col, row;
    int               l;
    TetrisPiece *     piece;
    bool              pieceIsFalling = true;
    int               signal;
    sigset_t          set;
    struct sigevent   ev;
    timer_t           timer;
    time_t            period;
    struct itimerspec ts;
    bool              gravity;
    volatile int      gameInProgress = true;
    bool              paused = false;

    // setup signals
    sigemptyset(&set);
    sigaddset(&set, SIGGRAVITY);
    sigaddset(&set, SIGBUTTON1);
    sigaddset(&set, SIGBUTTON2);
    sigaddset(&set, SIGBUTTON3);
    sigaddset(&set, SIGBUTTON4);
    sigaddset(&set, SIGBUTTON5);

    // create/set a 'one-shot' gravity timer
    ev.sigev_notify = SIGEV_SIGNAL;
    ev.sigev_signo = SIGGRAVITY;
    timer_create(CLOCK_REALTIME, &ev, &timer);
    period = SIGGFIRST_NSCS;
    _nscs_to_timespec(0, &ts.it_interval);
    _nscs_to_timespec(period, &ts.it_value);
    timer_settime(timer, 0, &ts, NULL);

    // set high to top-most board line
    high = _firstBoardRow;

    // as soon as gravity is on .. pieces fall
    while (gameInProgress)
    {
        // start game

        // next lifetime of a fallen piece

        // coordinates of center of figure
        row = _lastBoardRow - 1;
        col = _lastBoardColumn / 2;

        // get random figure
        piece = RandomPiece();
        pieceIsFalling = true;

        // paint new one or again the old one
        piece->Paint(_board, row, col);

        // no restart timer unless indicated
        gravity = false;

        while (pieceIsFalling)
        {
            sigwait(&set, &signal);

            if (paused)
            {
                switch (signal)
                {
                    case SIGBUTTON1: // fallthrough
                    case SIGBUTTON2: // fallthrough
                    case SIGBUTTON3: // fallthrough
                    case SIGBUTTON4: // fallthrough
                    case SIGBUTTON5:
                        timer_create(CLOCK_REALTIME, &ev, &timer);
                        gravity = true;
                        paused = false;
                        _display->DisplayButtons();
                        break;

                    default:
                        break;
                }
            }
            else
            {
                switch (signal)
                {
                    case SIGGRAVITY:
                        if (piece->MovePossible(_board, row, col, MOVE_DOWN))
                        {
                            row -= 1;
                        }
                        else
                        {
                            pieceIsFalling = false;
                            high = (high < row + 1) ? (row + 1) : high;
                        }
                        gravity = true;
                        break;

                    case SIGBUTTON1: // left
                        if (piece->MovePossible(_board, row, col, MOVE_LEFT))
                        {
                            col -= 1;
                        }
                        break;

                    case SIGBUTTON2: // right
                        if (piece->MovePossible(_board, row, col, MOVE_RIGHT))
                        {
                            col += 1;
                        }
                        break;

                    case SIGBUTTON3: // rotate
                        if (piece->MovePossible(_board, row, col, MOVE_ROTATE))
                        {
                            piece = piece->GetRotatePiece();
                        }
                        break;

                    case SIGBUTTON4: // down
                        /* down until we hit upper part of other Tetris */
                        while (piece->MovePossible(_board, row, col, MOVE_DOWN))
                        {
                            row -= 1;
                        }
                        pieceIsFalling = false;
                        high = (high < row + 1) ? (row + 1) : high;
                        break;

                    case SIGBUTTON5: // pause
                        timer_delete(timer);
                        paused = true;
                        _display->DisplayButtons(/*paused=*/true);
                        break;

                    default:
                        break;
                }

                if (high == _lastBoardRow)
                {
                    /* game is over: highest Tetris touches upper
                    * part of the board */
                    high = _firstBoardRow;
                    /* leave while(pieceIsFalling) and prepares new board */
                    #define GAME_OVER       "\n   Game Over    "
                    mq_send(_sendQueue, GAME_OVER, sizeof(GAME_OVER) - 1, MSG_EXIT);
                    gameInProgress = false;
                }
                else
                {
                    /* paint new one or again the old one */
                    piece->Paint(_board, row, col);

                    /* a tetris piece has gone all the way down */
                    if (!pieceIsFalling)
                    {
                        /* end of tetris piece life */
                        WriteScoreBoard(S_PIECE, 1);

                        /* check possible completed lines */
                        for (l = row + 1; l >= _firstBoardRow && l >= row - 1; l--)
                        {
                            if (_board->LineIsFull(l))
                            {
                                // line is completed */
                                WriteScoreBoard(S_LINE, 1);
                                /* erase completed line, update board */
                                _board->ShiftLineDown(l, high);
                                /* paint it */
                                _board->PaintLines(l, high);
                                /* high is one less */
                                high--;
                            }
                        }

                        /* new score */
                        UpdateScoreBoard();
                    }
                }

            }
            if (gravity)
            {
                /* re-start gravity timer */
                _nscs_to_timespec(period - _scoreBoard.phase * SIGGDELTA_NSCS, &ts.it_value);
                timer_settime(timer, 0, &ts, NULL);
                gravity = false;
            }
        } // while (pieceIsFalling)

    } // while 1
}

TetrisPiece * TetrisGame::RandomPiece()
{
    unsigned int var;

    /* get a random number with timer interrupt */
    var = (int)(((double) NO_PIECES) * rand() / (RAND_MAX + 1.0));
    return _pieces[var];
}

void TetrisGame::InitializeScoreBoard()
{
    _scoreBoard.points = 0;
    _scoreBoard.pieces = 0;
    _scoreBoard.lines = 0;
    _scoreBoard.phase = 0;
}

void TetrisGame::WriteScoreBoard(scoreboard_type_t s, int update)
{
    switch (s)
    {
        case S_PIECE:
            _scoreBoard.pieces += update;
            _scoreBoard.points += 2 * update;
            break;

        case S_LINE:
            _scoreBoard.lines += update;
            _scoreBoard.points += 7 * update;
            if (_scoreBoard.lines >= NO_LINES_PHASE * (_scoreBoard.phase + 1))
            {
                _scoreBoard.phase++;
            }
            break;

        default:
            break;
    }
}

void TetrisGame::UpdateScoreBoard()
{
    int po, li, pi, ph;

    po = _scoreBoard.points;
    li = _scoreBoard.lines;
    pi = _scoreBoard.pieces;
    ph = _scoreBoard.phase;

    /* display score data: target */
    _display->DisplayScoreBoard(po, li, pi, ph);

    return;
}
