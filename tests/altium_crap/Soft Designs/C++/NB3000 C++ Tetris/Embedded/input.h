// Input thread
//

#ifndef __INPUT_CPP_H
#define __INPUT_CPP_H  1

#include "threads.h"
#include <drv_ioport.h>

#define BUTTON_COUNT 5
#define DEBOUNCE 100

class TetrisGame;

class Buttons
{
public:
    typedef enum {
        BTN_NONE    = 0,
        BTN_LEFT,
        BTN_RIGHT,
        BTN_ROTATE,
        BTN_DROP,
        BTN_PAUSE
    } button_kind_t;

public:
    Buttons(const int id);

    // handle a button event
    button_kind_t GetValue();

private:
    ioport_t  * _port;
    bool        _switchIsUp[BUTTON_COUNT];
    int         _switchUpCount[BUTTON_COUNT];
};

class InputThread : public ThreadBase
{
public:
    InputThread();

protected:
    virtual void Setup();
    virtual void* Execute(void* arg);

    virtual const pthread_attr_t* Attr() const
        {
            return &_attr;
        }

private:
    bool StrokeAction(TetrisGame* theGame, Buttons::button_kind_t kind);

    pthread_attr_t  _attr;
    char            _stack[12 * PTHREAD_STACK_MIN];
};

#endif /* !defined(__INPUT_CPP_H) */

