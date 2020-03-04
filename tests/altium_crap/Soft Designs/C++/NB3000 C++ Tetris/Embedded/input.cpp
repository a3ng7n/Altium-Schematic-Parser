// Logger thread

#include <stdio.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <mqueue.h>

#include "devices.h"
#include "drv_ioport.h"

#include "tetris.h"

Buttons::Buttons(const int id)
{
    _port = ioport_open(BUTTONS);

    for (int i = 0; i < BUTTON_COUNT; ++i)
    {
        _switchIsUp[i] = true;
        _switchUpCount[i] = 0;
    }
}

Buttons::button_kind_t Buttons::GetValue()
{
    char            buttons;
    int             i;
    char            button_value;
    char            result = 0;

    buttons = ioport_get_value(_port, 0);
    buttons = ~buttons & 0x1F;

    for (i = 0; i < BUTTON_COUNT; i++)
    {
        // for each button, it registers an event when it first goes down,
        // as long as it has been up DEBOUNCE times
        button_value = (buttons >> i) & 0x01;
        if (!button_value)
        {
            // button is up
            _switchUpCount[i]++;
            if (_switchUpCount[i] >= DEBOUNCE)
            {
                _switchIsUp[i] = true;
            }
        }
        else
        {
            // button is down
            _switchUpCount[i] = 0;
            if (_switchIsUp[i])
            {
                result = result | (1 << i);
                _switchIsUp[i] = false;
            }
        }
    }

    switch (result)
    {
        case 0x01:  return BTN_LEFT;
        case 0x02:  return BTN_RIGHT;
        case 0x04:  return BTN_ROTATE;
        case 0x08:  return BTN_DROP;
        case 0x10:  return BTN_PAUSE;
    }

    return BTN_NONE;
}

InputThread::InputThread() : ThreadBase()
{
}

void InputThread::Setup()
{
    struct sched_param schedparam;

    // base setup function
    ThreadBase::Setup();

    // initialize pthread attributes
    pthread_attr_init(& _attr);
    pthread_attr_setinheritsched(& _attr, PTHREAD_EXPLICIT_SCHED);

    // initialize scheduling priority
    schedparam.sched_priority = INPUT_THREAD_PRIORITY;
    pthread_attr_setschedparam(& _attr, & schedparam);

    // initialize thread stack
    pthread_attr_setstackaddr(& _attr, (void *) & _stack[0]);
    pthread_attr_setstacksize(& _attr, sizeof(_stack));
}

void * InputThread::Execute(void * arg)
{
    Buttons                 buttons(BUTTONS);
    volatile int            stop = 0;
    TetrisGame            * theGame;
    mqd_t                   mq;
    Buttons::button_kind_t  kind;

    theGame = (TetrisGame *) arg;
    mq = theGame->GetSendQueue();

    while (!stop)
    {
        kind = buttons.GetValue();

        // stroke actions
        if (StrokeAction(theGame, kind) == false)
        {
            // send exit msg to logger
            #define ENDED_BY_USER   "\n  ended by user"
            mq_send(mq, ENDED_BY_USER, sizeof(ENDED_BY_USER) - 1, MSG_EXIT);
        }
    }

    return NULL;
}

bool InputThread::StrokeAction(TetrisGame * theGame, Buttons::button_kind_t kind)
{
    switch (kind)
    {
        case Buttons::BTN_LEFT:
            theGame->GetTetrisThread().Kill(SIGBUTTON1);
            break;

        case Buttons::BTN_RIGHT:
            theGame->GetTetrisThread().Kill(SIGBUTTON2);
            break;

        case Buttons::BTN_ROTATE:
            theGame->GetTetrisThread().Kill(SIGBUTTON3);
            break;

        case Buttons::BTN_DROP:
            theGame->GetTetrisThread().Kill(SIGBUTTON4);
            break;

        case Buttons::BTN_PAUSE:
            theGame->GetTetrisThread().Kill(SIGBUTTON5);
            break;

        default:
            break;
    }

    return true;
}

